#define DEBUG_ACTIVE 0
#include "dm.h"

#include <limits.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/statvfs.h>
#include <sys/time.h>
#include <dirent.h>
#include <stat-time.h>

#include "dm2.h"
#include "dm3.h"
#include "dm-proc.h"
#include "dm-prop.h"
#include "dm-dvt-vm.h"
#include "dm-signals.h"
#include "dm6.h"
#include "dm8.h"
#include "dm-sem.h"
#include "error-local.h"

DM_INLINE_STATIC P closefd(B* stream) {
  P retc = OK;
  int fd  = STREAM_FD(stream);
  int fdl = STREAM_FD_LOCK(stream);
  
  if (fd != -1
      && (retc = delsocket_proc(fd))) {
    if (fdl >= 0 && fd != fdl)
      delsocket_force(fdl);
  }
  else if (fdl >= 0 && fd != fdl)
    retc = delsocket_force(fdl);

  STREAM_FD(stream) = -1;
  STREAM_FD_LOCK(stream) = -1;
  return retc;
}

#define rclosefd(stream) do {			\
    P retc = closefd((stream));			\
    if (retc) return retc;			\
  } while (0)

DM_INLINE_STATIC P pathcat_(B* op, B* def, B** next) {
  B* curr;
  B* dir;
  ULBIG dirlen;

  if (FLOORopds > op) return OPDS_UNF;
  switch (TAG(op)) {
    case NULLOBJ:
      if (! def) return OPD_CLA;
      dir = def;
      dirlen = strlen(dir);
      break;

    case ARRAY|BYTETYPE:
      dir = VALUE_PTR(op);
      dirlen = ARRAY_SIZE(op);
      break;

    default:
      return OPD_CLA;
  }

  if (TAG(op+FRAMEBYTES) != (ARRAY|BYTETYPE)) return OPD_CLA;

  if (FREEvm + dirlen + 1 + ARRAY_SIZE(op+FRAMEBYTES) + 1
      >= CEILvm)
    return VM_OVF;

  moveB(dir, FREEvm, dirlen);
  curr = FREEvm+dirlen;
  if (dirlen && curr[-1] != '/') curr++[0] = '/';
  moveB(VALUE_PTR(op+FRAMEBYTES), curr, ARRAY_SIZE(op+FRAMEBYTES));
  curr += ARRAY_SIZE(op+FRAMEBYTES);
  curr[0] = '\0';

  if (next) *next = curr;
  return OK;
}

DM_INLINE_STATIC P tmpcat(B* op) {
  P retc;
  B* curr;

  static char *tmp = NULL;
  if (! tmp) {
    tmp = getenv("TMPDIR");
    if (tmp && *tmp) tmp = strdup(tmp);
    else tmp = "/tmp";
    if (! tmp) dm_error(errno, "strdup(tmp)");
  }

  if ((retc = pathcat_(op, tmp, &curr))) return retc;
  if (curr + 6 + 1 > CEILvm) return VM_OVF;
  moveB((B*) "XXXXXX", curr, 7);
  curr += 6 + 1;

  return OK;
}

DM_INLINE_STATIC P pathscat_(B* op1, B* def, B* op2, B** mid) {
  B* ofreevm = FREEvm;
  P retc;

  if ((retc = pathcat_(op1, def, mid))) return retc;
  FREEvm = *mid;
  retc = pathcat_(op2, def, NULL);
  FREEvm = ofreevm;
  return retc;
}

#define pathcat(a) pathcat_((a), ".", NULL)
#define pathscat(a, mid) pathscat_((a), ".", ((a)+2*FRAMEBYTES), (mid))
#define rpathcat(a) do {			\
    P retc = pathcat((a));			\
    if (retc) return retc;			\
  } while (0)
#define rpathscat(a, mid) do {			\
    P retc = pathscat((a), &(mid));		\
    if (retc) return retc;			\
  } while (0)
#define rtmpcat(a) do {				\
    P retc = tmpcat((a));			\
    if (retc) return retc;			\
  } while (0)

enum {
  FILE_IRUSR = 00400,
  FILE_IWUSR = 00200,
  FILE_IXUSR = 00100,
  FILE_IRGRP = 00040,
  FILE_IWGRP = 00020,
  FILE_IXGRP = 00010,
  FILE_IROTH = 00004,
  FILE_IWOTH = 00002,
  FILE_IXOTH = 00001,
  FILE_ISUID = 04000,
  FILE_ISGID = 02000,
  FILE_ISVTX = 01000,
  FILE_ALL = 
  (
   FILE_IRUSR |
   FILE_IWUSR |
   FILE_IXUSR |
   FILE_IRGRP |
   FILE_IWGRP |
   FILE_IXGRP |
   FILE_IROTH |
   FILE_IWOTH |
   FILE_IXOTH |
   FILE_ISUID |
   FILE_ISGID |
   FILE_ISVTX
  )
};

static struct {
  ULBIG d;
  ULBIG un;
} perms_map[] = {
  {FILE_IRUSR, S_IRUSR},
  {FILE_IWUSR, S_IWUSR},
  {FILE_IXUSR, S_IXUSR},
  {FILE_IRGRP, S_IRGRP},
  {FILE_IWGRP, S_IWGRP},
  {FILE_IXGRP, S_IXGRP},
  {FILE_IROTH, S_IROTH},
  {FILE_IWOTH, S_IWOTH},
  {FILE_IXOTH, S_IXOTH},
  {FILE_ISUID, S_ISUID},
  {FILE_ISGID, S_ISGID},
  {FILE_ISVTX, S_ISVTX}
};

DM_INLINE_STATIC ULBIG filemode_d(mode_t mode) {
  ULBIG perms = 0;
  P i;

  for (i = 0; i < sizeof(perms_map)/sizeof(perms_map[0]); i++)
    if (mode & perms_map[i].un) perms |= perms_map[i].d;

  return perms;
}

DM_INLINE_STATIC mode_t filemode_un(ULBIG perms) {
  mode_t mode = 0;
  P i;

  for (i = 0; i < sizeof(perms_map)/sizeof(perms_map[0]); i++)
    if (perms & perms_map[i].d) mode |= perms_map[i].un;
  
  return mode;
}

enum {
  FILE_UNKNOWN = 0,
  FILE_IFBLK,
  FILE_IFCHR,
  FILE_IFREG,
  FILE_IFDIR,
  FILE_IFLNK,
  FILE_IFSOCK
};

static struct {
  ULBIG d;
  ULBIG un;
} type_map[] = {
  // unknown is unmarked, {FILE_UNKNOWN, *}
  {FILE_IFBLK,  S_IFBLK},
  {FILE_IFCHR,  S_IFCHR},
  {FILE_IFREG,  S_IFREG},
  {FILE_IFDIR,  S_IFDIR},
  {FILE_IFLNK,  S_IFLNK},
  {FILE_IFSOCK, S_IFSOCK}
};

DM_INLINE_STATIC LBIG filetype_d(mode_t mode) {
  P i;
  
  for (i = 0; i < sizeof(type_map)/sizeof(type_map[0]); i++)
    if (((ULBIG) (mode & S_IFMT)) == type_map[i].un)
      return type_map[i].d;

  return 0;
}

#define D_P_OP(off) (FREEopds+(off)*FRAMEBYTES)

#define D_P_SET(off, tag, attr, key, val) do {			\
    const B* fr = D_P_OP(off);					\
    TAG(fr)     = (tag);					\
    ATTR(fr)    = (attr);					\
    key(fr)     = (val);					\
  } while (0)

#define D_P_SET_LBIG(off, val)					\
  D_P_SET((off), (NUM|LONGBIGTYPE), 0, LONGBIG_VAL, (LBIG) (val))

#define D_P_SET_BOOL(off, val)				\
  D_P_SET(off, BOOL, 0, BOOL_VAL, ((val) ? TRUE : FALSE))

// (dir) | (file)...
P op_readdir(void) {
  B* curr;
  DIR* dir;
  struct dirent* file;
  B* ops;
  P retc;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_CLA;

  if (FREEvm + ARRAY_SIZE(o_1) + 1 >= CEILvm)
    return VM_OVF;
  moveB(VALUE_PTR(o_1), FREEvm, ARRAY_SIZE(o_1));
  FREEvm[ARRAY_SIZE(o_1)] = '\0';

  if (! (dir = opendir(FREEvm)))
    return -errno;

  errno = 0;
  ops = o_1;
  curr = FREEvm;
  while ((file = readdir(dir))) {
    size_t l = strlen(file->d_name);
    switch (l) {
      case 1:
	if (file->d_name[0] == '.') continue;
	break;
	
      case 2:
	if (file->d_name[0] == '.' && file->d_name[1] == '.')
	  continue;
	break;

      default: break;
    };

    if (curr + FRAMEBYTES + DALIGN(l) >= CEILvm) {
      retc = VM_OVF;
      goto err;
    };
    if (ops + FRAMEBYTES > CEILopds) {
      retc = OPDS_OVF;
      goto err;
    };

    TAG(curr) = (ARRAY|BYTETYPE);
    ATTR(curr) = PARENT;
    VALUE_PTR(curr) = curr+FRAMEBYTES;
    moveB(file->d_name, curr+FRAMEBYTES, l);
    ARRAY_SIZE(curr) = l;
    moveframe(curr, ops);

    curr += FRAMEBYTES + DALIGN(l);
    ops += FRAMEBYTES;
  };
  if (errno) {
    retc = -errno;
    goto err;
  };

  while (closedir(dir)) {
    if (errno != EINTR) return -errno;
    checkabort();
  };

  FREEvm = curr;
  FREEopds = ops;
  return OK;

 err:
  while (closedir(dir)) {
    if (errno != EINTR) break;
    checkabort();
  }
  return retc;
}

/* (file)/fd | --
   block_size
   blocks
   blocks_free
   blocks_avail
   files
   files_free
   files_avail
   fsid
   rdonly
   nosuid
   name_max
*/

DM_INLINE_STATIC int statvfs_int(struct statvfs *restrict s) {
  return statvfs((B*) FREEvm, s);
}

static int fstatvfs_int_fd;
DM_INLINE_STATIC int fstatvfs_int(struct statvfs *restrict s) {
  return fstatvfs(fstatvfs_int_fd, s);
}

P op_statvfs(void) {
  struct statvfs s;
  B* stream;
  int (*statfunc)(struct statvfs *restrict s);

  if (FLOORopds > o_1) return OPDS_UNF;
  if (CEILopds < FREEopds + 10*FRAMEBYTES) return OPDS_OVF;

  switch (TAG(o_1)) {
    case ARRAY|BYTETYPE:
      if (FREEvm + ARRAY_SIZE(o_1) + 1 >= CEILvm)
	return VM_OVF;
      moveB(VALUE_PTR(o_1), FREEvm, ARRAY_SIZE(o_1));
      FREEvm[ARRAY_SIZE(o_1)] = '\0';
      statfunc = statvfs_int;
      break;

    case STREAM:
      stream = VALUE_PTR(o_1);
      if ((fstatvfs_int_fd = (int) STREAM_FD(stream)) == -1)
	return STREAM_CLOSED;
      statfunc = fstatvfs_int;
      break;

    default:
      return OPD_CLA;
  }

  while (statfunc(&s)) {
    if (errno != EINTR) return -errno;
    checkabort();
  };

  D_P_SET_LBIG(-1, s.f_frsize);
  D_P_SET_LBIG(0,  s.f_blocks);
  D_P_SET_LBIG(1,  s.f_bfree);
  D_P_SET_LBIG(2,  s.f_bavail);
  D_P_SET_LBIG(3,  s.f_files);
  D_P_SET_LBIG(4,  s.f_ffree);
  D_P_SET_LBIG(5,  s.f_favail);
  D_P_SET_LBIG(6,  s.f_fsid);
  D_P_SET_BOOL(7,  s.f_flag == ST_RDONLY);
  D_P_SET_BOOL(8,  s.f_flag == ST_NOSUID);
  D_P_SET_LBIG(9,  s.f_namemax);

  FREEopds += 10*FRAMEBYTES;
  return OK;
}

/* (dir)/null (file) / fd | false /
     dev
     ino
     nlink
     uid
     gid
     rdev
     size
     blksize
     blocks
     access-time-s
     access-time-ns
     mod-time-s
     mod-time-ns
     c-time-s
     c-time-ns
     mode-as-enum
     type-as-enum
     true
*/
DM_INLINE_STATIC int stat_int(struct stat *restrict s) {
  return stat((B*) FREEvm, s);
}

static int fstat_int_fd;
DM_INLINE_STATIC int fstat_int(struct stat *restrict s) {
  return fstat(fstat_int_fd, s);
}

P op_stat(void) {
  struct stat s;
  B* stream;
  int (*statfunc)(struct stat *restrict s);
  P off;

  if (FLOORopds > o_1) return OPDS_UNF;
  switch (TAG(o_1)) {
    case ARRAY|BYTETYPE:
      off = -2;
      rpathcat(o_2);
      statfunc = stat_int;
      break;

    case STREAM:
      off = -1;
      stream = VALUE_PTR(o_1);
      if ((fstat_int_fd = (int) STREAM_FD(stream)) == -1)
	return STREAM_CLOSED;

      statfunc = fstat_int;
      break;

    default:
      return OPD_CLA;
  }
  if (CEILopds < FREEopds + (off+18)*FRAMEBYTES)
    return OPDS_OVF;

  if (statfunc(&s)) {
    if (errno != ENOENT) return -errno;
    D_P_SET_BOOL(off+0, FALSE);
    FREEopds += (off+1)*FRAMEBYTES;
    return OK;
  }

  D_P_SET_LBIG(off+0,  s.st_dev);
  D_P_SET_LBIG(off+1,  s.st_ino);
  D_P_SET_LBIG(off+2,  s.st_nlink);
  D_P_SET_LBIG(off+3,  s.st_uid);
  D_P_SET_LBIG(off+4,  s.st_gid);
  D_P_SET_LBIG(off+5,  s.st_rdev);
  D_P_SET_LBIG(off+6,  s.st_size);
  D_P_SET_LBIG(off+7,  s.st_blksize);
  D_P_SET_LBIG(off+8,  s.st_blocks);

  D_P_SET_LBIG(off+9,  s.st_atime);
  D_P_SET_LBIG(off+10, get_stat_atime_ns(&s));
  D_P_SET_LBIG(off+11, s.st_mtime);
  D_P_SET_LBIG(off+12, get_stat_mtime_ns(&s));
  D_P_SET_LBIG(off+13, s.st_ctime);
  D_P_SET_LBIG(off+14, get_stat_ctime_ns(&s));

  D_P_SET_LBIG(off+15,  filemode_d(s.st_mode));
  D_P_SET_LBIG(off+16,  filetype_d(s.st_mode));

  D_P_SET_BOOL(off+17, TRUE);

  FREEopds += (off+18)*FRAMEBYTES;
  return OK;
}

// (pathname) mode | --
P op_makedir(void) {
  ULBIG mode;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_2) != (ARRAY|BYTETYPE)) return OPD_CLA;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (TYPE(o_1) >= SINGLETYPE) return OPD_TYP;
  if (! VALUE(o_1, &mode)) return UNDF_VAL;

  if (FREEvm + ARRAY_SIZE(o_2) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_2), FREEvm, ARRAY_SIZE(o_2));
  FREEvm[ARRAY_SIZE(o_2)] = '\0';

  if (mkdir((char*) FREEvm, filemode_un(mode)))
    return -errno;

  FREEopds = o_2;
  return OK;
}

// Stein's algorithm or binary gcd algorithm
// http://www.nist.gov/dads/HTML/binaryGCD.html
DM_INLINE_STATIC ULBIG gcd(ULBIG u, ULBIG v) {
  ULBIG g;

  // divide by two until we get an odd
  // u,v even: gcd(u,v) = 2*gcd(u/2, v/2)
  for (g = 0; ! ((u|v) & 1); ++g) {
    u >>= 1;
    v >>= 1;
  }

  // gcd(0, v) = v
  while (u)
    // u even, v odd: gcd(u,v) = gcd(u/2, v)
    if (! (u&1)) u >>= 1;
    else if (! (v&1)) v >>= 1;
    // u,v odd: gcd(u,v) = gcd(|u-v|/2, v)
    else {
      ULBIG t = (u-v) >> 1;
      if (u < v) v = -t;
      else u = t;
    }
  
  return v << g;
}


//  fd1 fd2 | fd2
P op_cp(void) {
  B* streamin;
  B* streamout;
  int fdin, fdout;
  struct statvfs sfin, sfout;
  ULBIG bs;
  ssize_t n, nw, nw2;
  static long pagesize = -1;

  if (pagesize == -1) {
    errno = 0;
    if ((pagesize = sysconf(_SC_PAGESIZE)) == -1) {
      if (errno) return -errno;
      pagesize = 1;
    }
  }

  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != STREAM
      || TAG(o_2) != STREAM)
    return OPD_CLA;
  streamin  = VALUE_PTR(o_2);
  streamout = VALUE_PTR(o_1);

  if ((fdout = STREAM_FD(streamout)) == -1)
    return STREAM_CLOSED;
  
  if (STREAM_RO(streamout)
      || ! STREAM_RO(streamin))
    return STREAM_DIR;

  if (STREAM_BUFFERED(streamin)) {
    STREAM_BUFFERED(streamin) = FALSE;
    while ((n = write(fdout, &STREAM_CHAR(streamin), 1)) < 1) {
      if (n && errno != EINTR) return -errno;
      checkabort();
    }
    if (STREAM_FD(streamin) == -1) {
      moveframe(o_1, o_2);
      FREEopds = o_1;
      return OK;
    }
  }

  if ((fdin = STREAM_FD(streamin)) == -1)
    return STREAM_CLOSED;

  while (fstatvfs(fdin, &sfin)) {
    if (errno != EINTR) return -errno;
    checkabort();
  };

  while (fstatvfs(fdout, &sfout)) {
    if (errno != EINTR) return -errno;
    checkabort();
  };

  bs = sfin.f_frsize*(sfout.f_frsize/gcd(sfin.f_frsize, sfout.f_frsize));
  bs = bs*(pagesize/gcd(bs, pagesize));
  if (FREEvm + bs >= CEILvm) return VM_OVF;

  while ((n = read(fdin, FREEvm, (size_t) bs))) {
    if (n == -1) switch (errno) {
	case EAGAIN: case EINTR: checkabort(); continue;
	default: return -errno;
      }
    STREAM_CHAR(streamin) = FREEvm[n-1];

    nw = 0;
    while (nw < n) {
      nw2 = write(fdout, FREEvm + nw, (size_t) (n - nw));
      if (nw2 == -1) switch (errno) {
	  case EAGAIN: case EINTR: checkabort(); continue;
	  default: return -errno;
	}
      nw += nw2;
    }
  }
  
  rclosefd(streamin);
  moveframe(o_1, o_2);
  FREEopds = o_1;
  return OK;
}

// (dir)/null (file) (dir')/null (file') | true / false
P op_rename(void) {
  B* src;
  B* dest;
  BOOLEAN s = TRUE;

  rpathscat(o_4, dest);
  src = FREEvm;
  if (rename(src, dest)) {
    if (errno != EXDEV) return -errno;
    s = FALSE;
  }
  
  TAG(o_4) = BOOL;
  ATTR(o_4) = 0;
  BOOL_VAL(o_4) = s;

  FREEopds = o_3;
  return OK;
}

// (dir)/null (file) | (dir') (file')
P op_realpath(void) {
  B dest[PATH_MAX+1];
  B* file;
  B* efile;
  B* edest;

  rpathcat(o_2);
  if (! realpath((char*) FREEvm, (char*) dest)) {
    size_t len;
    if (errno != ENOENT) return -errno;
    if (TAG(o_2) != NULLOBJ)
      FREEvm[ARRAY_SIZE(o_2)] = '\0';
    else
      strcpy(FREEvm, ".");

    if (! realpath((char*) FREEvm, (char*) dest))
      return -errno;
    
    len = strlen(dest);
    if (len && dest[len-1] != '/') dest[len++] = '/';
    moveB(VALUE_PTR(o_1), dest + len, ARRAY_SIZE(o_1));
    dest[len+ARRAY_SIZE(o_1)] = '\0';
  }

  if ((file = strrchr(dest, '/'))) {
    edest = file++;
    efile = file + strlen(file);
  }
  else
    edest = efile = file = dest + strlen(dest);

  if (FREEvm + FRAMEBYTES + DALIGN(file-dest)
      + FRAMEBYTES + DALIGN(efile-file)
      >= CEILvm)
    return VM_OVF;
  
  TAG(FREEvm) = (ARRAY|BYTETYPE);
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  ARRAY_SIZE(FREEvm) = edest-dest;
  moveB(dest, VALUE_PTR(FREEvm), ARRAY_SIZE(FREEvm));
  moveframe(FREEvm, o_2);

  FREEvm += FRAMEBYTES + DALIGN(ARRAY_SIZE(FREEvm));
  TAG(FREEvm) = (ARRAY|BYTETYPE);
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  ARRAY_SIZE(FREEvm) = efile-file;
  moveB(file, VALUE_PTR(FREEvm), ARRAY_SIZE(FREEvm));
  moveframe(FREEvm, o_1);

  FREEvm += FRAMEBYTES + DALIGN(ARRAY_SIZE(FREEvm));
  return OK;
}

static int utimes_fd;
DM_INLINE_STATIC int futimes_int(const struct timespec times[2]) {
  return futimens(utimes_fd, times);
}

DM_INLINE_STATIC int utimes_int(const struct timespec times[2]) {
  return utimensat(AT_FDCWD, (char*) FREEvm, times, 0);
}

DM_INLINE_STATIC P utimes_pull_(B *restrict op, struct timespec *restrict t) {
  LBIG tm;
  switch (CLASS(op)) {
    case NUM:
      if (! VALUE(op, &tm)) {
	t->tv_sec = 0;
	t->tv_nsec = UTIME_NOW;
      }
      else {
	t->tv_sec = (time_t) tm;
	if (CLASS(op+FRAMEBYTES) != NUM) return RNG_CHK;
	if (! VALUE(op+FRAMEBYTES, &tm)) return UNDF_VAL;
	t->tv_nsec = (long) tm;
      }
      break;

    case NULLOBJ:
      t->tv_sec = 0;
      t->tv_nsec = UTIME_OMIT;
      break;
  }

  return OK;
}

#define utimes_pull(op, t) do {			\
    P retc = utimes_pull_((op), (t));		\
    if (retc) return retc;			\
  } while (0)

// access-epoch/*/null access-ns mod-epoch/*/null mod-ns
// (dir) (file) / fd | --
P op_utimes(void) {
  struct timespec times[2];
  B* i;
  B* bottom;
  int (*func)(const struct timespec times[2]);

  if (FLOORopds > o_1) return OPDS_UNF;
  switch (TAG(o_1)) {
    case STREAM: {
      B* stream = VALUE_PTR(o_1);
      bottom = o_5;

      if ((utimes_fd = STREAM_FD(stream)) == -1)
	return STREAM_CLOSED;
      func = futimes_int;
      break;
    };

    case ARRAY|BYTETYPE:
      bottom = o_6;
      rpathcat(o_2);
      func = utimes_int;
      break;

    default:
      return OPD_TYP;
  };

  if (FLOORopds > bottom) return OPDS_UNF;
  for (i = bottom + 3*FRAMEBYTES; i >= bottom; i -= FRAMEBYTES)
    switch (CLASS(i)) {
      case NULLOBJ:
	if (TYPE(i)) return OPD_TYP;
	break;

      case NUM:
	if (TYPE(i) >= SINGLETYPE) return OPD_TYP;
	break;

      default:
	return OPD_CLA;
    }

  utimes_pull(bottom, &times[0]);
  utimes_pull(bottom+2*FRAMEBYTES, &times[1]);
  if (func(times)) return -errno;
  
  FREEopds = bottom;
  return OK;
}

#undef D_P_OP
#undef D_P_SET
#undef D_P_SET_LBIG
#undef D_P_SET_BOOL

// (dir)/null (prefix) | fdr fdw (dir) (prefixXXXXXX)
P op_tmpfile(void) {
  P retc;
  B* curr;
  char* tmp;
  char* tmpsub;
  size_t n, nsub;
  P fdr, fdw;

  if (CEILopds <= o2) return OPDS_OVF;
  if (FREEvm + 2*FRAMEBYTES + 2*STREAMBOXBYTES >= CEILvm)
    return VM_OVF;
  
  rtmpcat(o_2);
  if ((fdr = mkstemp(FREEvm)) == -1) return -errno;
  if (! (tmp = strdup(FREEvm))) return -errno;
  if ((retc = addsocket(fdr, &pipetype, NULL))) {
    free(tmp);
    return retc;
  }
  if ((fdw = dup((int) fdr)) == -1) {
    retc = -errno;
    delsocket_force(fdr);
    free(tmp);
    return retc;
  }
  if ((retc = addsocket(fdw, &pipetype, NULL))) {
    delsocket_force(fdr);
    free(tmp);
    return retc;
  }

  curr = FREEvm;
  TAG(curr) = STREAM;
  ATTR(curr) = PARENT;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o_2);
  curr += FRAMEBYTES;

  STREAM_FD(curr) = fdr;
  STREAM_BUFFERED(curr) = FALSE;
  STREAM_RO(curr) = TRUE;
  STREAM_LOCKED(curr) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(curr) = fdr;
  curr += STREAMBOXBYTES;

  TAG(curr) = STREAM;
  ATTR(curr) = PARENT;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o_1);
  curr += FRAMEBYTES;
  
  STREAM_FD(curr) = fdw;
  STREAM_BUFFERED(curr) = FALSE;
  STREAM_RO(curr) = FALSE;
  STREAM_LOCKED(curr) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(curr) = fdw;
  curr += STREAMBOXBYTES;

  tmpsub = strrchr(tmp, '/') + 1;
  nsub = strlen(tmpsub);
  n = strlen(tmp) - nsub;
  if (curr + FRAMEBYTES + DALIGN(n) + FRAMEBYTES + DALIGN(nsub) 
      >= CEILvm) {
    delsocket_force(fdr);
    delsocket_force(fdw);
    free(tmp);
    return VM_OVF;
  }
  
  TAG(curr) = (ARRAY|BYTETYPE);
  ATTR(curr) = PARENT;
  ARRAY_SIZE(curr) = n;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o1);

  curr += FRAMEBYTES;
  moveB((B*) tmp, curr, n);
  curr += DALIGN(n);

  TAG(curr) = (ARRAY|BYTETYPE);
  ATTR(curr) = PARENT;
  ARRAY_SIZE(curr) = nsub;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o2);

  curr += FRAMEBYTES;
  moveB((B*) tmpsub, curr, nsub);
  curr += DALIGN(nsub);

  free(tmp);

  FREEvm = curr;
  FREEopds = o3;
  return OK;
}

// (dir)/null (subdir) | (dir/subdir) [(file)...] true / false
P op_finddir(void) {
  size_t n, nsub;
  B* curr = FREEvm;
  B* top = CEILvm;
  DIR* dir;
  struct dirent* dirent;
  B* f;
  B* fsub;

  if (CEILopds <= o1) return OPDS_OVF;
  rpathcat(o_2);
  if (! (dir = opendir(FREEvm))) return -errno;

  errno = 0;
  while ((dirent = readdir(dir))) {
    if (! strcmp(dirent->d_name, ".") || ! strcmp(dirent->d_name, ".."))
      continue;

    n = strlen(dirent->d_name);
    top -= FRAMEBYTES;
    if (curr + FRAMEBYTES + DALIGN(n) >= top) {
      closedir(dir);
      return VM_OVF;
    }
    TAG(curr) = (ARRAY|BYTETYPE);
    ATTR(curr) = PARENT;
    VALUE_PTR(curr) = curr + FRAMEBYTES;
    ARRAY_SIZE(curr) = n;
    moveframe(curr, top);
    curr += FRAMEBYTES;
    moveB(dirent->d_name, curr, n);
    curr += DALIGN(n);
  }
  if (errno) return -errno;
  if (closedir(dir)) return -errno;

  if (top == CEILvm) {
    TAG(o_2) = BOOL;
    ATTR(o_2) = 0;
    BOOL_VAL(o_2) = FALSE;
    FREEopds = o_1;
    return OK;
  }

  if (TAG(o_2) != NULLOBJ && ARRAY_SIZE(o_2)) { 
    f = VALUE_PTR(o_2);
    n = ARRAY_SIZE(o_2);
  } 
  else {
    f = ".";
    n = 1;
  }
  fsub = VALUE_PTR(o_1);
  nsub = ARRAY_SIZE(o_1);
  if (curr + FRAMEBYTES + DALIGN(n + nsub + 1) > top)
    return VM_OVF;

  TAG(curr) = (ARRAY|BYTETYPE);
  ATTR(curr) = PARENT;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  ARRAY_SIZE(curr) =  n + nsub + ((f[n-1] != '/') ? 1 : 0);
  moveframe(curr, o_2);

  curr += FRAMEBYTES;
  moveB(f, curr, n);
  if (curr[n-1] != '/') curr[n++] = '/';
  moveB(fsub, curr + n, nsub);
  curr += DALIGN(n + nsub);

  TAG(curr) = LIST;
  ATTR(curr) = PARENT;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  LIST_CEIL_PTR(curr) = curr + FRAMEBYTES + (CEILvm - top);
  moveframe(curr, o_1);
  curr += FRAMEBYTES;

  if (curr + (CEILvm - top) > top) return VM_OVF;

  moveframes(top, curr, (CEILvm - top)/FRAMEBYTES);
  curr += (CEILvm - top);

  TAG(o1) = BOOL;
  ATTR(o1) = 0;
  BOOL_VAL(o1) = TRUE;
  
  FREEopds = o2;
  FREEvm = curr;
  return OK;
}

// (dir)/null (file-or-subdir) | --
P op_rmpath(void) {
  rpathcat(o_2);
  if (remove((char*) FREEvm)) return -errno;

  FREEopds = o_2;
  return OK;
}

// (dir)/null (subdir) | (dir) (subdirXXXXXX) <<directory is created>>
P op_tmpdir(void) {
  B* curr;
  char *tmp;
  char *tmpsub;
  size_t n, nsub;

  rtmpcat(o_2);
  if (! mkdtemp(FREEvm)) return -errno;
  if (! (tmp = strdup(FREEvm))) return -errno;
  tmpsub = strrchr(tmp, '/') + 1;
  
  curr = FREEvm;
  nsub = strlen(tmpsub);
  n = strlen(tmp) - nsub;
  if (curr + FRAMEBYTES + DALIGN(n) + FRAMEBYTES + DALIGN(nsub) 
      >= CEILvm) {
    remove(tmp);
    free(tmp);
    return VM_OVF;
  }
  
  TAG(curr) = (ARRAY|BYTETYPE);
  ATTR(curr) = PARENT;
  ARRAY_SIZE(curr) = n;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o_2);

  curr += FRAMEBYTES;
  moveB((B*) tmp, curr, n);
  curr += DALIGN(n);

  TAG(curr) = (ARRAY|BYTETYPE);
  ATTR(curr) = PARENT;
  ARRAY_SIZE(curr) = nsub;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o_1);

  curr += FRAMEBYTES;
  moveB((B*) tmpsub, curr, nsub);
  curr += DALIGN(nsub);

  free(tmp);
  FREEvm = curr;
  return OK;
}

// -- | pid socket false
//    |     socket true
P op_fork(void) {
  pid_t child;
  int sockets[2]; //d-code sockets
  P retc;

  if (CEILopds < o4) return OPDS_OVF;

  if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets))
    return -errno;
  
  if ((retc = dm_setsockopts(sockets[0], PACKET_SIZE))
      || (retc = dm_setsockopts(sockets[1], PACKET_SIZE))) {
    close(sockets[0]);
    close(sockets[1]);
    return retc;
  }

  switch ((child = fork())) {
    case -1: return -errno;
    case 0:
      if (close(sockets[0])) dm_error(errno, "close sockets[0]");

      closesockets_fork();
      if (do_inter_lock_reset) do_inter_lock_reset();

      if ((retc = addsocket(sockets[1], &sockettype, &defaultsocketinfo)))
	dm_error(retc < 0 ? -retc : 0, 
		 "on addsocket for child-server connection");
      
      TAG(o1) = (NULLOBJ|SOCKETTYPE);
      ATTR(o1) = 0;
      SOCKET_VAL(o1) = sockets[1];
      DGRAM_VAL(o1)  = -1;

      TAG(o2) = BOOL;
      ATTR(o2) = 0;
      BOOL_VAL(o2) = TRUE;

      FREEopds = o3;
      return OK;
  };

  DEBUG("forked %li", (long) child);
  if (close(sockets[1])) {
    int errno_ = errno;
    kill(child, SIGKILL);
    close(sockets[0]);
    return -errno_;
  }

  if ((retc = addsocket(sockets[0], &sockettype, &defaultsocketinfo)))
    return retc;

  TAG(o1) = (NULLOBJ|PIDTYPE);
  ATTR(o1) = 0;
  PID_VAL(o1) = child;

  TAG(o2) = (NULLOBJ|SOCKETTYPE);
  ATTR(o2) = 0;
  SOCKET_VAL(o2) = sockets[0];
  DGRAM_VAL(o2)  = -1;

  TAG(o3) = BOOL;
  ATTR(o3) = 0;
  BOOL_VAL(o3) = FALSE;
    
  FREEopds = o4;
  return OK;
}

// -- | ppid
P op_getppid(void) {
  if (CEILopds < o2) return OPDS_OVF;
  TAG(o1) = (NULLOBJ|PIDTYPE);
  ATTR(o1) = 0;
  PID_VAL(o1) = getppid();
  FREEopds = o2;
  return OK;
}

// old-fd | new-fd
P op_copyfd(void) {
  P retc;
  B* streambox;
  B* nstreambox;
  int fdold, fdnew, fdoldl, fdnewl;

  if (TAG(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if ((fdold = STREAM_FD(streambox)) == -1) return STREAM_CLOSED;
  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILvm)
    return VM_OVF;

  fdoldl = STREAM_FD_LOCK(streambox);
  if ((fdnew = dup(fdold)) == -1) return -errno;
  if ((retc = addsocket(fdnew, &pipetype, NULL))) return retc;

  if (fdoldl < 0) fdnewl = fdoldl;
  else if ((fdnewl = dup(fdoldl)) == -1) {
    retc = -errno;
    delsocket_force(fdnew);
    return retc;
  }
  else if ((retc = addsocket(fdnewl, &pipetype, NULL))) {
    delsocket_force(fdnew);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  nstreambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  moveLBIG((LBIG*) streambox, (LBIG*) nstreambox, STREAMBOXBYTES/PACK_FRAME);
  STREAM_FD(nstreambox) = fdnew;
  STREAM_LOCKED(nstreambox) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(nstreambox) = fdnewl;

  moveframe(FREEvm, o_1);
  FREEvm += FRAMEBYTES + STREAMBOXBYTES;
  return OK;
}

// fd | fd#
P op_unmakefd(void) {
  P fd;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (TAG(o_1) != STREAM) return OPD_CLA;
  if ((fd = STREAM_FD(VALUE_PTR(o_1))) == -1)
    return STREAM_CLOSED;

  TAG(o_1) = (NUM|LONGBIGTYPE);
  ATTR(o_1) = 0;
  LONGBIG_VAL(o_1) = fd;
  return OK;
}

// fd | bool
 P op_readonlyfd(void) {
   B* streambox;
   if (FLOORopds > o_1) return OPDS_UNF;
   if (TAG(o_1) != STREAM) return OPD_CLA;
   streambox = VALUE_PTR(o_1);
   
   TAG(o_1) = BOOL;
   ATTR(o_1) = 0;
   BOOL_VAL(o_1) = STREAM_RO(streambox);
   return OK;
 }

// fd# bool-if-read | fd-obj
P op_makefd(void) {
  P fd;
  P retc;
  B* streambox;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (CLASS(o_1) != BOOL) return OPD_CLA;
  if (! PVALUE(o_2, &fd)) return UNDF_VAL;
  if (fd < 0 || 
      (sizeof(P) > sizeof(int) && fd > INT_MAX))
    return RNG_CHK;

  if ((retc = addsocket(fd, &stdtype, NULL)))
    return retc;

  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILvm)
    return VM_OVF;

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm+FRAMEBYTES;
  STREAM_FD(streambox) = (int) fd;
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox)  = BOOL_VAL(o_1);
  STREAM_LOCKED(streambox) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(streambox) = -1;
  moveframe(FREEvm, o_2);

  FREEvm += FRAMEBYTES+STREAMBOXBYTES;  
  FREEopds = o_1;
  return OK;
}

// fds fdd | --
P op_dupfd(void) {
  P retc;
  B* streambox1;
  B* streambox2;
  int fd1, fd2, fdl1, fdl2;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (CLASS(o_1) != STREAM || CLASS(o_2) != STREAM)
    return OPD_CLA;

  streambox1 = VALUE_PTR(o_1);
  streambox2 = VALUE_PTR(o_2);
  fd1 = STREAM_FD(streambox1);
  fd2 = STREAM_FD(streambox2);
  fdl1 = STREAM_FD_LOCK(streambox1);
  fdl2 = STREAM_FD_LOCK(streambox2);

  if (fd1 == -1 || fd2 == -1) return STREAM_CLOSED;
  if (STREAM_RO(streambox1) != STREAM_RO(streambox2))
    return STREAM_DIR;

  if (dup2(fd2, fd1) == -1) return -errno;
  if ((retc = addsocket_dup(fd1))) return retc;

  if (fdl2 < 0) {
    STREAM_FD_LOCK(streambox1) = fdl2;
    if (fdl1 >= 0) delsocket_force(fdl1);
  }
  else {
    if (fdl1 < 0) {
      if ((STREAM_FD_LOCK(streambox1) = dup(fdl2)) == -1) {
	retc = -errno;
	closefd(streambox1);
	return retc;
      }
    }
    else if (dup2(fdl2, fdl1) == -1) {
      retc = -errno;
      closefd(streambox1);
      return retc;
    }

    if ((retc = addsocket_dup(fdl1))) {
      closefd(streambox1);
      return retc;
    }
  }

  FREEopds = o_2;
  return OK;
}

// [(exec) (param)... ] | new executable
P op_spawn(void) {
  B* frame;
  B* nextvm;
  char** argv = (char**) FREEvm;
  int i;
  
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != LIST) return OPD_CLA;

  nextvm = (B*) (argv + (LIST_CEIL_PTR(o_1)-VALUE_PTR(o_1))/FRAMEBYTES + 1);
  if (nextvm >= CEILvm) return VM_OVF;
  
  i = 0;
  for (frame = VALUE_PTR(o_1); 
       frame < LIST_CEIL_PTR(o_1);
       frame += FRAMEBYTES) {
    B* nextvm_;
    if (TAG(frame) != (ARRAY|BYTETYPE)) return OPD_ERR;
    nextvm_ = nextvm + ARRAY_SIZE(frame)+1;
    if (nextvm_ >= CEILvm) return VM_OVF;
    moveB(VALUE_PTR(frame), nextvm, ARRAY_SIZE(frame));
    nextvm[ARRAY_SIZE(frame)] = '\0';
    argv[i++] = (char*) nextvm;
    nextvm = nextvm_;
  }
  argv[i] = NULL;

  closesockets_fork();
  DEBUG("spawned %s", argv[0]);
  execvp(argv[0], argv);
  return -errno;
}

// /VAR (val)/null | --
P op_setenv(void) {
  BOOLEAN nulled;
  static char str[NAMEBYTES+1];

  if (FLOORopds > o_2) return OPDS_UNF;
  switch (TAG(o_1)) {
    case NULLOBJ:
      nulled = TRUE;
      break;
    case ARRAY|BYTETYPE:
      nulled = FALSE;
      break;
    default:
      return OPD_ERR;
  }
  if (CLASS(o_2) != NAME) return OPD_CLA;

  pullname(o_2, str);
  if (! nulled) {
    if (FREEvm + ARRAY_SIZE(o_1) + 1 >= CEILvm) return VM_OVF;
    moveB(VALUE_PTR(o_1), FREEvm, ARRAY_SIZE(o_1));
    FREEvm[ARRAY_SIZE(o_1)] = '\0';
    if (setenv(str, (char*) FREEvm, 1)) return -errno;
  }
  else unsetenv(str);

  FREEopds = o_2;
  return OK;
}

// /VAR | (val)/NULL
P op_getenv(void) {
  static char str[NAMEBYTES+1];
  char* val;
  size_t n;
  
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != NAME) return OPD_CLA;
  
  pullname(o_1, str);
  if (! (val = getenv(str))) {
    TAG(o_1) = NULLOBJ;
    ATTR(o_1) = 0;
    return OK;
  }

  n = strlen(val);
  if (FREEvm + FRAMEBYTES + DALIGN(n) >= CEILvm) return VM_OVF;

  TAG(FREEvm) = (ARRAY|BYTETYPE);
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  ARRAY_SIZE(FREEvm) = n;
  moveframe(FREEvm, o_1);

  FREEvm += FRAMEBYTES;
  moveB((B*) val, FREEvm, n);
  FREEvm += DALIGN(n);

  return OK;
}

// -- | fdread fdwrite
P op_pipefd(void) {
  int pipefd[2];
  P retc;
  B* streambox;
  
  if (CEILopds <= o2) return OPDS_OVF;
  if (FREEvm + 2*(FRAMEBYTES + STREAMBOXBYTES) >= CEILvm)
    return VM_OVF;
  if (pipe(pipefd)) return -errno;

  if ((retc = addsocket(pipefd[0], &pipetype, NULL))) {
    close(pipefd[1]);
    return retc;
  }

  if ((retc = addsocket(pipefd[1], &pipetype, NULL))) {
    delsocket_force(pipefd[0]);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  STREAM_FD(streambox) = pipefd[0];
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox) = TRUE;
  STREAM_LOCKED(streambox) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(streambox) = -1;
  moveframe(FREEvm, o1);
  
  FREEvm += FRAMEBYTES + STREAMBOXBYTES;
  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  STREAM_FD(streambox) = pipefd[1];
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox) = FALSE;
  STREAM_LOCKED(streambox) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(streambox) = -1;
  moveframe(FREEvm, o2);

  FREEvm += FRAMEBYTES + STREAMBOXBYTES;
  FREEopds = o3;
  return OK;
}

// pid sig-encoded | --
P op_killpid(void) {
  int sig;
  UL32 sig_;
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_2) != (NULLOBJ|PIDTYPE)) return OPD_ERR;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! L32VALUE(o_1, (L32*) &sig_)) return UNDF_VAL;

  if (! (sig_ && 0xFF00)) sig_ |= 0x80;
  if (! (sig = decodesig((UW) sig_))) return RNG_CHK;
  if (kill(PID_VAL(o_2), sig)) return -errno;

  FREEopds = o_2;
  return OK;
}

// pid | exit-val-encode {true-if-child-stopped if nb}?
DM_INLINE_STATIC P dmwait(BOOLEAN nb) {
  siginfo_t status;
  pid_t pid;
  B* o_status = o_1;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (nb && CEILopds < o2) return OPDS_OVF;
  if (TAG(o_1) != (NULLOBJ|PIDTYPE)) return OPD_ERR;
  
  DEBUG("waiting for %li", (long) PID_VAL(o_1));
  while ((pid = waitid(P_PID,
		       (id_t) PID_VAL(o_1),
		       &status,
		       nb ? (WNOHANG|WNOWAIT|WEXITED) : WEXITED))
	 == -1) {
    if (errno != EINTR) return -errno;
    checkabort();
  }
  DEBUG("received %i from %li", (int) status.si_pid, (long) PID_VAL(o_1));

  if (nb) {
    if (! pid) {
      TAG(o_1) = BOOL;
      ATTR(o_1) = 0;
      BOOL_VAL(o_1) = FALSE;
      DEBUG("wait %i", 1);
      return OK;
    }

    FREEopds = o2;
    TAG(o_1) = BOOL;
    ATTR(o_1) = 0;
    BOOL_VAL(o_1) = TRUE;
  }

  TAG(o_status) = (NUM|LONG32TYPE);
  ATTR(o_status) = 0;
  if (status.si_code == CLD_EXITED)
    LONG32_VAL(o_status) = (L32) (status.si_status & 0xFF);
  else
    LONG32_VAL(o_status) = ((L32) encodesig(status.si_status)) << 8;

  DEBUG("wait %i", 2);
  return OK;
}

// pid | exit-val-encoded
P op_waitpid(void) {
  return dmwait(FALSE);
}

// pid | false
//     | exit-val-encoded true
P op_checkpid(void) {
  return dmwait(TRUE);
}

struct {int flags; BOOLEAN read;} flags[] = {
  {O_RDONLY,                   TRUE},
  {O_WRONLY|O_TRUNC |O_CREAT,  FALSE},
  {O_WRONLY|O_APPEND|O_CREAT,  FALSE}
};

// (dir)/null (filename) flags perms | fd
P op_openfd(void) {
  P flag;
  int fd;
  int lfd;
  P retc;
  B* streambox;
  ULBIG perm;

  if (FLOORopds > o_2) return OPDS_UNF;
  rpathcat(o_4);

  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! VALUE(o_1, (LBIG*) &perm)) return UNDF_VAL;
  if (perm & ~((ULBIG) FILE_ALL)) return RNG_CHK;

  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &flag)) return UNDF_VAL;
  if (flag < 0 || flag >= (P) (sizeof(flags)/sizeof(flags[0])))
    return RNG_CHK;

  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILvm)
    return VM_OVF;

  if ((fd = open(FREEvm, flags[flag].flags, filemode_un(perm)))
      == -1)
    return -errno;
  if ((retc = addsocket(fd, &pipetype, NULL))) return retc;
  if ((lfd = open(FREEvm, O_RDWR)) == -1) lfd = -2;
  else if ((retc = addsocket(lfd, &pipetype, NULL))) {
    delsocket_force(fd);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  STREAM_FD(streambox) = fd;
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox) = flags[flag].read;
  STREAM_LOCKED(streambox) = STREAM_LOCKED_UN;
  STREAM_FD_LOCK(streambox) = lfd;
  moveframe(FREEvm, o_4);
  FREEvm += FRAMEBYTES + STREAMBOXBYTES;

  FREEopds = o_3;
  return OK;
}

// (buffer) fd | (buffer) fd true / (sub-buffer) false
P op_readfd(void) {
  P fd;
  ssize_t nb, nb_;
  B* streambox;
  B* val;
  P buffd;
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (CEILopds <= o1) return OPDS_OVF;

  if (TAG(o_2) != (ARRAY|BYTETYPE)) return OPD_ERR;
  if (ATTR(o_2) & READONLY) return OPD_ATR;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
  if (! ARRAY_SIZE(o_2)) return RNG_CHK;

  fd = STREAM_FD(streambox);
  buffd = STREAM_BUFFERED(streambox) ? 1 : 0;
  if (buffd) {
    VALUE_PTR(o_2)[0] = STREAM_CHAR(streambox);
    if (fd == -1) {
      ARRAY_SIZE(o_2) = 1;
      ATTR(o_2) &= ~PARENT;
      TAG(o_1) = BOOL;
      ATTR(o_1) = 0;
      BOOL_VAL(o_1) = FALSE;
      STREAM_BUFFERED(streambox) = FALSE;
      return OK;
    }
  }
  
  nb = 0;
  nb_ = ARRAY_SIZE(o_2) - buffd;
  if (nb_) {
    if (! buffd && fd == -1) return STREAM_CLOSED;
    if (fd != -1) {
      val = VALUE_PTR(o_2) + buffd;
      while ((nb = read(fd, val, nb_)) == -1) {
	if (errno != EINTR) return -errno;
	checkabort();
      }
      if (nb < nb_) checkabort();
    }
  }

  if (! buffd && nb_ && ! nb) {
    rclosefd(streambox);

    ARRAY_SIZE(o_2) = 1;
    ATTR(o_2) &= ~PARENT;
    TAG(o_1) = BOOL;
    ATTR(o_1) = 0;
    BOOL_VAL(o_1) = FALSE;
    return OK;
  }

  ARRAY_SIZE(o_2) = (P) nb + buffd;
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_CHAR(streambox) = VALUE_PTR(o_2)[ARRAY_SIZE(o_2)-1];
  ATTR(o_2) &= ~PARENT;

  TAG(o1) = BOOL;
  ATTR(o1) = 0;
  BOOL_VAL(o1) = TRUE;
  FREEopds = o2;
  return OK;
}

// fd | (buffer)
P op_suckfd(void) {
  P fd;
  ssize_t nb;
  B* streambox;
  B* curr;
  
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
  if (FREEvm + FRAMEBYTES + DALIGN(1) >= CEILvm) return VM_OVF;
  
  TAG(FREEvm) = (ARRAY|BYTETYPE);
  ATTR(FREEvm) = PARENT;
  curr = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
 
  if (STREAM_BUFFERED(streambox)) {
    (curr++)[0] = STREAM_CHAR(streambox);
    STREAM_BUFFERED(streambox) = FALSE;
    if (STREAM_FD(streambox) == -1) {
      ARRAY_SIZE(FREEvm) = 1;
      moveframe(FREEvm, o_1);
      FREEvm += FRAMEBYTES + DALIGN(1);
      return OK;
    }
  }
  
  if ((fd = STREAM_FD(streambox)) == -1) return STREAM_CLOSED;

  do {
    while ((nb = read(fd, curr, CEILvm - curr)) == -1) {
      if (errno != EINTR) return -errno;
      checkabort();
    }
    if (nb < CEILvm - curr) checkabort();
    //fprintf(stderr, "suckfd: `%*s'\n", nb, curr);
    curr += nb;
  } while (nb && (DALIGN(curr) < (P) CEILvm));

  if (nb) return VM_OVF;
    
  rclosefd(streambox);
  nb = ARRAY_SIZE(FREEvm) = curr - (FREEvm + FRAMEBYTES);
  moveframe(FREEvm, o_1);
  FREEvm += FRAMEBYTES + DALIGN(nb);
  return OK;
}

// fd | true/false
P op_closedfd(void) {
  B* streambox;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  TAG(o_1) = BOOL;
  ATTR(o_1) = 0;
  BOOL_VAL(o_1) = (STREAM_FD(streambox) == -1) ? TRUE : FALSE;
  return OK;
}

// fd | byte/undef
P op_getfd(void) {
  P fd;
  ssize_t nb;
  B* streambox;
  B byte;
  
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
 
  fd = STREAM_FD(streambox);
  if (STREAM_BUFFERED(streambox)) {
    byte = STREAM_CHAR(streambox);
    STREAM_BUFFERED(streambox) = FALSE;
  }  
  else if (fd == -1) return STREAM_CLOSED;
  else {
    while ((nb = read(fd, &byte, 1)) == -1) {
      if (errno != EINTR) return -errno;
      checkabort();
    }
    if (nb < 1) checkabort();
    if (! nb) {
      byte = BINF;
      rclosefd(streambox);
    }
    else STREAM_CHAR(streambox) = byte;
  }
  
  TAG(o_1) = (NUM|BYTETYPE);
  ATTR(o_1) = 0;
  BYTE_VAL(o_1) = byte;
  return OK;
}

// fd | --
P op_ungetfd(void) {
  B* streambox;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
  if (STREAM_BUFFERED(streambox)) return STREAM_OVF;
  STREAM_BUFFERED(streambox) = TRUE;
  
  FREEopds = o_1;
  return OK;
}

DM_INLINE_STATIC P read_char(P fd, B* c) {
  ssize_t nb;
  while ((nb = read(fd, c, 1)) == -1) {
    if (errno != EINTR) return -errno;
    checkabort();
  }
  checkabort();
  return nb ? OK : DONE;
}

DM_INLINE_STATIC P read_char_nb(P fd, B* c) {
  int flags;
  P retc;

  if ((flags = fcntl(fd, F_GETFL)) == -1
      || fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1)
    return -errno;

  switch (retc = read_char(fd, c)) {
#if EAGAIN != EWOULDBLOCK    
    case -EAGAIN:
#endif
    case -EWOULDBLOCK:
      retc = MORE; break;
  };

  if (fcntl(fd, F_SETFL, flags) == -1)
    return retc < 0 ? retc : -errno;

  checkabort();
  return retc;
}


// (buffer) fd char | (subbuffer) fd true / (subbuffer) false
P op_readtomarkfd(void) {
  P retc;
  P fd;
  B* streambox;
  B* curr;
  B* end;
  B c;

  if (FLOORopds > o_3) return OPDS_UNF;
  if (TAG(o_1) != (NUM|BYTETYPE)) return OPD_ERR;
  if (TAG(o_2) != STREAM) return OPD_CLA;
  if (TAG(o_3) != (ARRAY|BYTETYPE)) return OPD_ERR;

  streambox = VALUE_PTR(o_2);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
  c = BYTE_VAL(o_1);
  
  curr = VALUE_PTR(o_3);
  fd = STREAM_FD(streambox);
  if (STREAM_BUFFERED(streambox)) {
    if (ARRAY_SIZE(o_3) < 1) return RNG_CHK;
    STREAM_BUFFERED(streambox) = FALSE;
    if ((*curr = STREAM_CHAR(streambox)) == c) {
      if (fd == -1) goto closed;
      goto open;
    }
    ++curr;
    if (fd == -1) goto closed;
  }
  else if (fd == -1) return STREAM_CLOSED;
  
  end = VALUE_PTR(o_3) + ARRAY_SIZE(o_3);
  if (curr == end) return RNG_CHK;

  while (! ((retc = read_char(fd, curr))) && *curr != c && ++curr < end);
  if (curr == end) return RNG_CHK;

  switch (retc) {
    case OK:
      STREAM_CHAR(streambox) = c;
      break;
    case DONE:
      if (curr != VALUE_PTR(o_3)) STREAM_CHAR(streambox) = *(curr-1);
      goto closed;
    default: 
      return retc;
  }

 open:
  switch (retc = read_char_nb(fd, &STREAM_CHAR(streambox))) {
    case OK:   STREAM_BUFFERED(streambox) = TRUE; break;
    case MORE: break;
    case DONE: goto closed;
    default:   return retc;
  };

  ARRAY_SIZE(o_3) = curr - VALUE_PTR(o_3);
  ATTR(o_3) &= ~PARENT;
  TAG(o_1) = BOOL;
  ATTR(o_1) = 0;
  BOOL_VAL(o_1) = TRUE;
  return OK;

 closed:
  rclosefd(streambox);

  ARRAY_SIZE(o_3) = curr - VALUE_PTR(o_3);
  ATTR(o_3) &= ~PARENT;
  TAG(o_2) = BOOL;
  ATTR(o_2) = 0;
  BOOL_VAL(o_2) = FALSE;
  FREEopds = o_1;
  return OK;  
}

// fd char | (buffer) fd true / (buffer) false
P op_readtomarkfd_nb(void) {
  P retc;
  P fd;
  ssize_t nb;
  B* streambox;
  B* curr;
  B c;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (CEILopds <= o1) return OPDS_OVF;
  if (TAG(o_1) != (NUM|BYTETYPE)) return OPD_ERR;
  if (TAG(o_2) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_2);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
  c = BYTE_VAL(o_1);

  if (FREEvm + FRAMEBYTES + DALIGN(1) >= CEILvm) return VM_OVF;
  TAG(FREEvm) = (ARRAY|BYTETYPE);
  ATTR(FREEvm) = PARENT;
  curr = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;

  TAG(o1) = BOOL;
  ATTR(o1) = 0;
  
  fd = STREAM_FD(streambox);
  if (STREAM_BUFFERED(streambox)) {
    STREAM_BUFFERED(streambox) = FALSE;
    if ((*curr = STREAM_CHAR(streambox)) == c) {
      if (fd == -1) goto closed;
      goto open;
    }
    ++curr;
    if (fd == -1) goto closed;
  }
  else if (fd == -1) return STREAM_CLOSED;

  if (curr + 1 >= CEILvm) return VM_OVF;
  while (! ((retc = read_char(fd, curr))) && *curr != c && ++curr < CEILvm);
  if ((B*) DALIGN(curr) >= CEILvm) return VM_OVF;
  switch (retc) {
    case OK:
      STREAM_CHAR(streambox) = c;
      break;
    case DONE:
      if (curr != VALUE_PTR(FREEvm)) STREAM_CHAR(streambox) = *(curr-1);
      goto closed;
    default:
      return retc;
  }

 open:
  switch (retc = read_char_nb(fd, &STREAM_CHAR(streambox))) {
    case OK:   STREAM_BUFFERED(streambox) = TRUE; break;
    case MORE: break;
    case DONE: goto closed;
    default:   return retc;
  }

  nb = ARRAY_SIZE(FREEvm) = curr - (FREEvm + FRAMEBYTES);
  moveframe(o_2, o_1);
  moveframe(FREEvm, o_2);
  BOOL_VAL(o1) = TRUE;
  FREEopds = o2;
  FREEvm += FRAMEBYTES + DALIGN(nb);
  return OK;

 closed:
  rclosefd(streambox);
  BOOL_VAL(o1) = FALSE;
  moveframe(o1, o_1);
  nb = ARRAY_SIZE(FREEvm) = curr - (FREEvm + FRAMEBYTES);
  moveframe(FREEvm, o_2);
  FREEvm += FRAMEBYTES + DALIGN(nb);
  return OK;
}

// fd (buffer) | fd
P op_writefd(void) {
  ssize_t nb, nb_, t;
  B* streambox;
  P fd;
  B* f;
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_ERR;
  if (CLASS(o_2) != STREAM) return OPD_CLA;

  streambox = VALUE_PTR(o_2);
  if (STREAM_RO(streambox)) return STREAM_DIR;
  if ((fd = STREAM_FD(streambox)) == -1) return STREAM_CLOSED;

  for (t = 0, nb_ = ARRAY_SIZE(o_1), f = VALUE_PTR(o_1); 
       t < nb_; 
       t += nb) {
    while ((nb = write(fd, f + t, nb_ - t)) 
	   == -1)
      switch (errno) {
	case EINTR: 
	  checkabort();
	  continue;
	case EPIPE:
	  closefd(streambox);
	  return STREAM_EPIPE;
	default:
	  return -errno;
      }
    if (nb < nb_ - t) checkabort();
  }

  FREEopds = o_1;
  return OK;
}

// fd | --
P op_closefd(void) {
  if (FLOORopds > o_1) return OPDS_UNF;
  if (TAG(o_1) != STREAM) return OPD_CLA;
  rclosefd(VALUE_PTR(o_1));
  
  FREEopds = o_1;
  return OK;
}

DM_INLINE_STATIC P x_op_lockfd_int(STREAM_LOCKED_STATE relock) {
  B* streambox;
  int fd;
  struct flock f = {
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0
  };

  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != STREAM) return EXECS_COR;  
  streambox = VALUE_PTR(x_1);

  if (STREAM_FD(streambox) == -1) goto locked;
  switch ((fd = STREAM_FD_LOCK(streambox))) {
    case -1: case -2: return EXECS_COR;
    default: break;
  };
  if (relock == STREAM_LOCKED(streambox)) goto locked;
  switch (relock) {
    case STREAM_LOCKED_UN:
      f.l_type = F_UNLCK;
      break;
    case STREAM_LOCKED_RD:
      f.l_type = F_RDLCK;
      break;
    case STREAM_LOCKED_WR:
      f.l_type = F_WRLCK;
      break;
  }
  
  while (fcntl(fd, F_SETLKW, &f) == -1) {
    if (errno != EINTR) return -errno;
    checkabort();
  }

 locked:
  STREAM_LOCKED(streambox) = relock;

  FREEexecs = x_1;
  repush_stop();
  return OK;
}

static P x_op_lockfd_wr(void) {
  return x_op_lockfd_int(STREAM_LOCKED_WR);
}

static P x_op_lockfd_rd(void) {
  return x_op_lockfd_int(STREAM_LOCKED_RD);
}

static P x_op_lockfd_un(void) {
  return x_op_lockfd_int(STREAM_LOCKED_UN);
}

typedef enum {
  D_LOCK_UN   = STREAM_LOCKED_UN,
  D_LOCK_RD   = STREAM_LOCKED_RD,
  D_LOCK_WR   = STREAM_LOCKED_WR,
  D_LOCK_RDWR = STREAM_LOCKED_WR+1
} D_LOCK_CMD;

// ~active fd | ...
DM_INLINE_STATIC P op_lockfd_int(D_LOCK_CMD lock, int cmd) {
  STREAM_LOCKED_STATE relock;
  int fd;
  B* streambox;
  struct flock f = {
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0
  };

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;
  if (! (ATTR(o_2) & ACTIVE)) return OPD_ATR;
  switch ((fd = STREAM_FD_LOCK(streambox))) {
    case -1: return STREAM_UNLOCKABLE_TYPE;
    case -2: return STREAM_UNLOCKABLE_FILE;
    default: break;
  };

  if (lock != D_LOCK_RDWR) relock = (STREAM_LOCKED_STATE) lock;
  else switch (STREAM_LOCKED(streambox)) {
    case STREAM_LOCKED_UN: case STREAM_LOCKED_RD:
      relock = STREAM_LOCKED_RD;
      break;
    case STREAM_LOCKED_WR:
      relock = STREAM_LOCKED_WR;
      break;
  };
  
  if (relock != STREAM_LOCKED(streambox)) {
    switch (relock) {
      case STREAM_LOCKED_UN:
	f.l_type = F_UNLCK;
	break;
      case STREAM_LOCKED_RD:
	f.l_type = F_RDLCK;
	break;
      case STREAM_LOCKED_WR:
	f.l_type = F_WRLCK;
	break;
    }

    while (fcntl(fd, cmd, &f) == -1) {
      if (errno != EINTR) return -errno;
      checkabort();
    }
  };

  moveframe(o_1, x1);
  
  TAG(x2) = OP;
  ATTR(x2) = ACTIVE;
  switch (STREAM_LOCKED(streambox)) {
    case STREAM_LOCKED_UN:
      OP_NAME(x2) = "x_lockfd_un";
      OP_CODE(x2) = x_op_lockfd_un;
      break;
    case STREAM_LOCKED_RD:
      OP_NAME(x2) = "x_lockfd_rd";
      OP_CODE(x2) = x_op_lockfd_rd;
      break;
    case STREAM_LOCKED_WR:
      OP_NAME(x2) = "x_lockfd_wr";
      OP_CODE(x2) = x_op_lockfd_wr;
      break;
  };
  STREAM_LOCKED(streambox) = relock;

  TAG(x3) = BOOL;
  ATTR(x3) = (STOPMARK|ABORTMARK|ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_2, x4);
  FREEexecs = x5;
  FREEopds = o_2;

  return OK;
}


// ~active fd | ...
P op_lockfd(void) {
  return op_lockfd_int(D_LOCK_RDWR, F_SETLKW);
}

// ~active fd | ..
P op_lockfd_ex(void) {
  return op_lockfd_int(D_LOCK_WR, F_SETLKW);
}

P op_lockfd_sh(void) {
  return op_lockfd_int(D_LOCK_RD, F_SETLKW);
}

// ~active fd | --
P op_unlockfd(void) {
  return op_lockfd_int(D_LOCK_UN, F_SETLKW);
}

// ~active fd | ... true-if-success
DM_INLINE_STATIC P op_trylockfd_int(D_LOCK_CMD st) {
  B* test;
  P retc;

  if (CEILexecs < x2) return EXECS_OVF;
  test = x1;
  TAG(x1) = BOOL;
  ATTR(x1) = 0;
  FREEexecs = x2;

  switch (retc = op_lockfd_int(st, F_SETLK)) {
    case -EACCES: case -EAGAIN:
      BOOL_VAL(test) = FALSE;
      return OK;

    case OK:
      BOOL_VAL(test) = TRUE;
      return OK;

    default:
      FREEexecs = test;
      return retc;
  };
}

P op_trylockfd(void) {
  return op_trylockfd_int(D_LOCK_RDWR);
}

P op_trylockfd_ex(void) {
  return op_trylockfd_int(D_LOCK_WR);
}

P op_trylockfd_sh(void) {
  return op_trylockfd_int(D_LOCK_RD);
}

////// tokenizing /////////////////////////////////////

static B* streambox;

DM_INLINE_STATIC B getc_fd(P* retc) {
  ssize_t nb;

  if (*retc) return 0;
  if (recvd_quit) {
    *retc = QUIT;
    return 0;
  }
  if (abortflag) {
    *retc = ABORT;
    return 0;
  }

  if (STREAM_BUFFERED(streambox)) {
    STREAM_BUFFERED(streambox) = FALSE;
    return STREAM_CHAR(streambox) & 0x7F;
  }

  if (STREAM_FD(streambox) == -1) {
    *retc = STREAM_CLOSED;
    return 0;
  }

  while ((nb = read(STREAM_FD(streambox), 
		    &STREAM_CHAR(streambox), 1)) == -1) {
    if (errno != EINTR) {
      *retc = -errno;
      return 0;
    }
    if (recvd_quit) {
      *retc = QUIT;
      return 0;
    }
    if (abortflag) {
      *retc = ABORT;
      return 0;
    }
  }
  
  if (! nb) {
    if (recvd_quit) *retc = QUIT;
    if (abortflag) *retc = ABORT;
    return 0;
  }
  return STREAM_CHAR(streambox) & 0x7F;
}

DM_INLINE_STATIC void ungetc_fd(P* retc) {
  if (recvd_quit) {
    *retc = QUIT;
    return;
  }
  if (abortflag) {
    *retc = ABORT;
    return;
  }

  if (STREAM_BUFFERED(streambox)) {
    *retc = STREAM_OVF;
    return;
  }

  STREAM_BUFFERED(streambox) = TRUE;
}

DM_INLINE_STATIC P tokenizefd(B* streambox_) {
  streambox = streambox_;
  getc_func = getc_fd;
  ungetc_func = ungetc_fd;
  return tokenize_gen();
}

DM_INLINE_STATIC P tokenfd(void) {
  B* streambox = VALUE_PTR(o_1);
  if (! STREAM_RO(streambox)) 
    return STREAM_DIR;

  if (STREAM_FD(streambox) == -1
      && ! STREAM_BUFFERED(streambox)) 
    return STREAM_CLOSED;
  return tokenizefd(streambox);
}

DM_INLINE_STATIC P execfd(void) {
  B* streambox = VALUE_PTR(x_1);
  if (! STREAM_RO(streambox)) 
    return STREAM_DIR;

  if (STREAM_FD(streambox) == -1
      && ! STREAM_BUFFERED(streambox))
    return STREAM_CLOSED;

  return tokenizefd(streambox);
}

DM_INLINE_STATIC P usedfd(void) {
  B* streambox = VALUE_PTR(o_1);

  TAG(o_1) = NUM|LONGBIGTYPE;
  ATTR(o_1) = 0;
  LONGBIG_VAL(o_1) = STREAM_RO(streambox) && STREAM_BUFFERED(streambox) 
    ? 1 : 0;
  return OK;
}

// obj | obj
DM_INLINE_STATIC P cleanupfd(void) {
  P retc;
  if (CLASS(o_1) != STREAM
      || STREAM_FD(VALUE_PTR(o_1)) == -1)
    return OK;

  if ((retc = op_closefd())) return retc;

  // push the stream back on the stack for restore.
  FREEopds = o2;
  return OK;
}

void setupfd(void) {
  cleanupfd_func = cleanupfd;
  execfd_func = execfd;
  tokenfd_func = tokenfd;
  usedfd_func = usedfd;
}
