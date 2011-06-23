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

/* (file) | 
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
   
P op_statvfs(void) {
  struct statvfs s;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (CEILopds < FREEopds + 10*FRAMEBYTES) return OPDS_OVF;

  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_CLA;
  if (FREEvm + ARRAY_SIZE(o_1) + 1 >= CEILvm)
    return VM_OVF;
  moveB(VALUE_PTR(o_1), FREEvm, ARRAY_SIZE(o_1));
  FREEvm[ARRAY_SIZE(o_1)] = '\0';

  while (statvfs(FREEvm, &s)) {
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

#undef D_P_OP
#undef D_P_SET
#undef D_P_SET_LBIG
#undef D_P_SET_BOOL

// (dir)/null (prefix) | fdr fdw (dir) (prefixXXXXXX)
P op_tmpfile(void) {
  P retc;
  B* curr = FREEvm;
  char *tmp;
  char *tmpsub;
  size_t n, nsub;
  P fdr, fdw;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (CEILopds <= o2) return OPDS_OVF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_CLA;
  if (FREEvm + 2*FRAMEBYTES + 2*STREAMBOXBYTES >= CEILvm)
    return VM_OVF;

  switch (TAG(o_2)) {
    case NULLOBJ:
      tmp = getenv("TMPDIR");
      if (! tmp || ! *tmp) tmp = "/tmp";
      n = strlen(tmp);
      if (curr + n >= CEILvm) return VM_OVF;
      moveB(tmp, curr, n);
      curr += n;
      break;
 
    case (ARRAY|BYTETYPE):
      if (curr + ARRAY_SIZE(o_2) + 1 >= CEILvm) return VM_OVF;
      if (! ARRAY_SIZE(o_2)) (curr++)[0] = '.';
      else {
	moveB(VALUE_PTR(o_2), curr, ARRAY_SIZE(o_2));
	curr += ARRAY_SIZE(o_2);
      }
      break;
      
    default: return OPD_ERR;
  };

  if (curr[-1] != '/') {
    if (curr + 1 >= CEILvm) return VM_OVF;
    (curr++)[0] = '/';
  }
  if (curr + ARRAY_SIZE(o_1) + 6 + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_1), (B*) curr, ARRAY_SIZE(o_1));
  curr += ARRAY_SIZE(o_1);
  moveB((B*) "XXXXXX", curr, 6);
  curr += 6;
  curr[0] = '\0';

  if ((fdr = mkstemp(FREEvm)) == -1) return -errno;
  if (! (tmp = strdup(FREEvm))) return -errno;
  if ((retc = addsocket(fdr, &pipetype, NULL))) {
    delsocket_force(fdr);
    free(tmp);
    return retc;
  }
  if ((fdw = dup((int) fdr)) == -1) {
    retc = -errno;
    delsocket_force(fdr);
    delsocket_force(fdw);
    free(tmp);
    return retc;
  }
  if ((retc = addsocket(fdw, &pipetype, NULL))) {
    delsocket_force(fdr);
    delsocket_force(fdw);
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
  curr += STREAMBOXBYTES;

  TAG(curr) = STREAM;
  ATTR(curr) = PARENT;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  moveframe(curr, o_1);
  curr += FRAMEBYTES;
  
  STREAM_FD(curr) = fdw;
  STREAM_BUFFERED(curr) = FALSE;
  STREAM_RO(curr) = FALSE;
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
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (CEILopds <= o1) return OPDS_OVF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_ERR;
  switch (TAG(o_2)) {
    case (ARRAY|BYTETYPE):
      if (ARRAY_SIZE(o_2)) {
	if (curr + ARRAY_SIZE(o_2) >= CEILvm) return VM_OVF;
	moveB(VALUE_PTR(o_2), curr, ARRAY_SIZE(o_2));
	curr += ARRAY_SIZE(o_2);
	break;
      }
      // else intentional fallthrough

    case NULLOBJ:
      if (curr + 1 >= CEILvm) return VM_OVF;
      (curr++)[0] = '.';
      break;

    default:
      return OPD_ERR;
  };

  if (curr[-1] != '/' && ARRAY_SIZE(o_1)) (curr++)[0] = '/';
  if (curr + ARRAY_SIZE(o_1) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_1), curr, ARRAY_SIZE(o_1));
  curr[ARRAY_SIZE(o_1)] = '\0';

  if (! (dir = opendir(FREEvm))) return -errno;
  curr = FREEvm;
  
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
  B* curr = FREEvm;
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_ERR;
  switch (TAG(o_2)) {
    case (ARRAY|BYTETYPE):
      if (ARRAY_SIZE(o_2)) {
	if (curr + ARRAY_SIZE(o_2) >= CEILvm) return VM_OVF;
	moveB(VALUE_PTR(o_2), curr, ARRAY_SIZE(o_2));
	curr += ARRAY_SIZE(o_2);
	break;
      }
      // else intentional fallthrough

    case NULLOBJ:
      if (curr + 1 >= CEILvm) return VM_OVF;
      (curr++)[0] = '.';
      break;

    default:
      return OPD_ERR;
  };

  if (curr[-1] != '/') (curr++)[0] = '/';
  if (curr + ARRAY_SIZE(o_1) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_1), curr, ARRAY_SIZE(o_1));
  curr[ARRAY_SIZE(o_1)] = '\0';
  if (remove((char*) FREEvm)) return -errno;

  FREEopds = o_2;
  return OK;
}

// (dir)/null (subdir) | (dir) (subdirXXXXXX) <<directory is created>>
P op_tmpdir(void) {
  B* curr = FREEvm;
  char *tmp;
  char *tmpsub;
  size_t n, nsub;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_CLA;

  switch (TAG(o_2)) {
    case NULLOBJ:
      tmp = getenv("TMPDIR");
      if (! tmp || ! *tmp) tmp = "/tmp";
      n = strlen(tmp);
      if (curr + n >= CEILvm) return VM_OVF;
      moveB(tmp, curr, n);
      curr += n;
      break;
 
    case (ARRAY|BYTETYPE):
      if (curr + ARRAY_SIZE(o_2) + 1 >= CEILvm) return VM_OVF;
      if (! ARRAY_SIZE(o_2)) (curr++)[0] = '.';
      else {
	moveB(VALUE_PTR(o_2), curr, ARRAY_SIZE(o_2));
	curr += ARRAY_SIZE(o_2);
      }
      break;
      
    default: return OPD_ERR;
  };

  if (curr[-1] != '/') {
    if (curr + 1 >= CEILvm) return VM_OVF;
    (curr++)[0] = '/';
  }
  if (curr + ARRAY_SIZE(o_1) + 7 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_1), (B*) curr, ARRAY_SIZE(o_1));
  curr += ARRAY_SIZE(o_1);
  moveB((B*) "XXXXXX", curr, 6);
  curr += 6;
  curr[0] = '\0';
  
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
  B* streambox;
  B* nstreambox;
  int fdold, fdnew;

  if (TAG(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if ((fdold = STREAM_FD(streambox)) == -1) return STREAM_CLOSED;
  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILvm)
    return VM_OVF;

  if ((fdnew = dup(fdold)) == -1) return -errno;

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  nstreambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  moveLBIG((LBIG*) streambox, (LBIG*) nstreambox, STREAMBOXBYTES/PACK_FRAME);
  STREAM_FD(nstreambox) = fdnew;

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
  moveframe(FREEvm, o_2);

  FREEvm += FRAMEBYTES+STREAMBOXBYTES;  
  FREEopds = o_1;
  return OK;
}

// fds fdd | --
P op_dupfd(void) {
  B* streambox1;
  B* streambox2;

  if (FLOORopds > o_2) return OPDS_UNF;
  if (CLASS(o_1) != STREAM || CLASS(o_2) != STREAM)
    return OPD_CLA;

  streambox1 = VALUE_PTR(o_1);
  streambox2 = VALUE_PTR(o_2);
  if (STREAM_FD(streambox1) == -1 || STREAM_FD(streambox2) == -1)
    return STREAM_CLOSED;
  if (STREAM_RO(streambox1) != STREAM_RO(streambox2))
    return STREAM_DIR;

  if (dup2(STREAM_FD(streambox2), STREAM_FD(streambox1))
      == -1)
    return -errno;

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
    delsocket_force(pipefd[1]);
    return retc;
  }

  if ((retc = addsocket(pipefd[1], &pipetype, NULL))) {
    delsocket_force(pipefd[0]);
    delsocket_force(pipefd[1]);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  STREAM_FD(streambox) = pipefd[0];
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox) = TRUE;
  moveframe(FREEvm, o1);
  
  FREEvm += FRAMEBYTES + STREAMBOXBYTES;
  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  STREAM_FD(streambox) = pipefd[1];
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox) = FALSE;
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
  {O_RDONLY,                  TRUE},
  {O_WRONLY|O_TRUNC|O_CREAT,  FALSE},
  {O_WRONLY|O_APPEND,         FALSE}
};

// (dir) (filename) flags | fd
P op_openfd(void) {
  P flag;
  B* nextvm;
  int fd;
  P retc;
  B* streambox;

  if (FLOORopds > o_3) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &flag)) return UNDF_VAL;
  if (flag < 0 || flag >= (P) (sizeof(flags)/sizeof(flags[0])))
    return RNG_CHK;
  if (TAG(o_2) != (ARRAY|BYTETYPE) || TAG(o_3) != (ARRAY|BYTETYPE))
    return OPD_ERR;
  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILvm)
    return VM_OVF;

  if (FREEvm + ARRAY_SIZE(o_3)+1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_3), FREEvm, ARRAY_SIZE(o_3));
  nextvm = FREEvm + ARRAY_SIZE(o_3);
  if (ARRAY_SIZE(o_3) != 0 && nextvm[-1] != '/')
    (nextvm++)[0] = '/';
  if (nextvm + ARRAY_SIZE(o_2) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_2), nextvm, ARRAY_SIZE(o_2));
  nextvm += ARRAY_SIZE(o_2);
  nextvm[0] = 0;

  if ((fd = open(FREEvm, flags[flag].flags, 
		 S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH))
      == -1)
    return -errno;
  if ((retc = addsocket(fd, &pipetype, NULL))) {
    delsocket_force(fd);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  streambox = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  STREAM_FD(streambox) = fd;
  STREAM_BUFFERED(streambox) = FALSE;
  STREAM_RO(streambox) = flags[flag].read;
  moveframe(FREEvm, o_3);
  FREEvm += FRAMEBYTES + STREAMBOXBYTES;

  FREEopds = o_2;
  return OK;
}

// (buffer) fd | (buffer) fd true / (sub-buffer) false
P op_readfd(void) {
  P retc;
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
    STREAM_FD(streambox) = -1;
    if ((retc = delsocket_proc(fd))) return retc;
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
  P retc;
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
    
  STREAM_FD(streambox) = -1;
  if ((retc = delsocket_proc(fd))) return retc;
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
  P retc;
  
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
      STREAM_FD(streambox) = -1;
      if ((retc = delsocket_proc(fd))) return retc;
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
  STREAM_FD(streambox) = -1;
  if (fd != -1 && (retc = delsocket_proc(fd))) return retc;
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
  STREAM_FD(streambox) = -1;
  if (fd != -1 && (retc = delsocket_proc(fd))) return retc;
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
	  delsocket_proc(fd);
	  STREAM_FD(streambox) = -1;
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
  B* streambox;
  P retc = OK;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (TAG(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (STREAM_FD(streambox) != -1) {
    retc = delsocket_proc(STREAM_FD(streambox));
    STREAM_FD(streambox) = -1;
  }
  
  FREEopds = o_1;
  return retc;
}

P x_op_lockfd(void) {
  B* streambox;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (CLASS(x_1) != STREAM) return EXECS_COR;
  
  streambox = VALUE_PTR(x_1);
  if (STREAM_FD(streambox) != -1)
    while (lockf(STREAM_FD(streambox), 0, F_ULOCK)) {
      if (errno != EINTR) return -errno;
      checkabort();
    }

  FREEexecs = x_1;
  repush_stop();
  return OK;
}

P x_op_unlockfd(void) {
  B* streambox;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (CLASS(x_1) != STREAM) return EXECS_COR;
  
  streambox = VALUE_PTR(x_1);
  if (STREAM_FD(streambox) != -1)
    while (lockf(STREAM_FD(streambox), 0, F_LOCK)) {
      if (errno != EINTR) return -errno;
      checkabort();
    }

  FREEexecs = x_1;
  return repush_stop();
}

// ~active fd | ...
P op_lockfd(void) {
  B* streambox;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;
  if (! (ATTR(o_2) & ACTIVE)) return OPD_ATR;

  while (lockf(STREAM_FD(streambox), F_LOCK, 0)) {
    if (errno != EINTR) return -errno;
    checkabort();
  }

  moveframe(o_1, x1);
  
  TAG(x2) = OP;
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_lockfd";
  OP_CODE(x2) = x_op_lockfd;

  TAG(x3) = BOOL;
  ATTR(x3) = (STOPMARK|ABORTMARK|ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_2, x4);
  FREEexecs = x5;
  FREEopds = o_1;

  return OK;
}

// ~active fd | --
P op_unlockfd(void) {
  B* streambox;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;
  if (! (ATTR(o_2) & ACTIVE)) return OPD_ATR;

  while (lockf(STREAM_FD(streambox), F_ULOCK, 0)) {
    if (errno != EINTR) return -errno;
    checkabort();
  }

  moveframe(o_1, x1);
  
  TAG(x2) = OP;
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_unlockfd";
  OP_CODE(x2) = x_op_unlockfd;

  TAG(x3) = BOOL;
  ATTR(x3) = (STOPMARK|ABORTMARK|ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_2, x4);
  FREEexecs = x5;
  FREEopds = o_1;

  return OK;
}

// ~active fd | ... true-if-success
P op_trylockfd(void) {
  B* streambox;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x6) return EXECS_OVF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;
  if (! (ATTR(o_2) & ACTIVE)) return OPD_ATR;

  while (lockf(STREAM_FD(streambox), F_TLOCK, 0))
    switch (errno) {
      case EACCES: case EAGAIN: 
	TAG(o_2) = BOOL;
	ATTR(o_2) = 0;
	BOOL_VAL(o_2) = FALSE;
	FREEopds = o_1;
	return OK;
	
      case EINTR: checkabort();
	break;

      default:
	return -errno;
    }

  TAG(x1) = BOOL;
  ATTR(x1) = 0;
  BOOL_VAL(x1) = TRUE;
  
  moveframe(o_1, x2);

  TAG(x3) = OP;
  ATTR(x3) = ACTIVE;
  OP_NAME(x3) = "x_lockfd";
  OP_CODE(x3) = x_op_lockfd;

  TAG(x4) = BOOL;
  ATTR(x4) = (STOPMARK|ABORTMARK|ACTIVE);
  BOOL_VAL(x4) = FALSE;

  moveframe(o_2, x5);
  FREEexecs = x6;
  FREEopds = o_2;

  return OK;
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

DM_INLINE_STATIC P cleanupfd(void) {
  P retc;

  if (CLASS(o_1) != STREAM
      || STREAM_FD(VALUE_PTR(o_1)) == -1)
    return OK;

  if (! (retc = op_closefd()))
    FREEopds = o2;
  return retc;
}

void setupfd(void) {
  cleanupfd_func = cleanupfd;
  execfd_func = execfd;
  tokenfd_func = tokenfd;
  usedfd_func = usedfd;
}
