

/*==================== D machine Rev. 3.0 (Linux): dm7.c ======================

     - time and file operators
          - gettime
          - localtime
          - getwdir
          - setwdir
          - readfile
          - writefile
          - writeboxfile
          - readboxfile
          - findfiles
	  - findfile
          - tosystem
          - fromsystem
          - transcribe
*/

#include "dm.h"
#if DISABLE_OPENSSL
#undef ENABLE_OPENSSL
#endif //DISABLE_OPENSSL

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <fnmatch.h>
#include <sys/wait.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <limits.h>
#include <time.h>

#if ENABLE_OPENSSL
#include <openssl/sha.h>
#endif //ENABLE_OPENSSL

#include "dm7.h"
#include "dm2.h"
#include "srandomdev-local.h"


/*------------------------------------------------ random
  -- | [0d..1d]
*/

P op_random(void) 
{
  if (o2 > CEILopds) return OPDS_OVF;
  TAG(o1) = NUM | DOUBLETYPE;
  ATTR(o1) = 0;
  DOUBLE_VAL(o1) = random() / (double) RAND_MAX;
  FREEopds = o2;
  return OK;
}

//  integer/* | --
P op_random_init(void) 
{
  LBIG v;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (TYPE(o_1) >= SINGLETYPE) return OPD_TYP;

  if (! VALUE(o_1, &v)) srandomdev();
  else srandom((unsigned int) (v % UINT_MAX));
  
  FREEopds = o_1;
  return OK;
}

//

/*--------------------------------------------------- gettime
   -- | time

 - returns compacted time as long numeral (seconds since something)
 - use 'localtime' to convert into date and time
*/

P op_gettime(void)
{
  struct timeval t;
  if (o2 > CEILopds) return OPDS_OVF;
  
  errno = 0;
  if (gettimeofday(&t, NULL)) 
    return errno ? -errno : CLOCK_ERR;

  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (LBIG) t.tv_sec;

  FREEopds = o2;
  return OK;
}

/*--------------------------------------------------- gettimeofday
   -- | time-secs time-usecs

 - returns compacted time as long numeral (seconds since something)
 -  and a long numeral of microsecs
 - use 'localtime' on time-secs to convert into date and time
*/

P op_gettimeofday(void)
{
  struct timeval t;
  if (o3 > CEILopds) return OPDS_OVF;
  
  errno = 0;
  if (gettimeofday(&t, NULL)) 
    return errno ? -errno : CLOCK_ERR;

  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (LBIG) t.tv_sec;
  TAG(o2) = NUM | LONGBIGTYPE;
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = (LBIG) t.tv_usec;

  FREEopds = o3;
  return OK;
}

/*---------------------------------------------------- profiletime
  ~active <x of >=8> | ... <x of 8>
  0,1 = user time
  2,3 = system time
  4,5 = user time of children
  6,7 = system time of children
  times are in seconds, useconds
*/

#define reterr(errv) do {retc = errv; goto err;} while (0)
#define upusage(index, group, ctype, ttype)		\
  ((L64*) VALUE_PTR(x_1))[index]			\
    = usage_##group.ru_##ctype##time.tv_##ttype		\
      - ((L64*) VALUE_PTR(x_1))[index]
    
static P x_op_profiletime(void) {
  struct rusage usage_self;
  struct rusage usage_children;
  P retc;

  if (getrusage(RUSAGE_SELF, &usage_self)
      || getrusage(RUSAGE_CHILDREN, &usage_children)) 
    reterr(-errno);
  if (x_1 < FLOORexecs) reterr(EXECS_UNF);
  if (TAG(x_1) != (ARRAY|LONG64TYPE)) reterr(EXECS_COR);
  if (ARRAY_SIZE(x_1) != 8) reterr(EXECS_COR);
  if (o2 > CEILopds) reterr(OPDS_OVF);

  upusage(0, self,     u, sec);
  upusage(1, self,     u, usec);
  upusage(2, self,     s, sec);
  upusage(3, self,     s, usec);
  upusage(4, children, u, sec);
  upusage(5, children, u, usec);
  upusage(6, children, s, sec);
  upusage(7, children, s, usec);

  moveframe(x_1, o1);
  FREEopds = o2;
  FREEexecs = x_1;

  return OK;

 err:
  if (x_1 >= FLOORexecs && TAG(x_1) == (ARRAY|LONG64TYPE))
    FREEexecs = x_1;
  return retc;
}

#undef upusage
#undef reterr

#define upusage(index, group, ctype, ttype) \
  ((L64*) VALUE_PTR(x1))[index] \
    = usage_##group.ru_##ctype##time.tv_##ttype

P op_profiletime(void) {
  struct rusage usage_self;
  struct rusage usage_children;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (! (ATTR(o_2) & ACTIVE)) return OPD_ATR;
  if (TAG(o_1) != (ARRAY|LONG64TYPE)) return OPD_TYP;
  if (ARRAY_SIZE(o_1) < 8) return RNG_CHK;
  if (x4 > CEILexecs) return EXECS_OVF;  

  moveframe(o_1, x1);
  ARRAY_SIZE(x1) = 8;

  TAG(x2) = OP;
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_profiletime";
  OP_CODE(x2) = x_op_profiletime;
  
  moveframe(o_2, x3);
  ATTR(x3) &= ~PARENT;

  if (getrusage(RUSAGE_SELF, &usage_self)
      || getrusage(RUSAGE_CHILDREN, &usage_children))
    return -errno;

  upusage(0, self,     u, sec);
  upusage(1, self,     u, usec);
  upusage(2, self,     s, sec);
  upusage(3, self,     s, usec);
  upusage(4, children, u, sec);
  upusage(5, children, u, usec);
  upusage(6, children, s, sec);
  upusage(7, children, s, usec);

  FREEopds = o_2;
  FREEexecs = x4;

  return OK;
}

#undef upusage

/*--------------------------------------------------- localtime
  time  <L of >=6 > | <L year month day hour min sec >

 - converts compacted 'time' into local time

NOTE: month[1...12], day[1...31], hour[0...23], min,sec[0...59]
*/

P op_localtime(void)
{
  time_t dt; 
  P dt_; 
  struct tm *ldt;
  LBIG *p;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &dt_)) return UNDF_VAL;
  if (TAG(o_1) != (ARRAY | LONGBIGTYPE)) return OPD_ERR;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  if (ARRAY_SIZE(o_1) < 6) return RNG_CHK;
  dt = (time_t) dt_;
  ldt = localtime(&dt);
  p = (LBIG *)VALUE_BASE(o_1);
  p[0] = ldt->tm_year + 1900; 
  p[1] = ldt->tm_mon + 1; 
  p[2] = ldt->tm_mday;
  p[3] = ldt->tm_hour; 
  p[4] = ldt->tm_min; 
  p[5] = ldt->tm_sec;

  ARRAY_SIZE(o_1) = 6;
  moveframe(o_1,o_2);
  FREEopds = o_1;
  return OK;
}

// sec nano-sec | --
P op_sleep(void) {
  LBIG sec, nsec;
  struct timespec req, rem;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM || CLASS(o_1) != NUM) return OPD_CLA;
  if (! VALUE(o_2, &sec) || ! VALUE(o_1, &nsec)) return UNDF_VAL;
  if (nsec > LONG_MAX || nsec < 0) return RNG_CHK;
  if (((LBIG) ((time_t) sec)) != sec || sec < 0) return RNG_CHK;
  req.tv_sec = (time_t) sec;
  req.tv_nsec = (long) nsec;
  while (nanosleep(&req, &rem)) {
    switch (errno) {
      case EINTR: checkabort();
      default: return -errno;
    }
    req = rem;
  }

  FREEopds = o_2;
  return OK;
}

/*---------------------------------------------------- getwdir
   --- | substring

  - allocates 'string', the absolute filename of
    the current working directory
*/

P op_getwdir(void)
{
  B *curr;
  size_t nb;

  if (o1 >= CEILopds) return OPDS_OVF;
  if (FREEvm + FRAMEBYTES > CEILvm) return VM_OVF;
  TAG(FREEvm) = (ARRAY|BYTETYPE);
  ATTR(FREEvm) = PARENT;
  curr = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;

  if (! getcwd(curr, CEILvm - curr)) return -errno;
 
  nb = ARRAY_SIZE(FREEvm) = strlen((char*) curr);
  moveframe(FREEvm, o1);
  FREEopds = o2;
  FREEvm += FRAMEBYTES + DALIGN(nb);
  return OK;
}

/*---------------------------------------------------- setwdir
     string | --

  - sets the current working directory to 'string'
*/

P op_setwdir(void)
{
  P nb;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  nb = ARRAY_SIZE(o_1);
  if (FREEvm + nb >= CEILvm) return VM_OVF;

  moveB(VALUE_PTR(o_1), FREEvm, nb);
  FREEvm[nb] = '\0';
  if (chdir((char*) FREEvm)) return -errno;
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- readfile
   dir filename string | substring

  - dir is a string specifying the directory
  - filename is a string
  - the file contents are saved in substrings of 'string'
  - when the file contents exceed the capacity of 'string',
    a 'range check' error is reported
*/

P op_readfile(void)
{
  int fd;
  P nb, atmost;
  B *p;
  B* oldfreevm = FREEvm;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_ERR;

  if (FREEvm + ARRAY_SIZE(o_3)+1 > CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_3), FREEvm, ARRAY_SIZE(o_3));
  FREEvm += ARRAY_SIZE(o_3);
  if (FREEvm[-1] != '/') (FREEvm++)[0] = '/';

  if (FREEvm + ARRAY_SIZE(o_2) + 1 > CEILvm) {
    FREEvm = oldfreevm;
    return VM_OVF;
  }
  moveB(VALUE_PTR(o_2), FREEvm, ARRAY_SIZE(o_2));
  FREEvm[ARRAY_SIZE(o_2)] = '\000';
  FREEvm = oldfreevm;

  if ((fd = open((char*)FREEvm, O_RDONLY)) == -1) {
    checkabort();
    return -errno;
  }
  p = (B *)VALUE_BASE(o_1); atmost = ARRAY_SIZE(o_1);

  do {
    while ((nb = read(fd, p, atmost)) == -1)
      if (errno == EINTR) checkabort();
      else return -errno;
    if (nb < atmost) checkabort();
    p += nb;
    atmost -= nb;
    if (! atmost) return RNG_CHK;
  } while (nb > 0);

  while (close(fd) == -1)
    if (errno == EINTR) checkabort();
    else return -errno;
 
  ARRAY_SIZE(o_1) = p - (B *)VALUE_BASE(o_1);
  moveframe(o_1, o_3);
  FREEopds = o_2;
  return OK;
}

/*---------------------------------------------------- writefile
   string dir filename | --

  - dir is a string specifying the directory
  - filename is a string
  - 'string' contains the byte array to be written
*/

P op_writefile(void)
{
  int fd;
  P nb, atmost;
  B *p;
  B* oldfreevm = FREEvm;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_ERR;

  if (FREEvm + ARRAY_SIZE(o_2) + 1 > CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_2), FREEvm, ARRAY_SIZE(o_2));
  FREEvm += ARRAY_SIZE(o_2);
  if (FREEvm[-1] != '/') (FREEvm++)[0] = '/';

  if (FREEvm + ARRAY_SIZE(o_1) + 1 > CEILvm) {
    FREEvm = oldfreevm;
    return VM_OVF;
  }
  moveB(VALUE_PTR(o_1), FREEvm, ARRAY_SIZE(o_1));
  FREEvm[ARRAY_SIZE(o_1)] = '\000';
  FREEvm = oldfreevm;
  
  while ((fd = open((char*)FREEvm, O_CREAT | O_RDWR | O_TRUNC,
		    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH))
	 == -1)
    if (errno == EINTR) checkabort();
    else return -errno;
  p = (B *)VALUE_BASE(o_3); atmost = ARRAY_SIZE(o_3);

  while (atmost) {
    while ((nb = write(fd, p, atmost)) == -1)
      if (errno == EINTR) checkabort();
      else return -errno;
    if (nb < atmost) checkabort();
    atmost -= nb;
    p += nb;
  }

  while (close(fd) == -1)
    if (errno == EINTR) checkabort();
    else return -errno;

  FREEopds = o_3;
  return OK;
}

/*---------------------------------------------------- readboxfile
   dir filename / fd | root

  - reads the contents of the file specified by the strings 'dir' and
    'filename' into VM
  - unfolds the tree of objects in the box
  - pushes root object of the tree on operand stack
*/

DM_INLINE_STATIC P closebox(int fd) {
  while (close(fd) == -1)
    if (errno == EINTR) checkabort();
    else return -errno;
  
  return OK;
}

DM_INLINE_STATIC P readbox(int fd, B* p, size_t atmost, size_t* nread) {
  ssize_t nb;
  B* s = p;
  do {
    while ((nb = read(fd, p, atmost)) == -1)
      if (errno == EINTR) checkabort();
      else return -errno;
    if (nb < atmost) checkabort();
    p += nb;
    atmost -= nb;
  } while (atmost && nb);
  
  *nread = p - s;
  return OK;
}

P op_readboxfile(void)
{
  int fd;
  size_t nread;
  P retc;
  B isnonnative;
  B* curr;
  static B sha1[FRAMEBYTES+20];
  B *esha1;
  BOOLEAN closefd;
  B* opd;
#if ENABLE_OPENSSL
  static B rsha1[20];
#endif //ENABLE_OPENSSL

  if (o_1 < FLOORopds) return OPDS_UNF;
  switch (TAG(o_1)) {
    case STREAM:
      closefd = FALSE;
      opd = o_1;

      if ((fd = STREAM_FD(VALUE_PTR(o_1))) == -1)
	return STREAM_CLOSED;
      break;

    case ARRAY|BYTETYPE:
      if (o_1 < FLOORopds) return OPDS_UNF;
      if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;

      closefd = TRUE;
      curr = FREEvm;
      opd = o_2;

      if (curr + ARRAY_SIZE(o_2) + 1 > CEILvm) return VM_OVF;
      moveB(VALUE_PTR(o_2), curr, ARRAY_SIZE(o_2));
      curr += ARRAY_SIZE(o_2);
      if (ARRAY_SIZE(o_2) && curr[-1] != '/')
	(curr++)[0] = '/';

      if (curr + ARRAY_SIZE(o_1) + 1 > CEILvm)
	return VM_OVF;
      moveB(VALUE_PTR(o_1), curr, ARRAY_SIZE(o_1));
      curr[ARRAY_SIZE(o_1)] = '\0';

      while ((fd = open((char*)FREEvm, O_RDONLY)) == -1)
	if (errno == EINTR) checkabort();
	else return -errno;
      break;

    default:
      return OPD_ERR;
  };

  curr = FREEvm;
  if ((retc = readbox(fd, sha1, FRAMEBYTES, &nread))) goto fderr;
  if (nread != FRAMEBYTES) {
    retc = BADBOX;
    goto fderr;
  };

  if (! GETSHA1(sha1)) {
    if (FREEvm + FRAMEBYTES > CEILvm) {
      retc = VM_OVF;
      goto fderr;
    }
    moveframe(sha1, FREEvm);
    FREEvm += FRAMEBYTES;
    esha1 = NULL;
  }
  else {
#if ! ENABLE_OPENSSL
    retc = NEED_SSL;
    goto fderr;
#else
    esha1 = sha1 + FRAMEBYTES;
    if ((retc = readbox(fd, esha1, 20, &nread))) goto fderr;
    if (nread != 20) {
      retc = BADBOX;
      goto fderr;
    };
    //if ((retc = deendian_frame(sha1, isnnonnative))) goto fderr; //not needed
#endif // ! ENABLE_OPENSSL
  };
  
  if ((retc = readbox(fd, FREEvm, CEILvm - FREEvm, &nread))) goto fderr;
  if (nread == CEILvm - FREEvm) {
    retc = VM_OVF;
    goto fderr;
  };
  if (closefd && (retc = closebox(fd))) return retc;

#if ENABLE_OPENSSL
  if (esha1) {
    SHA1((unsigned char*) FREEvm, nread, (unsigned char*) rsha1);
    if (memcmp(esha1, rsha1, 20)) {
      retc = BADBOX;
      goto err;
    }
  }
#endif // ENABLE_OPENSSL

  FREEvm += DALIGN(nread);
  if (! GETNATIVEFORMAT(curr) || ! GETNATIVEUNDEF(curr)) {
    retc = BAD_FMT;
    goto err;
  }
  isnonnative = GETNONNATIVE(curr);
  if ((retc = deendian_frame(curr, isnonnative))) goto err;
  if ((retc = unfoldobj(curr, (P) curr, isnonnative))) goto err;

  FORMAT(curr) = 0;
  moveframe(curr, opd);
  FREEopds = opd + FRAMEBYTES;
  return OK;

 fderr:
  if (closefd) closebox(fd);
 err:
  FREEvm = curr;
  return retc;
}

/*----------------------------------------------- umask
  new-mask | old-mask
  set umask for the machine, and return the previous one.
  They are integers from 0..0777 -- see umask posix function.
*/

P op_umask(void) {
  LBIG mask;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (TYPE(o_1) >= SINGLETYPE) return OPD_TYP;
  if (! VALUE(o_1, &mask)) return UNDF_VAL;
  if (mask < 0 || mask > 0777) return RNG_CHK;

  LONGBIG_VAL(o_1) = (LBIG) umask((mode_t) mask);
  TAG(o_1) = NUM|LONGBIGTYPE;

  return OK;
}
  

/*---------------------------------------------------- writeboxfile
   root dir filename | --
   fd filename | --
  
  - the object tree originating from 'root' is folded into a box
  - a file is created to hold the box and the box is written into the file
    specified by the strings 'dir' and 'filename'
*/

DM_INLINE_STATIC P writebox(int fd, B* base, ssize_t atmost) {
  ssize_t nb;
  P retc;
  do {
    while ((nb = write(fd, base, atmost)) == -1)
      if (errno == EINTR) {
	if ((retc = checkabort_())) return retc;
      }
      else return -errno;

    if (nb < atmost && (retc = checkabort_())) return retc;
    base += nb;
    atmost -= nb;
  } while (atmost > 0);

  return OK;
}

P op_writeboxfile(void) 
{
  int fd;
  size_t atmost;

  P retc;
  B *oldFREEvm = FREEvm;
  B *base, *top, *freemem;
  BOOLEAN closefd;
  B* opd;
  B frame[FRAMEBYTES];
#if ENABLE_OPENSSL
  static B sha1[FRAMEBYTES+20];
#endif //ENABLE_OPENSSL

  if (o_1 < FLOORopds) return OPDS_UNF;
  switch (TAG(o_1)) {
    case STREAM:
      closefd = FALSE;
      opd = o_2;

      if ((fd = STREAM_FD(VALUE_PTR(o_1))) == -1)
	return STREAM_CLOSED;
      break;
      
    case ARRAY|BYTETYPE:
      closefd = TRUE;
      opd = o_3;

      if (o_2 < FLOORopds) return OPDS_UNF;
      if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
      break;
      
    default:
      return OPD_ERR;
  }
  if (opd < FLOORopds) return OPDS_UNF;

  switch (TAG(opd)) {
    case ARRAY: case LIST: case DICT: break;
    default: return OPD_ERR;
  }
  moveframe(opd, frame);
  if ((retc = foldobj_ext(frame)) != OK) {
    FREEvm = oldFREEvm;
    return retc;
  }

  freemem = FREEvm;
  if (! foldobj_mem(&base, &top)) {
    base = oldFREEvm;
    top = FREEvm;
    FREEvm = oldFREEvm;
  }

  SETNATIVE(base);
  atmost = top - base;

  if (! closefd) {
    B* oldfreemem = freemem;
    if (freemem + ARRAY_SIZE(o_2) + 1 > CEILvm) {
      retc = VM_OVF;
      goto err;
    }
    moveB(VALUE_PTR(o_2), freemem, ARRAY_SIZE(o_2));
    freemem += ARRAY_SIZE(o_2);
    if (ARRAY_SIZE(o_2) && freemem[-1] != '/') (freemem++)[0] = '/';

    if (freemem + ARRAY_SIZE(o_1) + 1 > CEILvm) {
      retc = VM_OVF;
      goto err;
    }
    moveB(VALUE_PTR(o_1), freemem, ARRAY_SIZE(o_1));
    freemem[ARRAY_SIZE(o_1)] = '\000';

    while ((fd = open((char*) oldfreemem, O_CREAT | O_RDWR | O_TRUNC,
		      S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH))
	   == -1)
      if (errno == EINTR) {
	if ((retc = checkabort_())) goto fderr;
      }
      else {
	retc = -errno; 
	goto fderr;
      }
  }

#if ENABLE_OPENSSL
  TAG(sha1) = ARRAY|BYTETYPE;
  ATTR(sha1) = 0;
  SETNATIVE(sha1);
  SETSHA1(sha1);
  ARRAY_SIZE(sha1) = 20;
  SHA1((unsigned char*) base, atmost, (unsigned char*) (sha1+FRAMEBYTES));

  if ((retc = writebox(fd, sha1, FRAMEBYTES+20))) goto fderr;
#endif // ENABLE_OPENSSL
  if ((retc = writebox(fd, base, atmost))) goto fderr;

  foldobj_free();
  if (closefd && (retc = closebox(fd))) return retc; 
  FREEopds = opd;
  return OK;

 fderr:
  if (closefd) closebox(fd);
 err:
  foldobj_free();
  return retc;
}

/*---------------------------------------------- findfiles
   dir | filelist 

 - the string 'dir' specifies the directory in which the search is
   to be performed
 - filelist is a list of file entries
 - each file entry is a list of the following objects:
    0 - filename (string)
    1 - file size, in bytes
    2 - (compact) datetime last modified
    3 - file attribute bits TTTT---ooogggaaa:
         TTTT = 1100  socket            *
                1010  symbolic link     *
                1000  regular file
                0110  block device      *
                0100  directory
                0010  character device  *
                0001  fifo              *
		* files with this attribute are not included into
                  filelist, but might be in the future
         ooo - owner privileges (read/write/execute)
         ggg - group privileges
         aaa - all privileges

 - directories are listed first in filelist, followed by regular files;
   directories and regular files are sorted alphabetically in each
   of the two subsets     
*/

/*-- compare filelist entries for sorting by dir/reg attribute and
    name
*/

DM_INLINE_STATIC int compare(const void *a, const void *b)
{
  B *fpa, *fpb, *sa, *sb; 
  LBIG ta, tb;
  P na, nb;

  fpa = (B *)(VALUE_PTR((B*)a) + 3 * FRAMEBYTES);
  fpb = (B *)(VALUE_PTR((B*)b) + 3 * FRAMEBYTES);
  VALUE(fpa,&ta); 
  VALUE(fpb,&tb);
  fpa = VALUE_PTR((B*)a); 
  fpb = VALUE_PTR((B*)b);
  sa = VALUE_PTR(fpa); 
  sb = VALUE_PTR(fpb);
  na = ARRAY_SIZE(fpa); 
  nb = ARRAY_SIZE(fpb);

  if (S_ISDIR(ta)) {
    if (S_ISDIR(tb))
      return strncmp((char*)sa,(char*)sb,(na <= nb? na : nb));
    else return -1;
  }
  else {
    if (S_ISDIR(tb)) return 1;
    else return strncmp((char*)sa,(char*)sb,(na <= nb)? na : nb);
  }
}

P op_findfiles(void)
{
  P ndirn, nentries, k, nname; 
  B *dirn, *vmp, *lp, *fnp, *fp;
  struct stat sb;
  struct dirent *ep;
  DIR *dp;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if ((dirn = malloc(1024)) == 0) return MEM_OVF;

  /*-- we need dirname as null-terminated string */
  ndirn = ARRAY_SIZE(o_1);
  if (ndirn > 1023) { free(dirn); return RNG_CHK; }
  moveB((B *)VALUE_BASE(o_1),dirn,ndirn); dirn[ndirn] = '\000';

  /*-- open the directory */
  if ((dp = opendir((char*)dirn)) == NULL) { free(dirn); return -errno; }
  vmp = FREEvm;

  /*-- scan file by file and save filenames and status info in VM
    skipping files other than regular files or directories
  */
  nentries = 0;
  while ((ep = readdir(dp))) {
    nname = strlen(ep->d_name);
    if ((ndirn + nname) > 1023) {closedir(dp); free(dirn); return RNG_CHK;}
    moveB((B*)ep->d_name, dirn+ndirn, nname); dirn[ndirn + nname] = '\000'; 
    
    if (stat((char*)dirn, &sb) == 0 
        && ((S_ISDIR(sb.st_mode) || (S_ISREG(sb.st_mode))))) {
      if ((vmp + 6 * FRAMEBYTES + DALIGN(nname)) > CEILvm) { 
        closedir(dp); free(dirn); return VM_OVF; 
      }
      TAG(vmp) = LIST; 
      ATTR(vmp) = 0;
      lp = (B*)(VALUE_BASE(vmp) = (P)(vmp + FRAMEBYTES));
      fnp = (B*)(LIST_CEIL(vmp) = (P)(lp + 4 * FRAMEBYTES));

      TAG(lp) = (ARRAY | BYTETYPE); 
      ATTR(lp) = 0;
      VALUE_BASE(lp) = (P)(fnp + FRAMEBYTES);
      ARRAY_SIZE(lp) = nname;
      moveB((B*)ep->d_name, (B *)VALUE_BASE(lp), ARRAY_SIZE(lp));
      moveframe(lp,fnp);
      
      lp += FRAMEBYTES;
      TAG(lp) = (NUM | LONGBIGTYPE); 
      ATTR(lp) = 0;
      LONGBIG_VAL(lp) = sb.st_size;
      lp += FRAMEBYTES;
      TAG(lp) = (NUM | LONGBIGTYPE); 
      ATTR(lp) = 0;
      LONGBIG_VAL(lp) = sb.st_mtime;
      lp += FRAMEBYTES;
      TAG(lp) = (NUM | LONGBIGTYPE); 
      ATTR(lp) = 0;
      LONGBIG_VAL(lp) = sb.st_mode;
      vmp = fnp + FRAMEBYTES + DALIGN(nname);
      nentries++;
    }
  }

  if (closedir(dp) != 0) return -errno;
  /*-- build the 'filelist' master list */
  if ((vmp + (nentries + 1) * FRAMEBYTES) > CEILvm) { 
    free(dirn); return VM_OVF; 
  }
  TAG(vmp) = LIST; 
  ATTR(vmp) = READONLY;
  VALUE_BASE(vmp) = (P)(vmp + FRAMEBYTES);
  LIST_CEIL(vmp) = (P)(vmp + (nentries + 1) * FRAMEBYTES);
  lp = vmp + FRAMEBYTES; fp = FREEvm;
  for (k=0; k<nentries; k++) {
    moveframe(fp,lp);
    fp += 5 * FRAMEBYTES;
    fp += FRAMEBYTES + DALIGN(ARRAY_SIZE(fp));
    lp += FRAMEBYTES;
  }

  /*-- sort into directories and regular files */

  qsort((void *)VALUE_BASE(vmp), nentries, FRAMEBYTES, compare);
 
  FREEvm = lp;    
  moveframe(vmp, o_1);
  free(dirn);
  return OK;
}

/**************************************************** findfile
 *
 * (dir) (file) | false : filesize datetime attribute-bits true
 *
 * see findfiles for description of output
 *
 **************************************************************
*/

P op_findfile(void) 
{
  struct stat buf;
  B addslash;
  P dirlen;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((TAG(o_1) != (ARRAY | BYTETYPE)) || (TAG(o_2) != (ARRAY | BYTETYPE)))
    return OPD_ERR;
  if (! ARRAY_SIZE(o_2))
    return RNG_CHK;

  dirlen = ARRAY_SIZE(o_2);
  addslash = ((VALUE_PTR(o_2)[ARRAY_SIZE(o_2)-1] != '/') 
	      && ARRAY_SIZE(o_1)) 
    ? 1 : 0;
  // make null terminated file string
  if (ARRAY_SIZE(o_1) + ARRAY_SIZE(o_2) + 1 + addslash >= CEILvm - FREEvm)
    return VM_OVF;

  moveB(VALUE_PTR(o_2), FREEvm, ARRAY_SIZE(o_2));
  if (addslash) FREEvm[dirlen++] = '/';
  moveB(VALUE_PTR(o_1), FREEvm + dirlen, ARRAY_SIZE(o_1));
  FREEvm[dirlen+ARRAY_SIZE(o_1)] = '\0';

  if (stat((char*)FREEvm, &buf)) {
    if (errno == ENOENT) { // non-existent returns false
      TAG(o_2) = BOOL;
      ATTR(o_2) = 0;
      BOOL_VAL(o_2) = FALSE;
      FREEopds = o_1;
      return OK;
    }
    return -errno;
  }

  // unhandled type return false
  if (! S_ISDIR(buf.st_mode) && ! S_ISREG(buf.st_mode)) {
    TAG(o_2) = BOOL; ATTR(o_2) = 0;
    BOOL_VAL(o_2) = FALSE;
    FREEopds = o_1;
    return OK;
  }

  if (o3 > CEILopds) return OPDS_OVF;
  TAG(o_2) = (NUM | LONGBIGTYPE); 
  ATTR(o_2) = 0;
  LONGBIG_VAL(o_2) = buf.st_size;
  TAG(o_1) = (NUM | LONGBIGTYPE); 
  ATTR(o_1) = 0;
  LONGBIG_VAL(o_1) = buf.st_mtime;
  TAG(o1) = (NUM | LONGBIGTYPE); 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = buf.st_mode;
  TAG(o2) = BOOL; 
  ATTR(o2) = 0;
  BOOL_VAL(o2) = TRUE;
  FREEopds = o3;

  return OK;
}

/*------------------------------------- transcribe
   original | replica

 - creates a replica of the given original object
 - applies itself recursively to objects nested in the original,
   thus replicating the object tree; the root object is returned
   as the replica
*/

P op_transcribe(void)
{
  P retc; 
  B *p;

  if (o_1 < FLOORopds) return OPDS_UNF;
  switch (CLASS(o_1)) {
    case ARRAY: case LIST: case DICT: break;
    default: return OPD_ERR;
  };

  p = FREEvm;
  if ((retc = transcribe(o_1)) != OK) {FREEvm = p; return retc;}

  moveframe(p, o_1);
  return OK;
}

/*------------------ tostderr -----------------------
  (string) | --
  prints string out to stderr
*/

P op_tostderr(void) {
  B *p;
  P nb, atmost;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

  p = VALUE_PTR(o_1);
  atmost = ARRAY_SIZE(o_1);
  while (atmost > 0) {
    while ((nb = write(DM_STDERR_FILENO, p, atmost)) < 0)
      if (errno == EINTR) checkabort();
      else return -errno;
    if (nb < atmost) checkabort();
    atmost -= nb;
    p += nb;
  }

  FREEopds = o_1;
  return OK;
}
