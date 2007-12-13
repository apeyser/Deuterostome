

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

#define _GNU_SOURCE

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
#include <math.h>
#include <sys/wait.h>
#include <signal.h>
#include "dm.h"

/*--------------------------------------------------- gettime
   -- | time

 - returns compacted time as long numeral (seconds since something)
 - use 'localtime' to convert into date and time
*/

P op_gettime(void)
{
  time_t t;
  if (o1 > CEILopds) return OPDS_OVF;
  
  if (((t = time(NULL)) == -1) && errno)
    return CLOCK_ERR;

  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (LBIG) t;
  FREEopds = o2;
  return OK;
}

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

/*---------------------------------------------------- gwdir
   string | substring

  - returns in 'substring' of 'string' the absolute filename of
    the current working directory
*/

P op_getwdir(void)
{
  B *p; 
  P nb;

  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  
  if ((p = (B*)getcwd(0L,0L)) == 0L) return -errno;
  nb = strlen((char*)p);
  if (nb > ARRAY_SIZE(o_1)) return RNG_CHK;
  moveB(p, (B *)VALUE_BASE(o_1),nb);
  free(p);
  ARRAY_SIZE(o_1) = nb;
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
  nb = ARRAY_SIZE(o_1) + 1;
  if (nb > (CEILvm - FREEvm)) return VM_OVF;

  moveB((B *)VALUE_BASE(o_1),FREEvm, nb-1);
  FREEvm[nb-1] = '\000';
  if (chdir((char*)FREEvm)) return -errno;
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- tosystem
     string | --

  - executes 'string' as a bash shell command
*/

P op_tosystem(void)
{
  P nb;
  pid_t f, r;
  int status;
	
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  nb = ARRAY_SIZE(o_1) + 1;
  if (nb > (CEILvm - FREEvm)) return VM_OVF;
  moveB((B *)VALUE_BASE(o_1),FREEvm, nb-1);
  FREEvm[nb-1] = '\000';

	if ((f = fork()) == -1) return -errno;
	if (! f) {
    int retc2;
    while ((retc2 = open("/dev/null", O_RDWR, 0)) == -1 && errno == EINTR);
    if (retc2 == -1) {
      perror("Error opening /dev/null");
      exit(-1);
    }
			
    while ((status = dup2(retc2, STDIN_FILENO)) == -1 && errno == EINTR);
    if (status == -1) {
      perror("Error opening stdin into /dev/null");
      exit(-1);
    }
			
    while ((status = dup2(retc2, STDOUT_FILENO)) == -1 && errno == EINTR);
    if (status == -1) {
      perror("Error opening stdout into /dev/null");
      exit(-1);
    }

    execl(ENABLE_BASH, ENABLE_BASH, "-c", FREEvm, (char*) NULL);
    perror("Error exec'ing bash");
    exit(-1);
	}
  
 wts:
  if (abortflag) {
    alarm(0);
    kill(f, SIGKILL);
    return ABORT;
	};
  alarm(30);
  if ((r = waitpid(f, &status, 0)) == -1) {
    int errno_;
    if (errno == EINTR) goto wts;
    errno_ = errno;
    kill(f, SIGKILL);
    return -errno_;
  }
	else if (r != f) goto wts;
  if (status != 0) return NOSYSTEM;
	
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- fromsystem
 * string | string
 *
 * - executes 'string' as a bash shell command, and returns the output
 * - the output is returned as a new string - remember to put it in a
 * - box.
 */

P op_fromsystem(void) 
{
	P max, retc;
	B* c;
	pid_t f, r;
	ssize_t rf;
	int status;
	int fd[2];
	
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  max = ARRAY_SIZE(o_1) + 1;
  if (max > (CEILvm - FREEvm)) return VM_OVF;
  moveB((B *)VALUE_BASE(o_1),FREEvm, max-1);
  FREEvm[max-1] = '\000';

	if (pipe(fd)) return -errno;
	
	if ((f = fork()) == -1) {
    retc = -errno;
    close(fd[1]);
    close(fd[0]);
    return retc;
	}
	
	if (! f) {
    int retc2;
    while ((retc2 = open("/dev/null", O_RDWR, 0)) == -1 && errno == EINTR);
    if (retc2 == -1) {
      perror("Error opening /dev/null");
      exit(-1);
    }
		
    while ((status = dup2(retc2, STDIN_FILENO)) == -1 && errno == EINTR);
    if (status == -1) {
      perror("Error opening stdin into /dev/null");
      exit(-1);
    }
    
    while ((status = close(fd[0]))  && errno == EINTR);
    if (status) {
      perror("Error closing pipe in");
      exit(-1);
    }
		
    while ((status = dup2(fd[1], STDOUT_FILENO) == -1) && errno == EINTR);
    if (status == -1) perror("Error duping pipe out to stdout");
		
    while ((status = close(fd[1])) && errno == EINTR);
    if (status) {
      perror("Error closing pipe out");
      exit(-1);
    }
		
    execl(ENABLE_BASH, ENABLE_BASH, "-c", FREEvm, (char*) NULL);
    perror("Error exec'ing bash");
    exit(-1);
	}

	alarm(30);
	while ((status = close(fd[1])) == -1 && errno == EINTR) {
    if (abortflag) {
      retc = ABORT;
      goto EXIT_FILE;
    }
	}
	
	if (status) {
    retc = -errno;
    goto EXIT_FILE;
	}

	if (FREEvm + FRAMEBYTES >= CEILvm) {
    retc = VM_OVF;
    goto EXIT_FILE;
	}
	
	max = CEILvm - FREEvm - FRAMEBYTES;
	TAG(FREEvm) = ARRAY | BYTETYPE;
	ATTR(FREEvm) = PARENT;
	c = VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;

 READ:
	alarm(30);
	while ((rf = read(fd[0], c, max)) > 0) {
    c += rf;
    if ((max -= rf) == 0) {
      char c_;
      if ((rf = read(fd[0], &c_, 1)) != 0) {
        retc = VM_OVF;
        goto EXIT_FILE;
      }
      break;
    }
	}
	
	if (rf == -1) {
    if (abortflag) {
      retc = ABORT;
      goto EXIT_FILE;
    }
			
    if (errno == EINTR) goto READ;
    retc = -errno;
    goto EXIT_FILE;
	}
	
	if (close(fd[0])) {
    retc = -errno;
    goto EXIT_PID;
	}
  
 WAIT_PID:
	alarm(0);
  if (abortflag) {
    retc = ABORT;
    goto EXIT_PID;
	};
	
	for (alarm(30); (r = waitpid(f, &status, 0)) != f; alarm(30)) {
    if (r == -1) {
      if (errno == EINTR) goto WAIT_PID;
      retc = -errno;
      goto EXIT_PID;
    }
	}

	if (status != 0) {
    retc = NOSYSTEM;
    goto EXIT_NOW;
	}
  
	max = c - FREEvm - FRAMEBYTES;
	if ((c = (B*) DALIGN(FREEvm + FRAMEBYTES + max)) > CEILvm) {
    retc = VM_OVF;
    goto EXIT_NOW;
	}
	ARRAY_SIZE(FREEvm) = c - FREEvm - FRAMEBYTES;
	moveframe(FREEvm, o_1);
	ARRAY_SIZE(o_1) = max;
	FREEvm = c;
	return OK;

 EXIT_FILE:
	close(fd[0]);
 EXIT_PID:
	kill(f, SIGKILL);
 EXIT_NOW:
	return retc;
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
  P nb, atmost, npath;
  B *p;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_ERR;
  npath = ARRAY_SIZE(o_3) + ARRAY_SIZE(o_2) + 1;
  if (FREEvm + npath > CEILvm) return(VM_OVF);
  moveB((B *)VALUE_BASE(o_3), FREEvm, ARRAY_SIZE(o_3));
  moveB((B *)VALUE_BASE(o_2), FREEvm + ARRAY_SIZE(o_3), ARRAY_SIZE(o_2));
  FREEvm[npath-1] = '\000';

  alarm(30);
  timeout = FALSE;
 rf1:
  if (timeout) return TIMER; 
  if (abortflag) return ABORT;
  fd = open((char*)FREEvm, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    if ((errno == EINTR) || (errno == EAGAIN))
      goto rf1; 
    else return -errno;
  }
  p = (B *)VALUE_BASE(o_1); atmost = ARRAY_SIZE(o_1);
 rf2:
  if (timeout) return TIMER; 
  if (abortflag) return ABORT;
  nb = read(fd, p, atmost);
  if (nb == -1) {
    if ((errno == EAGAIN) || (errno == EINTR)) goto rf2;
    else return -errno;
  }
  if (nb == 0) goto rf3;
  p += nb; atmost -= nb;
  if (atmost == 0) return RNG_CHK;
  goto rf2;
 rf3:
  if (timeout) return TIMER; 
  if (abortflag) return ABORT;
  if (close(fd) == -1) {
    if (errno == EINTR) goto rf3; 
    else return -errno;
  }
 
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
  P nb, atmost, npath;
  B *p;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_ERR;
  npath = ARRAY_SIZE(o_2) + ARRAY_SIZE(o_1) + 1;
  if (FREEvm + npath > CEILvm) return VM_OVF;
  moveB((B *)VALUE_BASE(o_2), FREEvm, ARRAY_SIZE(o_2));
  moveB((B *)VALUE_BASE(o_1), FREEvm + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
  FREEvm[npath-1] = '\000';
  
  alarm(30);
  timeout = FALSE;
 wf1:
  if (timeout) return TIMER; 
  if (abortflag) return ABORT;
  fd = open((char*)FREEvm, O_CREAT | O_RDWR | O_TRUNC | O_NONBLOCK,
            S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
  if (fd == -1) {
    if ((errno == EINTR) || (errno == EAGAIN)) goto wf1; 
    else return -errno;
  }
  p = (B *)VALUE_BASE(o_3); atmost = ARRAY_SIZE(o_3);
 wf2:
  if (timeout) return TIMER; 
  if (abortflag) return ABORT;
  nb = write(fd, p, atmost);
  if (nb == -1) {
    if ((errno == EAGAIN) || (errno == EINTR)) goto wf2;
    else return -errno;
  }
  p += nb; atmost -= nb;
  if (atmost > 0) goto wf2;
 wf3:
  if (timeout) return TIMER; 
  if (abortflag) return ABORT;
  if (close(fd) == -1) {
    if (errno == EINTR) goto wf3; 
    else return -errno;
  }
  FREEopds = o_3;
  return OK;
}

/******************************************* some funcs for read/writeboxfile
 * double time loops: 10 seconds for single operations
 *                    and 3 minutes for the full thing.
 */

static clock_t endclock;
static P chunk_size;

DM_INLINE_STATIC void START_ALARM(void) {
		endclock = clock() + 180*CLOCKS_PER_SEC;
		timeout = FALSE;
}

#define MAX_CHUNK (32000)
//100mbit/s*1/8mbyte/mbit*1024byte/mbyte*5s*1/2minrate*/

DM_INLINE_STATIC P CHECK_ALARM(void) {
  int timeout_;
  alarm(0);
  
  timeout_ = timeout;
  timeout = FALSE;
  if (clock() > endclock || timeout_) return TIMER;
  if (abortflag) return ABORT;
	
  alarm(10);
  return OK;
}

DM_INLINE_STATIC void END_ALARM(void) {
  alarm(0);
  timeout = FALSE;
}

/*---------------------------------------------------- readboxfile
   dir filename | root

  - reads the contents of the file specified by the strings 'dir' and
    'filename' into VM
  - unfolds the tree of objects in the box
  - pushes root object of the tree on operand stack
*/

P op_readboxfile(void)
{
  int fd;
  P nb, atmost, npath, retc;
  B *p; B isnonnative;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  npath = ARRAY_SIZE(o_1) + ARRAY_SIZE(o_2) + 1;
  if (FREEvm + npath > CEILvm) return VM_OVF;

  moveB((B *)VALUE_BASE(o_2), FREEvm, ARRAY_SIZE(o_2));
  moveB((B *)VALUE_BASE(o_1), FREEvm + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
  FREEvm[npath-1] = '\000';
  atmost = CEILvm - FREEvm;   
  START_ALARM();
 rb1:
  if ((retc = CHECK_ALARM()) != OK) return retc;
  fd = open((char*)FREEvm, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    if ((errno == EINTR) || (errno == EAGAIN)) goto rb1; 
    else {retc = -errno; END_ALARM(); return retc;};
  }
  p = FREEvm; 

 rb2:
  if ((retc = CHECK_ALARM()) != OK) return retc;
  chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost;
  nb = read(fd, p, chunk_size);
  if (nb == -1) {
    if ((errno == EAGAIN) || (errno == EINTR)) goto rb2;
    else {retc = -errno; END_ALARM(); return retc;};
  }
  if (nb == 0) goto rb3;
  p += nb; atmost -= nb;
  if (atmost == 0) {END_ALARM(); return VM_OVF;};
  goto rb2;
 
 rb3:
  if ((retc = CHECK_ALARM()) != OK) return retc;
  if (close(fd) == -1) {
    if ((errno == EINTR) || (errno == EAGAIN)) goto rb3;
    else return -errno;
  }
  END_ALARM();
 
  nb = DALIGN(p - FREEvm);
  if (! GETNATIVEFORMAT(FREEvm) || ! GETNATIVEUNDEF(FREEvm)) return BAD_FMT;
  isnonnative = GETNATIVEENDIAN(FREEvm);
  if ((retc = deendian_frame(FREEvm, isnonnative)) != OK) return retc;
  if ((retc = unfoldobj(FREEvm, (P)FREEvm, isnonnative)) != OK) return retc;
  FORMAT(FREEvm) = 0;
  moveframe(FREEvm,o_2);
  FREEvm += nb;
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- writeboxfile
   root dir filename | --
  
  - the object tree originating from 'root' is folded into a box
  - a file is created to hold the box and the box is written into the file
    specified by the strings 'dir' and 'filename'
*/

P op_writeboxfile(void) 
{
  int fd;
  P nb, atmost, retc, npath;
  B *oldFREEvm, *base, *top, *freemem;
  B frame[FRAMEBYTES];

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (!((CLASS(o_3) == ARRAY)
        || (CLASS(o_3) == LIST)
        || (CLASS(o_3) == DICT)))
    return OPD_ERR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

  oldFREEvm = FREEvm;
  moveframe(o_3, frame);
		
  npath = ARRAY_SIZE(o_2) + ARRAY_SIZE(o_1) + 1;
  if ((retc = foldobj_ext(frame, npath)) != OK) {
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
  
  if (freemem + npath > CEILvm) {foldobj_free(); return VM_OVF;}
  moveB((B *)VALUE_BASE(o_2), freemem, ARRAY_SIZE(o_2));
  moveB((B *)VALUE_BASE(o_1), freemem + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
  freemem[npath-1] = '\000';

  START_ALARM();
 wb1:
  if ((retc = CHECK_ALARM()) != OK) {foldobj_free(); return retc;};
  fd = open((char*)freemem, O_CREAT | O_NONBLOCK | O_RDWR | O_TRUNC,
            S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
  if (fd == -1) {
    if ((errno == EINTR) || (errno == EAGAIN)) goto wb1;
    else {retc = -errno; END_ALARM(); foldobj_free(); return retc;};
  }
  
 wb2:
  if ((retc = CHECK_ALARM()) != OK) {foldobj_free(); return retc;};
  chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost;
  nb = write(fd, base, chunk_size);
  if (nb == -1) {
    if ((errno == EAGAIN) || (errno == EINTR)) goto wb2;
    else {retc = -errno; END_ALARM(); foldobj_free(); return retc;};
  }
 
  base += nb; atmost -= nb;
  if (atmost > 0) goto wb2;
		
  foldobj_free();
 wb3:
  if ((retc = CHECK_ALARM()) != OK) return retc;
  if (close(fd) == -1) {
    if (errno == EINTR) goto wb3;
    else return -errno;
  }
  END_ALARM();
 
  FREEopds = o_3;
  return OK;
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

  fpa = (B *)(VALUE_BASE(a) + 3 * FRAMEBYTES);
  fpb = (B *)(VALUE_BASE(b) + 3 * FRAMEBYTES);
  VALUE(fpa,&ta); 
  VALUE(fpb,&tb);
  fpa = (B *)(VALUE_BASE(a)); 
  fpb = (B *)(VALUE_BASE(b));
  sa = (B *)(VALUE_BASE(fpa)); 
  sb = (B *)(VALUE_BASE(fpb));
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
	BOOLEAN addslash;
	P dirlen;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((TAG(o_1) != (ARRAY | BYTETYPE)) || (TAG(o_2) != (ARRAY | BYTETYPE)))
    return OPD_ERR;
	if (ARRAY_SIZE(o_1) == 0 || ARRAY_SIZE(o_2) == 0)
    return RNG_CHK;

	dirlen = ARRAY_SIZE(o_2);
	addslash = (VALUE_PTR(o_2)[dirlen-1] != '/');
  // make null terminated file string
	if (FREEvm+ARRAY_SIZE(o_1)+ARRAY_SIZE(o_2)+1+(addslash ? 1 : 0) >= CEILvm)
    return VM_OVF;

  moveB(VALUE_PTR(o_2), FREEvm, dirlen);
	if (addslash) FREEvm[dirlen++] = '/';
  moveB(VALUE_PTR(o_1), FREEvm + dirlen, ARRAY_SIZE(o_1));
  FREEvm[dirlen+ARRAY_SIZE(o_1)] = '\0';

  if (stat((char*)FREEvm, &buf)) {
    if (errno == ENOENT) { // non-existent returns false
      TAG(o_2) = BOOL; ATTR(o_2) = 0;
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
   thus replicating the object tree; the nes root object is returned
   as the replica
*/

P op_transcribe(void)
{
  P retc; 
  W depth;
  B *p;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (!((CLASS(o_1) == ARRAY) 
        || (CLASS(o_1) == LIST) 
        || (CLASS(o_1) == DICT)))
    return OPD_ERR;

  p = FREEvm; depth = 0;
  if ((retc = foldobj(o_1,0L ,&depth)) != OK) {FREEvm = p; return retc;}

  moveframe(p,o_1);
  return OK;
}

