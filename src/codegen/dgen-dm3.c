#include "dm.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "dgen-dm3.h"
#include "error-local.h"

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
    if (retc2 == -1)
      dm_error(errno, "Error opening /dev/null in tosystem");
    
    while ((status = dup2(retc2, STDIN_FILENO)) == -1 && errno == EINTR);
    if (status == -1)
      dm_error(errno, "Error opening stdin into /dev/null in tosystem");
			
    while ((status = dup2(retc2, STDOUT_FILENO)) == -1 && errno == EINTR);
    if (status == -1)
      dm_error(errno, "Error opening stdout into /dev/null in tosystem");
    
    //fprintf(stderr, "tosystem: '%s' '%s' '%s'\n", ENABLE_BASH, "-c", FREEvm);
    execl(ENABLE_BASH, ENABLE_BASH, "-c", FREEvm, (char*) NULL);
    dm_error(errno, "Error exec'ing bash in tosystem");
  }
  
 wts:
  if (abortflag) {
    kill(f, SIGKILL);
    return ABORT;
  };
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
    if (retc2 == -1) 
      dm_error(errno, "Error opening /dev/null in fromsystem");
		
    while ((status = dup2(retc2, STDIN_FILENO)) == -1 && errno == EINTR);
    if (status == -1) 
      dm_error(errno, "Error opening stdin into /dev/null in fromsystem");
    
    while ((status = close(fd[0]))  && errno == EINTR);
    if (status)
      dm_error(errno, "Error closing pipe in, in fromsystem");
		
    while ((status = dup2(fd[1], STDOUT_FILENO) == -1) && errno == EINTR);
    if (status == -1) 
      dm_error(errno, "Error duping pipe out to stdout in fromsystem");
		
    while ((status = close(fd[1])) && errno == EINTR);
    if (status)
      dm_error(errno, "Error closing pipe out, in fromsystem");
    
    execl(ENABLE_BASH, ENABLE_BASH, "-c", FREEvm, (char*) NULL);
    dm_error(errno, "Error exec'ing bash in fromsystem");
  }

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
  while ((rf = read(fd[0], c, max)) > 0) {
    c += rf;
    if ((max -= rf) == 0) {
      char c_;
      while ((rf = read(fd[0], &c_, 1)) == -1 && errno == EINTR) {
        if (abortflag) {
          retc = ABORT;
          goto EXIT_FILE;
        }
      }
      if (rf == -1) {
        retc = -errno;
        goto EXIT_FILE;
      } 
      if (rf > 0) {
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
  if (abortflag) {
    retc = ABORT;
    goto EXIT_PID;
  };
	
  while ((r = waitpid(f, &status, 0)) != f) {
    if (r == -1) {
      if (errno == EINTR) {
        if (abortflag) {
          retc = ABORT;
          goto EXIT_PID;
        }
        goto WAIT_PID;
      }
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
