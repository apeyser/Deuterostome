1000 dict dup begin

/version 1 def

/errsdict
  6 dict dup begin
  /ARGS (argument list must be composed of strings) def
  /WAIT (waitproc call not immediately followed by readproc) def
  /SIZE (argument string must not have a string length of 0) def
  /SIG  (error setting signal handler) def
  /EOF  (unexpected end of file) def
  /DEAD (process has been killed) def
  end
def

/headercode (
#define PROCESS_STATE_BUFFD 1
#define PROCESS_STATE_DEAD 2
) bind def

/makeops {
  [
    [
      /makeproc {(
    pid_t pid;
    B* param;
    int filedes_stdout[2];
    int filedes_stdin[2];
    ssize_t ret;
    int errno_;
    size_t p;
     
    if \(FREEvm + 2*FRAMEBYTES > CEILvm\) return VM_OVF;
    
    if \(o_1 < FLOORopds\) return OPDS_UNF;
    switch \(TAG\(o_1\)\) {
        case STRING: break;
        case LIST:
            if \(\(\(LIST_CEIL\(o_1\)-VALUE_BASE\(o_1\)\)/FRAMEBYTES\) == 0\)
                return RNG_CHK;
            for \(param = VALUE_PTR\(o_1\); 
                 param < \(B*\) LIST_CEIL\(o_1\); 
                 param += FRAMEBYTES\) {
                if \(TAG\(param\) != STRING\)
                    ) /ARGS error_ (;
                if \(ARRAY_SIZE\(param\) == 0\)
                    ) /SIZE error_ (;
            };
            break;
        default:
            return OPD_TYP;
    };

    if \(signal\(SIGPIPE, SIG_IGN\) == SIG_ERR\)
        ) /SIG error_ (;
    
    if \(pipe\(filedes_stdin\) || pipe\(filedes_stdout\)\) {
        errno_ = errno;
        close\(filedes_stdin[0]\);
        close\(filedes_stdin[1]\);
        close\(filedes_stdout[0]\);
        close\(filedes_stdout[1]\);		
        return -errno_;
    }

    if \(fcntl\(filedes_stdin[0], F_SETFD, 0\) == -1
        || fcntl\(filedes_stdout[1], F_SETFD, 0\) == -1
        || fcntl\(filedes_stdin[1], F_SETFD, FD_CLOEXEC\) 
        || fcntl\(filedes_stdout[0], F_SETFD, FD_CLOEXEC\)\) {
        errno_ = errno;
        close\(filedes_stdin[0]\);
        close\(filedes_stdin[1]\);
        close\(filedes_stdout[0]\);
        close\(filedes_stdout[1]\);		
        return -errno_;
    }

    if \(\(pid = fork\(\)\) == -1\) {
        errno_ = errno;
        close\(filedes_stdin[0]\);
        close\(filedes_stdin[1]\);
        close\(filedes_stdout[0]\);
        close\(filedes_stdout[1]\);
        return -errno_;
    }

    if \(! pid\) {
        char* prog;
        char** args;
        char** arg;

        if \(close\(0\) || close\(1\)\) senderrno\(\);
        if \(dup2\(filedes_stdin[0], 0\) == -1 
            || dup2\(filedes_stdout[1], 1\) == -1\) 
            senderrno\(\);
        if \(close\(filedes_stdin[0]\) || close\(filedes_stdout[1]\)\) senderrno\(\);
	
        if \(TAG\(o_1\) == STRING\) {
            if \(! \(prog = malloc\(ARRAY_SIZE\(o_1\)+1\)\)\) senderrno\(\);
            strncpy\(\(char*\)prog, \(char*\)VALUE_PTR\(o_1\), 
	             ARRAY_SIZE\(o_1\)\);
            prog[ARRAY_SIZE\(o_1\)] = '\\0';
            args = NULL;
        } else {
           if \(!\(args = malloc\(1+\(LIST_CEIL\(o_1\)-VALUE_BASE\(o_1\)\)/FRAMEBYTES\)\)\)
                senderrno\(\);
            for \(arg = args, param = VALUE_PTR\(o_1\);
                 param < \(B*\) LIST_CEIL\(o_1\);
                 \(param += FRAMEBYTES\), arg++\) {
                if \(! \(*arg = malloc\(ARRAY_SIZE\(param\)+1\)\)\) senderrno\(\);
                strncpy\(\(char*\)*arg, \(char*\)VALUE_PTR\(param\), 
		         ARRAY_SIZE\(param\)\);
                \(*arg\)[ARRAY_SIZE\(param\)] = '\\0';
            }
            *arg = NULL;
            prog = args[0];
        };
        
        errno_ = 0;
        if \(write\(1, &errno_, sizeof\(errno_\)\) == -1\) exit\(1\);
        
        execve\(prog, args, environ\);
        perror\("process plugin, execve failure in makeproc"\);
        prog = strerror\(errno\);
        write\(1, "process plugin, execve failure in makeproc: ", 
              strlen\("process plugin, execve failure in makeproc: "\)\);
        write\(1, prog, strlen\(prog\)\);
        exit\(127\);
    }

    if \(close\(filedes_stdin[0]\) || close\(filedes_stdout[1]\)\) {
        errno_ = errno;
        close\(filedes_stdin[1]\);
        close\(filedes_stdout[0]\);
        close\(filedes_stdout[1]\);
        kill\(pid, SIGKILL\);
        waitpid\(pid, NULL, 0\);
        return -errno;
    };

    p = 0;
    while \(1\) {
        ret = read\(filedes_stdout[0], \(\(B*\)&errno_\)+p, sizeof\(errno_\)-p\);
        if \(ret == -1\) {
            if \(errno != EINTR\) {
                errno_ = errno;
                close\(filedes_stdin[1]\);
                close\(filedes_stdout[0]\);
                kill\(pid, SIGKILL\);
                waitpid\(pid, NULL, 0\);
                return -errno;
            }
        } 
        else if \(ret == 0\) {
            close\(filedes_stdin[1]\);
            close\(filedes_stdout[0]\);
            kill\(pid, SIGKILL\);
            waitpid\(pid, NULL, 0\);
            ) /EOF error_ (;
        }
        else {
            if \(ret == \(ssize_t\) sizeof\(errno_\) - \(ssize_t\) p\) break;
            p += ret;
        };
    };

) {(
  TAG) handle (= \(NUM | LONGBIGTYPE \); ATTR) handle ( = 0;
  LONGBIG_VAL) handle (= pid;
  ) /PID make_handle (;
  LONGBIG_VAL) handle (= filedes_stdin[1];
  ) /STDIN make_handle (;
  LONGBIG_VAL) handle (= filedes_stdout[1];
  ) /STDOUT make_handle (;
  TAG) handle ( = \(NUM | BYTETYPE \);
  *\(NUM_VAL) handle (\) = 0;
  ) /STATE make_handle (;
  ) /BUFFC make_handle (;
)} 0 null (o_1) build_handle (
    OPAQUE_ACTIVE\(o_1\) = 1;
    return OK;
)
      }
    ][
      /writeproc {
(
	int pipein;
	P p, atmost, ret;
	DECLARE_ALARM;

	if \(o_2 < FLOORopds\) return OPDS_UNF;
	TEST_OPAQUE\(o_1\);
	
	if \(TAG\(o_2\) != STRING\) return OPD_TYP;
	if \(ARRAY_SIZE\(o_2\) == 0\) return RNG_CHK;

	if \() /STATE (o_1) handle ( == PROCESS_STATE_BUFFD\) return OK;

	pipein = ) /STDIN (o_1) handle (;
	p = 0;
	atmost = ARRAY_SIZE\(o_2\);
	START_ALARM;
	while \(1\) {
		CHECK_ALARM;
		ret = write\(pipein, VALUE_PTR\(o_2\)+p, chunk_size\);
		if \(ret == -1\) {
			if \(errno != EINTR\) {
				END_ALARM;
				return -errno;
			}
		} else {
			p += ret;
			if \(\(atmost -= ret\) <= 0\) goto procwrite;
		}
	}
 procwrite:
	END_ALARM;
	FREEopds = o_2;
	return OK;
)
      }
    ][
      /readproc {
(
	P pipeout;
	P index, p, ret, atmost;
	DECLARE_ALARM;
	
	if \(o_4 < FLOORopds\) return OPDS_UNF;
	TEST_OPAQUE\(o_1\);

	if \(CLASS\(o_2\) != NUM\) return OPD_TYP;
	if \(CLASS\(o_3\) != NUM\) return OPD_TYP;
	if \(TAG\(o_4\) != STRING\) return OPD_TYP;

	if \() /STATE (o_1) handle ( == PROCESS_STATE_DEAD\) ) /DEAD error_ (;

	if \(! PVALUE\(o_3, &index\)\) return UNDF_VAL;
	if \(ARRAY_SIZE\(o_4\)-index < 1\) return RNG_CHK;
	if \(! PVALUE\(o_2, &atmost\)\) atmost = ARRAY_SIZE\(o_4\)-index;
	if \(ARRAY_SIZE\(o_4\)-index-atmost < 0\) return RNG_CHK;

	pipeout = ) /STDOUT (o_1) handle (;
	p = index;
	if \() /STATE (o_1) handle ( == PROCESS_STATE_BUFFD\) {
		VALUE_PTR\(o_4\)[p++] = ) /BUFFC (o_1) handle (;
		) /STATE (o_1) handle ( = 0;
	}
	START_ALARM;
	while \(1\) {
		CHECK_ALARM;
		ret = read\(pipeout, VALUE_PTR\(o_4\)+p, chunk_size\);
		switch \(ret\) {
			case 0: goto procread;
			case -1:
				if \(errno != EINTR\) {
					END_ALARM;
					return -errno;
				}
				break;
			default:
				p += ret;
				if \(\(atmost -= ret\) <= 0\) goto procread;
				break;
		};
	};
 procread:
	END_ALARM;
	TAG\(o_3\) = NUM | LONGBIGTYPE;
	LONGBIG_VAL\(o_3\) = index+p;
	FREEopds = o_2;
	return OK;
)
      }
    ][
      /killproc {
(
	pid_t pid;
	int errno_ = 0;

	if \(o_1 < FLOORopds\) return OPDS_UNF;
	TEST_OPAQUE\(o_1\);

	if \() /STATE (o_1) handle ( == PROCESS_STATE_DEAD\) ) /DEAD error_ (;
	if \() /STATE (o_1) handle (== PROCESS_STATE_BUFFD\) ) /WAIT error_ (;

	) /STATE (o_1) handle ( = PROCESS_STATE_DEAD;
	pid = ) /PID (o_1) handle (;
	if \(kill\(pid, SIGTERM\)\) errno_ = errno;
	if \(waitpid\(pid, 0, 0\) == -1 && ! errno_\) errno_ = errno;
	if \(close\() /STDIN (o_1) handle (\) && ! errno_\) errno_ = errno;
	if \(close\() /STDOUT (o_1) handle (\) && ! errno_\) errno_ = errno; 
	if \(errno_\) return -errno_;

	KILL_OPAQUE\(\);
	return OK;
)
      }
    ][
      /liveproc {
(
	pid_t pid;
	int status;
	P ret;
	int errno_ = 0;
	B nextchar;
	P pipeout;

	if \(o_1 < FLOORopds\) return OPDS_UNF;
	TEST_OPAQUE\(o_1\);

	if \() /STATE (o_1) handle ( == PROCESS_STATE_DEAD\) ) /DEAD error_ (;

	if \() /STATE (o_1) handle ( == PROCESS_STATE_BUFFD\) {
		TAG\(o_1\) = BOOL; ATTR\(o_1\) = 0;
		BOOL_VAL\(o_1\) = TRUE;
		return OK;
	}

	pipeout = \(P\)) /STDOUT (o_1) handle (;
	timeout = FALSE;
	alarm\(10\);
	ret = read\(pipeout, &nextchar, 1\);
	alarm\(0\);
	if \(abortflag\) return ABORT;
	if \(timeout\) return TIMER;
	if \(ret == -1\) return -errno;

	if \(\() /STATE (o_1) handle ( = \(ret ? PROCESS_STATE_BUFFD : 0\)\)\) {
		) /BUFFC (o_1) handle ( = nextchar;
		TAG\(o_1\) = BOOL; ATTR\(o_1\) = 0;
		BOOL_VAL\(o_1\) = TRUE;
		return OK;
	}

	pid = \(P\)) /PID (o_1) handle (;
	if \(\(ret = waitpid\(pid, &status, WNOHANG\)\) == -1\) {
		errno_ = errno;
		close\(\(P\)) /STDIN (o_1) handle (\);
		close\(\(P\)) /STDOUT (o_1) handle (\);
		) /STATE (o_1) handle ( = PROCESS_STATE_DEAD;
		kill\(pid, SIGKILL\);
		waitpid\(pid, &status, 0\);
		return -errno_;
	}
	if \(! ret\) {
		TAG\(o_1\) = BOOL; ATTR\(o_1\) = 0;
		BOOL_VAL\(o_1\) = TRUE;
	} else {
		) /STATE (o_1) handle ( = PROCESS_STATE_DEAD;
		if \(close\(\(P\)) /STDIN (o_1) handle (\)\) errno_ = errno;
		if \(close\(\(P\)) /STDOUT (o_1) handle (\) && ! errno_\) errno_ = errno;
		if \(errno_\) return -errno_;
		
		if \(o2 > CEILopds\) return OPDS_OVF;
		ret = WIFEXITED\(status\) ?
                    WEXITSTATUS\(status\) : \(P\) 0xFFFF0000L;
		TAG\(o1\) = BOOL; ATTR\(o1\) = 0;
		BOOL_VAL\(o1\) = FALSE;
		TAG\(o_1\) = NUM | LONGBIGTYPE; ATTR\(o_1\) = 0;
		LONGBIG_VAL\(o_1\) = ret;
		FREEopds = o2;
	}

	return OK;
)
      }
    ][
      /waitproc {
(
	P pipeout;
	P ret;
	B nextchar;

	if \(o_1 < FLOORopds\) return OPDS_UNF;
	TEST_OPAQUE\(o_1\);

	if \() /STATE (o_1) handle ( == PROCESS_STATE_DEAD\) ) /DEAD error_ (;
	if \() /STATE (o_1) handle ( == PROCESS_STATE_BUFFD\) return OK;

	pipeout = \(P\)) /STDOUT (o_1) handle (;
	while \(1\) {
		alarm\(10\);
		timeout = FALSE;
		ret = read\(pipeout, &nextchar, 1\);
		alarm\(0\);
		if \(abortflag\) return ABORT;
		if \(! timeout\) {
			if \(ret == -1\) return -errno;
			else break;
		}
	}
 
	if \(\() /STATE (o_1) handle (= \(ret ? PROCESS_STATE_BUFFD : 0\)\)\)
        ) /BUFFC (o_1) handle ( = nextchar;
	FREEopds = o_1;
	return OK;
)
      }
    ][
      /unwaitproc {
(
	if \(o_1 < FLOORopds\) return OPDS_UNF;
	TEST_OPAQUE\(o_1\);

	if \() /STATE (o_1) handle (== PROCESS_STATE_DEAD\) ) /DEAD error_ (;

	) /STATE (o_1) handle ( = 0;
	FREEopds = o_1;
	return OK;
)
      }
    ]
  ]
} bind def


/makehandles {
  [
    [/PID {(LONGBIG_VAL\() handle (\))}]
    [/STDIN {(LONGBIG_VAL\() handle (\))}]
    [/STDOUT {(LONGBIG_VAL\() handle (\))}]
    [/STATE {(*\(NUM_VAL\() handle (\)\))}]
    [/BUFFC {(*\(NUM_VAL\() handle (\)\))}]
  ]
} bind def
  
  /bodyheaders (
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>
#include <time.h>
#include <errno.h>
#include <sys/wait.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

#if __APPLE__
#include <crt_externs.h>
#define environ \(*_NSGetEnviron\(\)\)
#else
extern char** environ;
#endif
) def

/bodycode 
(
#define DECLARE_ALARM \
  clock_t endclock = clock\(\) + 180*CLOCKS_PER_SEC; \
  P chunk_size

#define START_ALARM {timeout = FALSE;}

#define MAX_CHUNK \(32000\)
//100mbit/s*1/8mbyte/mbit*1024byte/mbyte*5s*1/2minrate*/

#define CHECK_ALARM { \
		alarm\(0\); \
		if \(clock\(\) > endclock\) return TIMER; \
		if \(timeout\) return TIMER; \
		if \(abortflag\) return ABORT; \
    chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost; \
		alarm\(10\); \
		timeout = FALSE; \
	}

#define END_ALARM { \
		alarm\(0\); \
		timeout = FALSE; \
	}

#define senderrno\(\) senderrno_\(__LINE__\)

static void senderrno_\(int line\) {
	fprintf\(stderr, "Error on: %d\\n", line\);
	write\(1, &errno, sizeof\(errno\)\); 
	exit\(1\); 
}
)
def

/all {
  getstartupdir (plugin.d) fromfiles
  PLUGINS begin all end
} bind def
  

end userdict /process2 put
