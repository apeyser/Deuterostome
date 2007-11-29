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
#define environ (*_NSGetEnviron())
#else
extern char** environ;
#endif

#include "process.h"

UP ll_type = 0;
P op_hi(void) {return wrap_hi("process V1");}
P op_libnum(void) {return wrap_libnum(ll_type);}
P ll_errc[] = {
	PROC_ARGS, PROC_WAIT, PROC_SIZE, PROC_SIG, PROC_EOF, PROC_DEAD, 0L
};
B* ll_errm[] = {
	"** process: argument list must be composed of strings",
	"** process: waitproc call not immediately followed bre readproc",
	"** process: argument string must not have a string length of 0",
	"** process: error setting signal handler",
	"** process: unexpected end of file",
	"** process: process has been killed",
	NULL
};

B* ll_export[] = {
	"hi", (B*) op_hi,
	"libnum", (B*) op_libnum,
	"makeproc", (B*) op_makeproc,
	"writeproc", (B*) op_writeproc,
	"readproc", (B*) op_readproc,
	"killproc", (B*) op_killproc,
	"waitproc", (B*) op_waitproc,
	"unwaitproc", (B*) op_unwaitproc,
	"liveproc", (B*) op_liveproc,
	"INIT_", (B*) op_INIT_,
	"", NULL
};

B opaquename[FRAMEBYTES];
static B PROCESS_PID_N[FRAMEBYTES];
static B PROCESS_STDOUT_N[FRAMEBYTES];
static B PROCESS_STDIN_N[FRAMEBYTES];
static B PROCESS_STATE_N[FRAMEBYTES];
static B PROCESS_BUFFC_N[FRAMEBYTES];

P op_INIT_(void) {
  makename(PROCESS_HANDLE, opaquename);
  makename("pid", PROCESS_PID_N);
  makename("stdout", PROCESS_STDOUT_N);
  makename("stdin", PROCESS_STDIN_N);
  makename("state", PROCESS_STATE_N);
  makename("buffc", PROCESS_BUFFC_N);

  return OK;
}

#define DECLARE_ALARM \
  clock_t endclock = clock() + 180*CLOCKS_PER_SEC; \
  P chunk_size

#define START_ALARM {timeout = FALSE;}

#define MAX_CHUNK (32000)
//100mbit/s*1/8mbyte/mbit*1024byte/mbyte*5s*1/2minrate*/

#define CHECK_ALARM {																			\
		alarm(0);																							\
		if (clock() > endclock) return TIMER;									\
		if (timeout) return TIMER;														\
		if (abortflag) return ABORT;													\
    chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost; \
		alarm(10);																						\
		timeout = FALSE;																			\
	}

#define END_ALARM {															\
		alarm(0);																		\
		timeout = FALSE;														\
	}

P op_writeproc(void) {
	P pipein;
	P p, atmost, ret;
	DECLARE_ALARM;

	if (o_2 < FLOORopds) return OPDS_UNF;
	TEST_OPAQUE(o_1);
	
	if (TAG(o_2) != STRING) return OPD_TYP;
	if (ARRAY_SIZE(o_2) == 0) return RNG_CHK;

	if (PROCESS_STATE(o_1) == PROCESS_BUFFD) return OK;

	pipein = (P)PROCESS_STDIN(o_1);
	p = 0;
	atmost = ARRAY_SIZE(o_2);
	START_ALARM;
	while (1) {
		CHECK_ALARM;
		ret = write(pipein, VALUE_PTR(o_2)+p, chunk_size);
		if (ret == -1) {
			if (errno != EINTR) {
				END_ALARM;
				return -errno;
			}
		} else {
			p += ret;
			if ((atmost -= ret) <= 0) goto procwrite;
		}
	}
 procwrite:
	END_ALARM;
	FREEopds = o_2;
	return OK;
}

P op_waitproc(void) {
	P pipeout;
	P ret;
	B nextchar;

	if (o_1 < FLOORopds) return OPDS_UNF;
	TEST_OPAQUE(o_1);

	if (PROCESS_STATE(o_1) == PROCESS_DEAD) RETURN_ERROR(PROC_DEAD);
	if (PROCESS_STATE(o_1) == PROCESS_BUFFD) return OK;

	pipeout = (P)PROCESS_STDOUT(o_1);
	while (1) {
		alarm(10);
		timeout = FALSE;
		ret = read(pipeout, &nextchar, 1);
		alarm(0);
		if (abortflag) return ABORT;
		if (! timeout) {
			if (ret == -1) return -errno;
			else break;
		}
	}
 
	if ((PROCESS_STATE(o_1) = (ret ? PROCESS_BUFFD : 0)))
            PROCESS_BUFFC(o_1) = nextchar;
	FREEopds = o_1;
	return OK;
}

P op_readproc(void) {
	P pipeout;
	P index, p, ret, atmost;
	DECLARE_ALARM;
	
	if (o_4 < FLOORopds) return OPDS_UNF;
	TEST_OPAQUE(o_1);

	if (CLASS(o_2) != NUM) return OPD_TYP;
	if (CLASS(o_3) != NUM) return OPD_TYP;
	if (TAG(o_4) != STRING) return OPD_TYP;

	if (PROCESS_STATE(o_1) == PROCESS_DEAD) RETURN_ERROR(PROC_DEAD);	

	if (! PVALUE(o_3, &index)) return UNDF_VAL;
	if (ARRAY_SIZE(o_4)-index < 1) return RNG_CHK;
	if (! PVALUE(o_2, &atmost)) atmost = ARRAY_SIZE(o_4)-index;
	if (ARRAY_SIZE(o_4)-index-atmost < 0) return RNG_CHK;

	pipeout = (P)PROCESS_STDOUT(o_1);
	p = index;
	if (PROCESS_STATE(o_1) == PROCESS_BUFFD) {
		VALUE_PTR(o_4)[p++] = PROCESS_BUFFC(o_1);
		PROCESS_STATE(o_1) = 0;
	}
	START_ALARM;
	while (1) {
		CHECK_ALARM;
		ret = read(pipeout, VALUE_PTR(o_4)+p, chunk_size);
		switch (ret) {
			case 0: goto procread;
			case -1:
				if (errno != EINTR) {
					END_ALARM;
					return -errno;
				}
				break;
			default:
				p += ret;
				if ((atmost -= ret) <= 0) goto procread;
				break;
		};
	};
 procread:
	END_ALARM;
	TAG(o_3) = NUM | LONGBIGTYPE;
	LONGBIG_VAL(o_3) = index+p;
	FREEopds = o_2;
	return OK;
}

P op_unwaitproc(void) {
	if (o_1 < FLOORopds) return OPDS_UNF;
	TEST_OPAQUE(o_1);

	if (PROCESS_STATE(o_1) == PROCESS_DEAD) RETURN_ERROR(PROC_DEAD);

	PROCESS_STATE(o_1) = 0;
	FREEopds = o_1;
	return OK;
}

P op_killproc(void) {
	pid_t pid;
	int errno_ = 0;

	if (o_1 < FLOORopds) return OPDS_UNF;
	TEST_OPAQUE(o_1);

	if (PROCESS_STATE(o_1) == PROCESS_DEAD) RETURN_ERROR(PROC_DEAD);
	if (PROCESS_STATE(o_1) == PROCESS_BUFFD) RETURN_ERROR(PROC_WAIT);

	PROCESS_STATE(o_1) = PROCESS_DEAD;
	pid = (P)PROCESS_PID(o_1);
	if (kill(pid, SIGTERM)) errno_ = errno;
	if (waitpid(pid, 0, 0) == -1 && ! errno_) errno_ = errno;
	if (close((P)PROCESS_STDIN(o_1)) && ! errno_) errno_ = errno;
	if (close((P)PROCESS_STDOUT(o_1)) && ! errno_) errno_ = errno; 
	if (errno_) return -errno_;

	FREEopds = o_1;
	KILL_OPAQUE(o1);
	return OK;
}

P op_liveproc(void) {
	pid_t pid;
	int status;
	P ret;
	int errno_ = 0;
	B nextchar;
	P pipeout;

	if (o_1 < FLOORopds) return OPDS_UNF;
	TEST_OPAQUE(o_1);

	if (PROCESS_STATE(o_1) == PROCESS_DEAD) RETURN_ERROR(PROC_DEAD);

	if (PROCESS_STATE(o_1) == PROCESS_BUFFD) {
		TAG(o_1) = BOOL; ATTR(o_1) = 0;
		BOOL_VAL(o_1) = TRUE;
		return OK;
	}

	pipeout = (P)PROCESS_STDOUT(o_1);
	timeout = FALSE;
	alarm(10);
	ret = read(pipeout, &nextchar, 1);
	alarm(0);
	if (abortflag) return ABORT;
	if (timeout) return TIMER;
	if (ret == -1) return -errno;

	if ((PROCESS_STATE(o_1) = (ret ? PROCESS_BUFFD : 0))) {
		PROCESS_BUFFC(o_1) = nextchar;
		TAG(o_1) = BOOL; ATTR(o_1) = 0;
		BOOL_VAL(o_1) = TRUE;
		return OK;
	}

	pid = (P)PROCESS_PID(o_1);
	if ((ret = waitpid(pid, &status, WNOHANG)) == -1) {
		errno_ = errno;
		close((P)PROCESS_STDIN(o_1));
		close((P)PROCESS_STDOUT(o_1));
		PROCESS_STATE(o_1) = PROCESS_DEAD;
		kill(pid, SIGKILL);
		waitpid(pid, &status, 0);
		return -errno_;
	}
	if (! ret) {
		TAG(o_1) = BOOL; ATTR(o_1) = 0;
		BOOL_VAL(o_1) = TRUE;
	} else {
		PROCESS_STATE(o_1) = PROCESS_DEAD;
		if (close((P)PROCESS_STDIN(o_1))) errno_ = errno;
		if (close((P)PROCESS_STDOUT(o_1)) && ! errno_) errno_ = errno;
		if (errno_) return -errno_;
		
		if (o2 > CEILopds) return OPDS_OVF;
		ret = WIFEXITED(status) ?
                    WEXITSTATUS(status) : (P) 0xFFFF0000L;
		TAG(o1) = BOOL; ATTR(o1) = 0;
		BOOL_VAL(o1) = FALSE;
		TAG(o_1) = NUM | LONGBIGTYPE; ATTR(o_1) = 0;
		LONGBIG_VAL(o_1) = ret;
		FREEopds = o2;
	}

	return OK;
}

#define senderrno() senderrno_(__LINE__)

static void senderrno_(int line) {
	fprintf(stderr, "Error on: %d\n", line);
	write(1, &errno, sizeof(errno)); 
	exit(1); 
}

P op_makeproc(void) {
    pid_t pid;
    B* param;
    int filedes_stdout[2];
    int filedes_stdin[2];
    ssize_t ret;
    int errno_;
    size_t p;
    B* procframe;
    B initframe[FRAMEBYTES];
    
    if (FREEvm + 2*FRAMEBYTES > CEILvm) return VM_OVF;
    
    if (o_1 < FLOORopds) return OPDS_UNF;
    switch (TAG(o_1)) {
        case STRING: break;
        case LIST:
            if (((LIST_CEIL(o_1)-VALUE_BASE(o_1))/FRAMEBYTES) == 0)
                return RNG_CHK;
            for (param = VALUE_PTR(o_1); 
                 param < (B*) LIST_CEIL(o_1); 
                 param += FRAMEBYTES) {
                if (TAG(param) != STRING)
                    RETURN_ERROR(PROC_ARGS);
                if (ARRAY_SIZE(param) == 0)
                    RETURN_ERROR(PROC_SIZE);
            };
            break;
        default:
            return OPD_TYP;
    };

    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
        RETURN_ERROR(PROC_SIG);
    
    if (pipe(filedes_stdin) || pipe(filedes_stdout)) {
        errno_ = errno;
        close(filedes_stdin[0]);
        close(filedes_stdin[1]);
        close(filedes_stdout[0]);
        close(filedes_stdout[1]);		
        return -errno_;
    }

    if (fcntl(filedes_stdin[0], F_SETFD, 0) == -1
        || fcntl(filedes_stdout[1], F_SETFD, 0) == -1
        || fcntl(filedes_stdin[1], F_SETFD, FD_CLOEXEC) 
        || fcntl(filedes_stdout[0], F_SETFD, FD_CLOEXEC)) {
        errno_ = errno;
        close(filedes_stdin[0]);
        close(filedes_stdin[1]);
        close(filedes_stdout[0]);
        close(filedes_stdout[1]);		
        return -errno_;
    }

    if ((pid = fork()) == -1) {
        errno_ = errno;
        close(filedes_stdin[0]);
        close(filedes_stdin[1]);
        close(filedes_stdout[0]);
        close(filedes_stdout[1]);
        return -errno_;
    }

    if (! pid) {
        char* prog;
        char** args;
        char** arg;

        if (close(0) || close(1)) senderrno();
        if (dup2(filedes_stdin[0], 0) == -1 
            || dup2(filedes_stdout[1], 1) == -1) 
            senderrno();
        if (close(filedes_stdin[0]) || close(filedes_stdout[1])) senderrno();
	
        if (TAG(o_1) == STRING) {
            if (! (prog = malloc(ARRAY_SIZE(o_1)+1))) senderrno();
            strncpy(prog, VALUE_PTR(o_1), ARRAY_SIZE(o_1));
            prog[ARRAY_SIZE(o_1)] = '\0';
            args = NULL;
        } else {
            if (! (args = malloc(1+(LIST_CEIL(o_1)-VALUE_BASE(o_1))/FRAMEBYTES)))
                senderrno();
            for (arg = args, param = VALUE_PTR(o_1);
                 param < (B*) LIST_CEIL(o_1);
                 (param += FRAMEBYTES), arg++) {
                if (! (*arg = malloc(ARRAY_SIZE(param)+1))) senderrno();
                strncpy(*arg, VALUE_PTR(param), ARRAY_SIZE(param));
                (*arg)[ARRAY_SIZE(param)] = '\0';
            }
            *arg = NULL;
            prog = args[0];
        };
        
        errno_ = 0;
        if (write(1, &errno_, sizeof(errno_)) == -1) exit(1);
        
        execve(prog, args, environ);
        perror("process plugin, execve failure in makeproc");
        prog = strerror(errno);
        write(1, "process plugin, execve failure in makeproc: ", 
              strlen("process plugin, execve failure in makeproc: "));
        write(1, prog, strlen(prog));
        exit(127);
    }

    if (close(filedes_stdin[0]) || close(filedes_stdout[1])) {
        errno_ = errno;
        close(filedes_stdin[1]);
        close(filedes_stdout[0]);
        close(filedes_stdout[1]);
        kill(pid, SIGKILL);
        waitpid(pid, NULL, 0);
        return -errno;
    };

    p = 0;
    while (1) {
        ret = read(filedes_stdout[0], ((B*)&errno_)+p, sizeof(errno_)-p);
        if (ret == -1) {
            if (errno != EINTR) {
                errno_ = errno;
                close(filedes_stdin[1]);
                close(filedes_stdout[0]);
                kill(pid, SIGKILL);
                waitpid(pid, NULL, 0);
                return -errno;
            }
        } 
        else if (ret == 0) {
            close(filedes_stdin[1]);
            close(filedes_stdout[0]);
            kill(pid, SIGKILL);
            waitpid(pid, NULL, 0);
            RETURN_ERROR(PROC_EOF);
        }
        else {
            if (ret == (ssize_t) sizeof(errno_) - (ssize_t) p) break;
            p += ret;
        };
    };
    
    if (! (procframe = MAKE_OPAQUE_DICT(0, NULL,
                                        PROCESS_PID_N,
                                        PROCESS_STDOUT_N,
                                        PROCESS_STDIN_N,
                                        PROCESS_STATE_N,
                                        PROCESS_BUFFC_N))) 
        return VM_OVF;
    
    TAG(initframe) = (NUM | LONGBIGTYPE); ATTR(initframe) = 0;
    LONGBIG_VAL(initframe) = pid;
    OPAQUE_MEM_SET(procframe, PROCESS_PID_N, initframe);
    LONGBIG_VAL(initframe) = filedes_stdin[1];
    OPAQUE_MEM_SET(procframe, PROCESS_STDIN_N, initframe);
    LONGBIG_VAL(initframe) = filedes_stdout[0];
    OPAQUE_MEM_SET(procframe, PROCESS_STDOUT_N, initframe);
    TAG(initframe) = (NUM | BYTETYPE);
    *(NUM_VAL(initframe)) = 0;
    OPAQUE_MEM_SET(procframe, PROCESS_STATE_N, initframe);
    OPAQUE_MEM_SET(procframe, PROCESS_BUFFC_N, initframe);

    moveframe(procframe, o_1);	
    return OK;
}
