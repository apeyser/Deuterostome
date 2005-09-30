#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <time.h>
#include <errno.h>
#include <sys/wait.h>
#include <string.h>

#include "process.h"

UL ll_type = 0;
L op_hi(void) {return wrap_hi("process V1");}
L op_libnum(void) {return wrap_libnum(ll_type);}
L ll_errc[] = {PROC_ARGS, PROC_WAIT, PROC_SIZE, PROC_SIG, PROC_EOF, 0L};
B* ll_errm[] = {
	"** process: argument list must be composed of strings",
	"** process: waitproc call not immediately followed bre readproc",
	"** process: argument string must not have a string length of 0",
	"** process: error setting signal handler",
	"** process: unexpected end of file",
};

B* ll_export[] = {
	"hi", (B*) op_hi,
	"libnum", (B*) op_libnum,
	"makeproc", (B*) op_makeproc,
	"writeproc", (B*) op_writeproc,
	"readproc", (B*) op_readproc,
	"killproc", (B*) op_killproc,
	"waitproc", (B*) op_waitproc,
	"", NULL
};

#define DECLARE_ALARM \
  clock_t endclock = clock() + 180*CLOCKS_PER_SEC; \
  L chunk_size; UL ai = 0

#define START_ALARM {timeout = FALSE;}

#define MAX_CHUNK (32000)
//100mbit/s*1/8mbyte/mbit*1024byte/mbyte*5s*1/2minrate*/

#define CHECK_ALARM {\
  alarm(0); \
  if (clock() > endclock) {return(TIMER);}; \
  if (timeout) {return(TIMER);}; \
  chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost; \
  alarm(10); \
  timeout = FALSE; ai++; \
  if (abortflag) return ABORT;}

#define END_ALARM { \
  alarm(0); \
  timeout = FALSE;}

static B waitchar;
static BOOLEAN waitchar_;

L op_writeproc(void) {
	pid_t pid;
	L pipein;
	L p, atmost, ret;
	DECLARE_ALARM;

	if (waitchar_) {
		waitchar_ = FALSE;
		RETURN_ERROR(PROC_WAIT);
	}

	if (o_2 < FLOORopds) return OPDS_UNF;
	if (TAG(o_1) != (NULLOBJ | HANDLETYPE)) return OPD_TYP;
	if (HANDLE_ID(o_1) != PROCESS_HANDLE) return ILL_HANDLE;
	if (TAG(o_2) != STRING) return OPD_TYP;
	if (ARRAY_SIZE(o_2) == 0) return RNG_CHK;

	pipein = LONG_VAL2(o_2);
	p = 0;
	atmost = ARRAY_SIZE(o_2);
	START_ALARM;
	while (1) {
		CHECK_ALARM;
		ret = write(pipein, VALUE_PTR(o_2)+p, chunk_size);
		if (ret == -1) {
			if (errno != EINTR && errno != EAGAIN) {
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

L op_waitproc(void) {
	L pipeout;
	L ret;

	if (waitchar_) RETURN_ERROR(PROC_WAIT);

	if (o_1 < FLOORopds) return OPDS_UNF;
	if (TAG(o_1) != (NULLOBJ | HANDLETYPE)) return OPD_TYP;
	if (HANDLE_ID(o_1) != PROCESS_HANDLE) return ILL_HANDLE;

	pipeout = LONG_VAL3(o_1);
 waitproc:
	timeout = FALSE;
	alarm(10);
	ret = read(pipeout, &waitchar, 1);
	alarm(0);
	if (timeout && abortflag) return ABORT;
	if (ret == -1) {
		if (errno != EINTR && errno != EAGAIN) return -errno;
		goto waitproc;
	};
 
	waitchar_ = ret ? TRUE : FALSE;
	FREEopds = o_1;
	return OK;
}

L op_readproc(void) {
	L pipeout;
	L index, p, ret, atmost;
	DECLARE_ALARM;
	
	if (o_4 < FLOORopds) return OPDS_UNF;
	if (TAG(o_1) != (NULLOBJ | HANDLETYPE)) return OPD_TYP;
	if (HANDLE_ID(o_1) != PROCESS_HANDLE) return ILL_HANDLE;
	if (CLASS(o_2) != NUM) return OPD_TYP;
	if (CLASS(o_3) != NUM) return OPD_TYP;
	if (TAG(o_4) != STRING) return OPD_TYP;
	if (ARRAY_SIZE(o_4)-index < 1) return RNG_CHK;
	if (! VALUE(o_2, &atmost)) atmost = ARRAY_SIZE(o_4)-index;
	if (ARRAY_SIZE(o_4)-index-atmost < 0) return RNG_CHK;

	pipeout = LONG_VAL3(o_1);
	p = index;
	if (waitchar_ == TRUE) {
		VALUE_PTR(o_4)[p++] = waitchar;
		waitchar_ = FALSE;
	}
	START_ALARM;
	while (1) {
		CHECK_ALARM;
		ret = read(pipeout, VALUE_PTR(o_4)+p, chunk_size);
		if (ret == -1) {
			if (errno != EINTR && errno != EAGAIN) {
				END_ALARM;
				return -errno;
			}
		} else {
			p += ret;
			if ((atmost -= ret) <= 0) goto procread;
		};
	};
 procread:
	END_ALARM;
	TAG(o_3) = NUM | LONGTYPE;
	LONG_VAL(o_3) = index+p;
	FREEopds = o_2;
	return OK;
}

L op_killproc(void) {
	pid_t pid;
	int status;
	L ret;
	int errno_;

	if (waitchar_) {
		waitchar_ = FALSE;
		RETURN_ERROR(PROC_WAIT);
	}

	if (o_1 < FLOORopds) return OPDS_UNF;
	if (TAG(o_1) != (NULLOBJ | HANDLETYPE)) return OPD_TYP;
	if (HANDLE_ID(o_1) != PROCESS_HANDLE) return ILL_HANDLE;

	pid = (pid_t) LONG_VAL(o_1);
	if (kill(pid, SIGTERM)) {
		errno_ = errno;
		close(LONG_VAL2(o_1));
		close(LONG_VAL3(o_1));
		return -errno_;
	}
	if (close(LONG_VAL2(o_1))) {
		errno_ = errno;
		close(LONG_VAL3(o_1));
		return -errno_;
	}
	if (close(LONG_VAL3(o_1))) 
		return -errno;

	FREEopds = o_1;
	return OK;
}

L op_liveproc(void) {
	pid_t pid;
	int status;
	L ret;

	if (waitchar_) {
		waitchar_ = FALSE;
		RETURN_ERROR(PROC_WAIT);
	}

	if (o_1 < FLOORopds) return OPDS_UNF;
	if (TAG(o_1) != (NULLOBJ | HANDLETYPE)) return OPD_TYP;
	if (HANDLE_ID(o_1) != PROCESS_HANDLE) return ILL_HANDLE;

	pid = (pid_t) LONG_VAL(o_1);
	if ((ret = waitpid(pid, &status, WNOHANG)) == -1) {
		close(LONG_VAL2(o_1));
		close(LONG_VAL3(o_1));
		kill(pid, SIGKILL);
		return -errno;
	}
	if (! ret) {
		TAG(o_1) = BOOL; ATTR(o_1) = 0;
		BOOL_VAL(o_1) = TRUE;
	} else {
		if (close(LONG_VAL2(o_1)) || close(LONG_VAL3(o_1))) {
			close(LONG_VAL3(o_1));
			return -errno;
		}
		
		if (o2 > CEILopds) return OPDS_OVF;
		ret = WIFEXITED(status) ? WEXITSTATUS(status) : 0xFFFF0000L;
		TAG(o1) = BOOL; ATTR(o1) = 0;
		BOOL_VAL(o1) = FALSE;
		TAG(o_1) = LONGTYPE; ATTR(o_1) = 0;
		LONG_VAL(o_1) = ret;
		FREEopds = o2;
	}

	return OK;
}

static void senderrno(void) {						 
	write(1, &errno, sizeof(errno)); 
	exit(1); 
}

L op_makeproc(void) {
	pid_t pid;
	B* param;
	L filedes_stdout[2];
	L filedes_stdin[2];
	char buf[1];
	int ret;
	int errno_;
	size_t p;

	if (waitchar_) {
		waitchar_ = FALSE;
		RETURN_ERROR(PROC_WAIT);
	}

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
		return -errno;
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

		if (dup2(filedes_stdin[0], 0) || dup2(filedes_stdout[1], 1)) senderrno();
		if (close(filedes_stdin[0]) || close(filedes_stdout[1])
				|| close(filedes_stdin[1]) || close(filedes_stdout[0])) 
			senderrno();
		
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
			prog = *(args++);
		};

		errno_ = 0;
		if (write(1, &errno_, sizeof(errno_)) == -1) exit(1);

		execve(prog, args, NULL);
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
		return -errno;
	};

	p = 0;
	while (1) {
		ret = read(filedes_stdout[0], ((B*)&errno_)+p, sizeof(errno_)-p);
		if (ret == -1) {
			if (errno == EINTR) break;
			errno_ = errno;
			close(filedes_stdin[1]);
			close(filedes_stdout[0]);
			kill(pid, SIGKILL);
			return -errno;
		} else {
			if (ret == sizeof(errno_)-p) goto procsetup;
			p += ret;
			break;
		};
	};
	
 procsetup:
	if (p < sizeof(errno_)) RETURN_ERROR(PROC_EOF);

	if (errno_) {
		waitpid(pid, NULL, 0);
		close(filedes_stdin[1]);
		close(filedes_stdout[0]);
		return -errno_;
	}

	TAG(o_1) = (NULLOBJ | HANDLETYPE);
	ATTR(o_1) = 0;
	HANDLE_ID(o_1) = PROCESS_HANDLE;
	LONG_VAL(o_1) = (L) pid;
	LONG_VAL2(o_1) = (L) filedes_stdin[1];
	LONG_VAL3(o_1) = (L) filedes_stdout[0];
	
	return OK;
}
