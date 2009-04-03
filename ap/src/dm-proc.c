#include <limits.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "dm2.h"
#include "dm3.h"
#include "dm-proc.h"
#include "dm-prop.h"
#include "dm-dvt-vm.h"

// -- | socket pid false
//    | socket true
P op_fork(void) {
  pid_t child;
  int sockets[2];
  int sigs[2];
  P retc;

  if (CEILopds < o4) return OPDS_OVF;

  if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockets))
    return -errno;
  
  if (socketpair(AF_UNIX, SOCK_DGRAM, 0, sigs)) {
    int errno_ = errno;
    close(sockets[0]);
    close(sockets[1]);
    return -errno_;
  }

  if ((retc = dm_setsockopts(sockets[0], PACKET_SIZE))
      || (retc = dm_setsockopts(sockets[1], PACKET_SIZE))
      || (retc = dm_setsockopts(sigs[0], 1))
      || (retc = dm_setsockopts(sigs[1], 1))) {
    close(sockets[0]);
    close(sockets[1]);
    close(sigs[0]);
    close(sigs[1]);
    return retc;
  }

  switch ((child = fork())) {
    case -1: return -errno;
    case 0:
      if (close(sockets[0])) exit(-1);
      if (close(sigs[0])) exit(-1);

      set_unixowner(FALSE);
      closesockets();
      set_unixowner(TRUE);

      if ((retc = addsocket(sockets[1], 0, FALSE, TRUE, -1)))
	error(1, -retc, "on addsocket for child-server connection");
      forksighandler(sigs[1], 0);

      TAG(o1) = (NULLOBJ|SOCKETTYPE);
      ATTR(o1) = 0;
      SOCKET_VAL(o1) = sockets[1];
      DGRAM_VAL(o1) = -1;
      consolesocket = sockets[1];

      TAG(o2) = BOOL;
      ATTR(o2) = 0;
      BOOL_VAL(o2) = TRUE;

      FREEopds = o3;
      return OK;
  };

  if (close(sockets[1]) || close(sigs[1])) {
    int errno_ = errno;
    kill(child, SIGKILL);
    close(sigs[1]);
    close(sockets[0]);
    close(sigs[0]);
    return -errno_;
  }

  if ((retc = addsocket(sockets[0], 0, FALSE, TRUE, sigs[0])))
    return retc;

  TAG(o1) = (NULLOBJ|SOCKETTYPE);
  ATTR(o1) = 0;
  SOCKET_VAL(o1) = sockets[0];
  DGRAM_VAL(o1) = sigs[0];

  TAG(o2) = (NULLOBJ|PIDTYPE);
  ATTR(o2) = 0;
  PID_VAL(o2) = child;

  TAG(o3) = BOOL;
  ATTR(o3) = 0;
  BOOL_VAL(o3) = TRUE;
    
  FREEopds = o4;
  return OK;
}

// num | exited
P op_die(void) {
  P ex;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &ex)) return UNDF_VAL;
  if (sizeof(P) > sizeof(int)
      && ex > INT_MAX) return RNG_CHK;

  exit((int)ex);
}

// fd# bool/if-read | fd-obj
P op_makefd(void) {
  P fd;
  if (FLOORopds > o_2) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (CLASS(o_1) != BOOL) return OPD_CLA;
  if (! PVALUE(o_2, &fd)) return UNDF_VAL;
  if (fd < 0 || 
      (sizeof(P) > sizeof(int) && fd > INT_MAX)) 
    return RNG_CHK;

  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILvm) 
    return VM_OVF;
  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm+FRAMEBYTES;
  STREAM_FD(FREEvm+FRAMEBYTES) = (int) fd;
  STREAM_BUFFERED(FREEvm+FRAMEBYTES) = FALSE;
  STREAM_RO(FREEvm+FRAMEBYTES) = BOOL_VAL(o_1);
  moveframe(FREEvm, o_2);
  ATTR(o_2) &= ~PARENT;
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
  if (STREAM_FD(streambox1) == -1 
      || STREAM_FD(streambox2) == -1)
    return STREAM_CLOSED;
  if (STREAM_RO(streambox1) != STREAM_RO(streambox2))
    return STREAM_DIR;

  if (dup2(STREAM_FD(streambox2), STREAM_FD(streambox1)))
    return -errno;
  FREEopds = o_2;
  return OK;
}

// [(exec) (param)... ] | new executable
P op_spawn(void) {
  B* frame;
  B* nextvm = FREEvm;
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
    if (TAG(frame) != (ARRAY|BYTETYPE)) return OPD_TYP;
    nextvm_ = nextvm + ARRAY_SIZE(frame)+1;
    if (nextvm_ >= CEILvm) return VM_OVF;
    moveB(VALUE_PTR(frame), nextvm, ARRAY_SIZE(frame));
    nextvm[ARRAY_SIZE(frame)] = '\0';
    argv[i++] = (char*) nextvm;
    nextvm = nextvm_;
  }
  argv[i] = NULL;

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
      return OPD_CLA;
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

// /VAR (val-buffer) | (val)/NULL
P op_getenv(void) {
  static char str[NAMEBYTES+1];
  char* val;
  size_t n;
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_CLA;
  if (CLASS(o_2) != NAME) return OPD_CLA;
  
  pullname(o_2, str);
  if (! (val = getenv(str))) {
    TAG(o_2) = NULLOBJ;
    ATTR(o_2) = 0;
    FREEopds = o_1;
    return OK;
  }

  if ((n = strlen(val)) > (size_t) ARRAY_SIZE(o_1)) return RNG_CHK;
  moveB((B*) val, VALUE_PTR(o_1), n);
  ARRAY_SIZE(o_1) = n;
  moveframe(o_1, o_2);
  FREEopds = o_1;
  return OK;
}

// fd | --
P op_persistfd(void) {
  int retc;
  B* streambox;
  if (FLOORopds < o_1) return OPDS_UNF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;
  
  if ((retc = nocloseonexec(STREAM_FD(streambox))))
    return retc;
  
  FREEopds = o_1;
  return OK;
}

// -- | fdread fdwrite
P op_pipefd(void) {
  int pipefd[2];
  P retc;
  if (CEILopds < o3) return OPDS_OVF;
  if (FREEvm + 2*FRAMEBYTES + 2*STREAMBOXBYTES >= CEILvm)
    return VM_OVF;
  if (pipe(pipefd)) return -errno;

  if ((retc = addsocket(pipefd[0], 0, FALSE, FALSE, -1))) {
    close(pipefd[1]);
    return retc;
  }

  if ((retc = addsocket(pipefd[1], 0, FALSE, FALSE, -1))) {
    delsocket(pipefd[0]);
    close(pipefd[1]);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm+FRAMEBYTES;
  STREAM_FD(FREEvm+FRAMEBYTES) = pipefd[0];
  STREAM_BUFFERED(FREEvm+FRAMEBYTES) = FALSE;
  STREAM_RO(FREEvm+FRAMEBYTES) = TRUE;
  moveframe(FREEvm, o1);
  ATTR(o1) &= ~PARENT;

  FREEvm += FRAMEBYTES+STREAMBOXBYTES;
  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm+FRAMEBYTES;
  STREAM_FD(FREEvm+FRAMEBYTES) = pipefd[1];
  STREAM_BUFFERED(FREEvm+FRAMEBYTES) = FALSE;
  STREAM_RO(FREEvm+FRAMEBYTES) = FALSE;
  moveframe(FREEvm, o2);
  ATTR(o2) &= ~PARENT;
  FREEvm += FRAMEBYTES+STREAMBOXBYTES;

  FREEopds = o3;
  return OK;
}

// sig pid | --
P op_killpid(void) {
  P sig;
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (NULLOBJ|PIDTYPE)) return OPD_ERR;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &sig)) return UNDF_VAL;
  if (sig > INT_MAX || sig < 0) 
    return RNG_CHK;

  if (kill(PID_VAL(o_1), (int) sig))
    return -errno;

  FREEopds = o_2;
  return OK;
}

// pid | signal-val/exit-val/* true-if-exited or true/false if child not stopped
DM_INLINE_STATIC P dmwait(BOOLEAN nb) {
  pid_t pid;
  int status;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (! nb && CEILopds < o2) return OPDS_OVF;
  if (TAG(o_1) != (NULLOBJ|PIDTYPE)) return OPD_CLA;

  while ((pid = waitpid(PID_VAL(o_1), &status, nb ? (WNOHANG|WNOWAIT) : 0)) 
	 == -1) {
    if (errno != EINTR) return -errno;
    if (abortflag) return ABORT;
  }

  if (nb) {
    TAG(o_1) = BOOL;
    ATTR(o_1) = 0;
    BOOL_VAL(o_1) = pid ? TRUE : FALSE;
    return OK;
  }

  TAG(o_1) = (NUM|BYTETYPE);
  ATTR(o_1) = 0;

  TAG(o1) = BOOL;
  ATTR(o1) = 0;

  if (WIFEXITED(status)) {
    BYTE_VAL(o_1) = WEXITSTATUS(status);
    BOOL_VAL(o1) = TRUE;
  }
  else {
    if (WIFSIGNALED(status))
      BYTE_VAL(o_1) = WTERMSIG(status);
    else
      BYTE_VAL(o_1) = BINF;

    BOOL_VAL(o1) = FALSE;
  }

  FREEopds = o2;
  return OK;
}

// pid | signal-val/exit-val/* true-if-exited
P op_waitpid(void) {
  return dmwait(FALSE);
}

// pid | true/false if child is ready to be waited on.
P op_checkpid(void) {
  return dmwait(TRUE);
}

int flags[] = {
  O_RDWR,
  O_WRONLY|O_CREAT|O_TRUNC,
  O_WRONLY|O_CREAT|O_APPEND
};

// (dir) (filename) flags | fd
P op_openfd(void) {
  P flag;
  B* nextvm;
  int fd;
  P retc;

  if (FLOORopds > o_3) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &flag)) return UNDF_VAL;
  if (flag < 0 || flag >= (P) (sizeof(flags)/sizeof(flags[0])))
    return RNG_CHK;
  if (TAG(o_2) != (ARRAY|BYTETYPE) || TAG(o_3) != (ARRAY|BYTETYPE))
    return OPD_TYP;
  if (FREEvm + FRAMEBYTES + STREAMBOXBYTES >= CEILopds)
    return VM_OVF;

  if (FREEvm + ARRAY_SIZE(o_3)+1 >= CEILopds) return VM_OVF;
  moveB(VALUE_PTR(o_3), FREEvm, ARRAY_SIZE(o_3));
  nextvm = FREEvm + ARRAY_SIZE(o_3);
  if (ARRAY_SIZE(o_3) != 0 && nextvm[-1] != '/')
    (nextvm++)[0] = '/';
  if (nextvm + ARRAY_SIZE(o_2) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_2), nextvm, ARRAY_SIZE(o_2));
  nextvm += ARRAY_SIZE(o_2);
  nextvm[0] = '0';

  if ((fd = open(FREEvm, flags[flag])) == -1)
    return -errno;
  if ((retc = closeonexec(fd))) {
    close(fd);
    return retc;
  }

  TAG(FREEvm) = STREAM;
  ATTR(FREEvm) = PARENT;
  VALUE_PTR(FREEvm) = FREEvm+FRAMEBYTES;
  STREAM_FD(FREEvm+FRAMEBYTES) = fd;
  STREAM_BUFFERED(FREEvm+FRAMEBYTES) = FALSE;
  STREAM_RO(FREEvm+FRAMEBYTES) = (flag == 0) ? TRUE : FALSE;
  moveframe(FREEvm, o_3);
  ATTR(o_3) &= ~PARENT;
  FREEvm += FRAMEBYTES+STREAMBOXBYTES;

  FREEopds = o_2;
  return OK;
}

// fd (buffer) | true/false (sub-buffer)
P op_readfd(void) {
  P fd;
  ssize_t nb;
  B* streambox;
  BOOLEAN closed = FALSE;
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_TYP;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  if (CLASS(o_2) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_2);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
 
  if (ARRAY_SIZE(o_1))
    if (STREAM_BUFFERED(streambox))
      VALUE_PTR(o_1)[0] = STREAM_CHAR(streambox);
  
  if (STREAM_BUFFERED(streambox) && ARRAY_SIZE(o_1) < 2)
    nb = ARRAY_SIZE(o_1);
  else {
    if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;
    B* val = VALUE_PTR(o_1) + (STREAM_BUFFERED(streambox) ? 1 : 0);
    size_t nb_ = ARRAY_SIZE(o_1) - STREAM_BUFFERED(streambox) ? 1: 0;
    while ((nb = read(fd, val, nb_)) == -1) {
      if (errno != EINTR) return -errno;
      if (abortflag) return ABORT;
    }
    if (nb_ && ! nb) {
      delsocket(STREAM_FD(streambox));
      STREAM_FD(streambox) = -1;
      closed = TRUE;
    }
  }

  ARRAY_SIZE(o_1) = (P) nb + STREAM_BUFFERED(streambox) ? 1 : 0;
  STREAM_BUFFERED(streambox) = FALSE;
  if (ARRAY_SIZE(o_1))
    STREAM_CHAR(streambox) = VALUE_PTR(o_1)[ARRAY_SIZE(o_1)-1];
  
  TAG(o_2) = BOOL;
  ATTR(o_2) = 0;
  BOOL_VAL(o_2) = closed;

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
  streambox = VALUE_PTR(o_2);
  if (! STREAM_RO(streambox)) return STREAM_DIR;
  if (FREEvm + FRAMEBYTES >= CEILvm) return VM_OVF;
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
      ATTR(o_1) &= ~PARENT;
      FREEvm += FRAMEBYTES + DALIGN(1);
      return OK;
    }
  }
  
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;

  do {
    while ((nb = read(fd, curr, CEILvm - curr)) == -1) {
      if (errno != EINTR) return -errno;
      if (abortflag) return ABORT;
    }
    curr += nb;
  } while (nb && (DALIGN(curr) < (P) CEILvm));

  if (nb) {
    B extra;
    while ((nb = read(fd, &extra, 1)) == -1) {
      if (errno != EINTR) return -errno;
      if (abortflag) return ABORT;
    }

    if (nb) return VM_OVF;
  }
    
  delsocket(STREAM_FD(streambox));
  STREAM_FD(streambox) = -1;
  ARRAY_SIZE(FREEvm) = curr - (FREEvm + FRAMEBYTES);
  moveframe(FREEvm, o_1);
  ATTR(o_1) &= ~PARENT;
  FREEvm += FRAMEBYTES + DALIGN(ARRAY_SIZE(FREEvm));
  return OK;
}

// fd | true/false
P op_closedfd(void) {
  B* streambox;
  if (FREEopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_1);
  TAG(o_1) = BOOL;
  ATTR(o_1) = 0;
  BOOL_VAL(o_1) = (STREAM_FD(streambox) == -1) ? TRUE : FALSE;
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
  

// fd (buffer) | fd
P op_writefd(void) {
  ssize_t nb, nb_, t;
  B* streambox;
  
  if (FLOORopds > o_2) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_TYP;
  if (CLASS(o_2) != STREAM) return OPD_CLA;
  streambox = VALUE_PTR(o_2);
  if (STREAM_RO(streambox)) return STREAM_DIR;
  if (STREAM_FD(streambox) == -1) return STREAM_CLOSED;

  for (t = 0, nb_ = ARRAY_SIZE(o_1);
       t < nb_;
       t += nb)
    while ((nb = write(STREAM_FD(streambox), 
		       VALUE_PTR(o_1) + t, nb_ - t)) == -1)
      switch (errno) {
	case EINTR: 
	  if (abortflag) return ABORT; 
	  continue;
	case EPIPE: 
	  delsocket(STREAM_FD(streambox));
	  STREAM_FD(streambox) = -1;
	  return STREAM_EPIPE;
	default:
	  return -errno;
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
    retc = delsocket(STREAM_FD(streambox));
    STREAM_FD(streambox) = -1;
  }
  
  FREEopds = o_1;
  return retc;
}

P x_op_lockfd(void) {
  B* streambox;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;
  if (CLASS(x_1) != STREAM) return EXECS_COR;
  
  streambox = VALUE_PTR(x_1);
  if (STREAM_FD(streambox) != -1)
    while (lockf(STREAM_FD(streambox), 0, F_ULOCK)) {
      if (errno != EINTR) return -errno;
      if (abortflag) return ABORT;
    }
  if (! BOOL_VAL(o_1)) FREEexecs = x_1;
  else {
    TAG(x_1) = OP;
    ATTR(x_1) = ACTIVE;
    if (ATTR(o_1) & STOPMARK) {
      OP_NAME(x_1) = "stop";
      OP_CODE(x_1) = op_stop;
    }
    else {
      OP_NAME(x_1) = "abort"; 
      OP_CODE(x_1) = op_abort;
    }
  }
  
  FREEopds = o_1;
  return OK;
}

P x_op_unlockfd(void) {
  B* streambox;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;
  if (CLASS(x_1) != STREAM) return EXECS_COR;
  
  streambox = VALUE_PTR(x_1);
  if (STREAM_FD(streambox) != -1)
    while (lockf(STREAM_FD(streambox), 0, F_LOCK)) {
      if (errno != EINTR) return -errno;
      if (abortflag) return ABORT;
    }

  if (! BOOL_VAL(o_1)) FREEexecs = x_1;
  else {
    TAG(x_1) = OP;
    ATTR(x_1) = ACTIVE;
    if (ATTR(o_1) & STOPMARK) {
      OP_NAME(x_1) = "stop";
      OP_CODE(x_1) = op_stop;
    }
    else {
      OP_NAME(x_1) = "abort"; 
      OP_CODE(x_1) = op_abort;
    }
  }
  
  FREEopds = o_1;
  return OK;
}

P x_op_trylockfd(void) {
  B* streambox;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;
  if (CLASS(x_1) != STREAM) return EXECS_COR;
  
  streambox = VALUE_PTR(x_1);
  if (STREAM_FD(streambox) != -1)
    while (lockf(STREAM_FD(streambox), 0, F_ULOCK)) {
      if (errno != EINTR) return -errno;
      if (abortflag) return ABORT;
    }

  if (! BOOL_VAL(o_1)) {
    FREEexecs = x_1;
    BOOL_VAL(o_1) = TRUE;
    return OK;
  }
  else {
    TAG(x_1) = OP;
    ATTR(x_1) = ACTIVE;
    if (ATTR(o_1) & STOPMARK) {
      OP_NAME(x_1) = "stop";
      OP_CODE(x_1) = op_stop;
    }
    else {
      OP_NAME(x_1) = "abort"; 
      OP_CODE(x_1) = op_abort;
    }

    FREEopds = o_1;
    return OK;
  }
}

// ~active fd | --
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
    if (abortflag) return ABORT;
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
    if (abortflag) return ABORT;
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

// ~active fd | true-if-locked/false
P op_trylockfd(void) {
  B* streambox;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
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
	
      case EINTR: if (abortflag) return ABORT;
	break;

      default:
	return -errno;
    }

  moveframe(o_1, x1);
  
  TAG(x2) = OP;
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_trylockfd";
  OP_CODE(x2) = x_op_trylockfd;

  TAG(x3) = BOOL;
  ATTR(x3) = (STOPMARK|ABORTMARK|ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_2, x4);
  FREEexecs = x5;
  FREEopds = o_1;

  return OK;
}

////// tokenizing /////////////////////////////////////

static B* streambox;

DM_INLINE_STATIC B getc_fd(P* retc) {
  ssize_t nb;

  if (*retc) return 0;
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
    if (abortflag) {
      *retc = ABORT;
      return 0;
    }
  };
  
  if (! nb) {
    *retc = delsocket(STREAM_FD(streambox));
    STREAM_FD(streambox) = -1;
    return 0;
  }

  return STREAM_CHAR(streambox) & 0x7F;
}

DM_INLINE_STATIC void ungetc_fd(P* retc) {
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

  if (! STREAM_RO(streambox)) 
    return STREAM_DIR;

  TAG(o_1) = NUM|LONGBIGTYPE;
  ATTR(o_1) = 0;
  LONGBIG_VAL(o_1) = STREAM_BUFFERED(streambox) ? 1 : 0;
  return OK;
}

void setupfd(void) {
  execfd_func = execfd;
  tokenfd_func = tokenfd;
  usedfd_func = usedfd;
}
