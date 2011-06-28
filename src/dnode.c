#include "dm-config.h"
#include "dm-types.h"

#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <errno.h>
#include <spawn.h>

/*************************************** error handling *****************/

#include "dnode.h"

#define DM_IGNORE_RETURN(a) if (a);

static void va_dm_error_msg(int errnum,
			    const char* format,
			    va_list ap)
{
  char* str;
  int s;

  if (vasprintf(&str, format, ap) != -1) {
    DM_IGNORE_RETURN(write(STDERR_FILENO, str, strlen(str)));
    free(str);
  }

  if (errnum) s = asprintf(&str, ": %s\n", strerror(errnum));
  else s = asprintf(&str, "\n");

  if (s != -1) {
    DM_IGNORE_RETURN(write(STDERR_FILENO, str, strlen(str)));
    free(str);
  }
}

__attribute__ ((unused, format (printf, 2, 3)))
static void dm_error_msg(int errnum,
			 const char* format,
			 ...)
{
  va_list ap;
  va_start(ap, format);
  va_dm_error_msg(errnum, format, ap);
  va_end(ap);
}

__attribute__ ((noreturn))
static void va_dm_error(int errnum,
			const char* format,
			va_list ap)
{
  va_dm_error_msg(errnum, format, ap);
  exit(EXIT_FAILURE);
}


__attribute__ ((noreturn, unused, format (printf, 2, 3)))
static void dm_error(int errnum,
		     const char* format,
		     ...)
{
  va_list ap;
  va_start(ap, format);
  va_dm_error(errnum, format, ap);
}

#define err(fn, ...) do {						\
    if (fn(__VA_ARGS__) < 0)						\
      dm_error(errno, #fn);						\
  } while (0)

#define perr(fn, ...) do {						\
    int p;								\
    if ((p = posix_##fn(__VA_ARGS__)))					\
      dm_error(p, "posix_"#fn);						\
  } while (0)

#ifndef DEBUG
#define DEBUG (0)
#endif

#define debug(...) do {							\
    if (DEBUG) {							\
      fprintf(stderr, "%li: file: %s, line: %u: ",			\
	      (long int) getpid(), __FILE__, __LINE__);			\
      fprintf(stderr, __VA_ARGS__);					\
      fprintf(stderr, "\n");						\
    };									\
  } while (0)

/**********************************************************************/

static sigset_t block_all;
static sigset_t original_set;
static sigset_t alarm_set;

#define RESTART_WAIT (10)

extern char** environ;
static posix_spawnattr_t attrp;
static char** _argv;
static int _argc;
static pid_t pid = 0;

DM_INLINE_STATIC void dnode(void) {
  perr(spawn, &pid, dmrunnode, NULL, &attrp, _argv, environ);
}

DM_INLINE_STATIC void makealarm(int s) {
  sigset_t p;
  alarm(0);
  err(sigemptyset, &p);

  while (1) {
    err(sigpending, &p);
    if (! sigismember(&p, SIGALRM)) break;

    err(sigemptyset, &p);
    err(sigaddset,   &p, SIGALRM);
    err(sigwaitinfo, &p, NULL);
  }
  alarm(s);
}

static void killterm(int sig) {
  err(kill, pid, SIGKILL);
}

static struct sigaction killterm_sa = {
  .sa_handler = killterm,
  .sa_flags = 0
};

static void die(void) {
  if (! pid) return;

  err(kill, pid, SIGTERM);
  err(sigaction, SIGALRM, &killterm_sa, NULL);
  makealarm(RESTART_WAIT);
  err(sigprocmask, SIG_UNBLOCK, &alarm_set, NULL);

  if (wait(NULL) < 0) err(wait, NULL);
}

DM_INLINE_STATIC int waitchild(int* status) {
  pid_t _pid = pid;
  if (! pid) return 0;

  pid = 0;
  debug("wait for: %li ", (long int) _pid);
  return waitpid(_pid, status, 0);
}

enum SIGSTATE {
  SIGSTATE_PROP,
  SIGSTATE_RESTART,
  SIGSTATE_CHILD
};

DM_INLINE_STATIC enum SIGSTATE nextsig(const sigset_t* sigs, int* sig) {
  static int _sig;
  siginfo_t info;
  if (! sig) sig = &_sig;

  switch ((*sig = sigwaitinfo(sigs, &info))) {
    case -1:
      dm_error(errno, "sigwaitinfo");
    case SIGCHLD:
      return SIGSTATE_CHILD;
    case DM_RESTART:
      if (info.si_code == SI_QUEUE
	  && info.si_value.sival_int == DM_RESTART_VAL.sival_int) {
	debug("restart");
	return SIGSTATE_RESTART;
      }
      else debug("not restart");
    // intentional fallthrough
    default:
      return SIGSTATE_PROP;
  }
}

int main(int argc, char *argv[]) {
  sigset_t main_set, restart_set, pending, child_set, recvd_set;
  int status;
  int sig;
  struct sigaction recvd_sa = {
    .sa_handler = SIG_DFL,
    .sa_flags = 0
  };

  err(sigfillset,  &block_all);
  err(sigemptyset, &original_set);
  err(sigemptyset, &main_set);
  err(sigprocmask, SIG_SETMASK, &block_all, &original_set);
  err(sigprocmask, SIG_UNBLOCK, &original_set, NULL);
  err(sigprocmask, SIG_SETMASK, &block_all, &main_set);
  err(sigaddset,   &main_set, SIGCHLD);
  err(sigaddset,   &main_set, DM_RESTART);

  err(sigemptyset, &child_set);
  err(sigaddset,   &child_set, SIGCHLD);

  err(sigemptyset, &restart_set);
  err(sigaddset,   &restart_set, DM_RESTART);

  err(sigemptyset, &alarm_set);
  err(sigaddset,   &alarm_set, SIGALRM);
  err(sigfillset,  &killterm_sa.sa_mask);

  _argv = malloc(sizeof(char*)*(argc+1));
  _argc = argc;

  for (; argc >= 0; argc--) _argv[argc] = argv[argc];

  perr(spawnattr_init,       &attrp);
  perr(spawnattr_setflags,   &attrp, POSIX_SPAWN_SETSIGMASK);
  perr(spawnattr_setsigmask, &attrp, &original_set);

  atexit(die);

 restart:
  err(waitchild, NULL);
  debug("dnode");
  dnode();
  debug("started dnode: %li", (long int) pid);

 nextsig:
  debug("next signal");
  switch (nextsig(&main_set, &sig)) {
    case SIGSTATE_PROP:
      debug("received: %i", sig);
      err(kill, pid, sig);
      goto nextsig;

    case SIGSTATE_RESTART:
      debug("grab child for: %li", (long int) pid);
      nextsig(&child_set, NULL);
      goto restart;

    case SIGSTATE_CHILD:
      err(sigpending, &pending);
      if (! sigismember(&pending, DM_RESTART)) break;

      debug("grab restart for: %li", (long int) pid);
      nextsig(&restart_set, &sig);
      goto restart;
  };

  dm_error_msg(0, "Exiting dnode");
  err(waitchild, &status);
  if (WIFEXITED(status)) {
    dm_error_msg(0, "Status dnode: %i", WEXITSTATUS(status));
    return WEXITSTATUS(status);
  }

  dm_error_msg(0, "Signal dnode: %i", WTERMSIG(status));
  err(sigfillset, &recvd_sa.sa_mask);
  err(sigaction, WTERMSIG(status), &recvd_sa, NULL);

  err(sigemptyset, &recvd_set);
  err(sigaddset,   &recvd_set, WTERMSIG(status));
  err(sigprocmask, SIG_UNBLOCK, &recvd_set, NULL);
  err(raise, WTERMSIG(status));

  dm_error(0, "No exit value, no signal?");
}
