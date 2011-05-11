#include "dm.h"

#include <signal.h>
#include <errno.h>

#include "dm-signals.h"
#include "error-local.h"

// these must be kept in the same order as SIGMAP_* in dm-signals.h
// and SIGNALS in startup_common.din
// from
// http://pubs.opengroup.org/onlinepubs/009695399/basedefs/signal.h.html
static int sigmap[] = {
  SIGQUIT,
  SIGKILL,
  SIGTERM,
  SIGHUP,
  SIGINT,
  SIGALRM,
  SIGFPE,
  SIGABRT,
  SIGBUS,
  SIGCHLD,
  SIGCONT,
  SIGILL,
  SIGPIPE,
  SIGSEGV,
  SIGSTOP,
  SIGTSTP,
  SIGTTIN,
  SIGTTOU,
  SIGUSR1,
  SIGUSR2,
  SIGPOLL,
  SIGPROF,
  SIGSYS,
  SIGTRAP,
  SIGURG,
  SIGVTALRM,
  SIGXCPU,
  SIGXFSZ,
};

void propagate_sig(B sig, void (*redirect_sigf)(int sig)) {
  if (sig > (B) (sizeof(sigmap)/sizeof(sigmap[0])) || sig < 0) {
    error_local(0, 0, "received illegal signal %i", sig);
    return;
  }

  redirect_sigf(sigmap[sig]);
}

enum SIGMAP getcookedsig(int sig) {
  int i;
  for (i = 0; i < SIGMAP_LEN; i++)
    if (sig == sigmap[i]) return (enum SIGMAP) i;
  return (enum SIGMAP) SIGMAP_LEN;
}

UW encodesig(int sig) {
  int i;
  for (i = 0; i < (int) SIGMAP_LEN; i++)
    if (sig == sigmap[i]) return (((UW) 0xFF) << 8) | (UW) i;

  return ((UW) sig) << 8;
}

int decodesig(UW sig) {
  UW dsig = sig >> 8;
  if (dsig != 0xFF) return dsig;

  sig &= 0xFF;
  if (sig >= 0 && sig < (int) SIGMAP_LEN)
    return sigmap[sig];

  return 0;
}

void clearhandler(int sig) 
{
  struct sigaction sa;
  sa.sa_handler = SIG_IGN;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 0;
  if (sigaction(sig, &sa, NULL))
    error_local(1, errno, "Unable to set signal handler for %i", sig);
}

void sethandler(int sig, void (*handler)(int sig, siginfo_t* info, void* ucon))
{
  struct sigaction sa;
  sa.sa_sigaction = handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;
  if (sigaction(sig, &sa, NULL))
    error_local(1, errno, "Unable to set signal handler for %i", sig);
}
