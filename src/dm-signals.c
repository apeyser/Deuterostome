#include "dm.h"

#include <signal.h>
#include <errno.h>

#include "dm-signals.h"
#include "error-local.h"

// these must be kept in the same order as SIGMAP_* in dm-signals.h
// and SIGNALS in startup_common.din
int sigmap[] = {
  SIGQUIT,
  SIGKILL,
  SIGTERM, 
  SIGHUP, 
  SIGINT, 
  SIGALRM, 
  SIGFPE
};

void propagate_sig(B sig, void (*redirect_sigf)(int sig)) {
  if (sig > (B) (sizeof(sigmap)/sizeof(sigmap[0])) || sig < 0) {
    error_local(0, 0, "received illegal signal %i", sig);
    return;
  }

  redirect_sigf(sigmap[sig]);
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
