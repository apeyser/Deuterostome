#include <signal.h>
#include <errno.h>

#include "dm-signals.h"
#include "error-local.h"

// these must be kept in the same order as SIGMAP_* in dm-vm.h
// and SIGNALS in startup_common.d.in
static int sigmap[] = {
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
    error(0, 0, "received illegal signal %i", sig);
    return;
  }

  redirect_sigf(sigmap[sig]);
}

void sethandler(int sig, void (*handler)(int sig)) {
  struct sigaction sa;
  
  sa.sa_handler = handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 0;
  if (sigaction(sig, &sa, NULL))
    error(1, errno, "Unable to set signal handler for %i", sig);
}
