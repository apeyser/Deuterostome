#ifndef DM_SIGNALS
#define DM_SIGNALS

#include "dm.h"

// these must be in the same order as sigmap in dm-vm.c
#define SIGMAP_QUIT  0
#define SIGMAP_KILL  (SIGMAP_QUIT+1)
#define SIGMAP_ABORT (SIGMAP_KILL+1)
#define SIGMAP_TERM  (SIGMAP_ABORT+1)
#define SIGMAP_HUP   (SIGMAP_TERM+1)
#define SIGMAP_INT   (SIGMAP_HUP+1)
#define SIGMAP_ALARM (SIGMAP_INT+1)
#define SIGMAP_FPE   (SIGMAP_ALARM+1)
DLL_SCOPE void propagate_sig(B sig, void (*redirect_sigf)(int sig));

DLL_SCOPE void sethandler(int sig, void (*handler)(int sig));

#endif //DM_SIGNALS
