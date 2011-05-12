#ifndef DM_SIGNALS
#define DM_SIGNALS

#include "dm.h"

// these must be in the same order as sigmap in dm-signals.c
enum SIGMAP {
  SIGMAP_QUIT,
  SIGMAP_KILL,
  SIGMAP_TERM,
  SIGMAP_HUP,
  SIGMAP_INT,
  SIGMAP_ALRM,
  SIGMAP_FPE,
  SIGMAP_ABRT,
  SIGMAP_BUS,
  SIGMAP_CHLD,
  SIGMAP_CONT,
  SIGMAP_ILL,
  SIGMAP_PIPE,
  SIGMAP_SEGV,
  SIGMAP_STOP,
  SIGMAP_TSTP,
  SIGMAP_TTIN,
  SIGMAP_TTOU,
  SIGMAP_USR1,
  SIGMAP_USR2,
  SIGMAP_POLL,
  SIGMAP_PROF,
  SIGMAP_SYS,
  SIGMAP_TRAP,
  SIGMAP_URG,
  SIGMAP_VTALRM,
  SIGMAP_XCPU,
  SIGMAP_XFSZ,
  SIGMAP_LEN
};

DLL_SCOPE void propagate_sig(B sig, void (*redirect_sigf)(int sig));
DLL_SCOPE UW encodesig(int sig);
DLL_SCOPE int decodesig(UW sig);

DLL_SCOPE void sethandler(enum SIGMAP sig,
			  void (*handler)(int sig, 
					  siginfo_t* info, 
					  void* ucon));
DLL_SCOPE void clearhandler(enum SIGMAP sig);

#endif //DM_SIGNALS
