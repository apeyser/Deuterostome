#ifndef DM_VM_H
#define DM_VM_H

#include "dm.h"

DLL_SCOPE P op_vmresize(void);
DLL_SCOPE void maketinysetup(void (*quithandler)(int sig));
DLL_SCOPE P op_abort(void);
DLL_SCOPE P op_vmresize_(void);
DLL_SCOPE P op_lock(void);
DLL_SCOPE P op_unlock(void);
DLL_SCOPE P op_serialize(void);
DLL_SCOPE P op_halt(void);
DLL_SCOPE P x_op_halt(void);
DLL_SCOPE P op_continue(void);
DLL_SCOPE P op_tostderr(void);

DLL_SCOPE void setuphandlers(void (*quithandler)(int sig));

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

#define MSF_SIZE (100000)
DLL_SCOPE B msf[FRAMEBYTES];
DLL_SCOPE B cmsf[FRAMEBYTES];

#endif //DM_VM_H
