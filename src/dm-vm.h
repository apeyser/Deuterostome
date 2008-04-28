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

DLL_SCOPE B msf[FRAMEBYTES];
DLL_SCOPE B cmsf[FRAMEBYTES];

#endif //DM_VM_H
