#ifndef DM_VM_H
#define DM_VM_H

#include "dm.h"

#include "dm-dvt-vm.h"

DLL_SCOPE P op_vmresize(void);
DLL_SCOPE void maketinysetup(void);
DLL_SCOPE P op_vmresize_(void);
DLL_SCOPE P op_lock(void);
DLL_SCOPE P op_unlock(void);
DLL_SCOPE P op_serialize(void);
DLL_SCOPE P op_halt(void);
DLL_SCOPE P x_op_halt(void);
DLL_SCOPE P op_continue(void);

#define MSF_SIZE (100000)
DLL_SCOPE B msf[FRAMEBYTES];
DLL_SCOPE B cmsf[FRAMEBYTES];

#endif //DM_VM_H
