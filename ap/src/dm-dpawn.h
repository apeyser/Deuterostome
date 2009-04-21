#ifndef DM_DPAWN_H
#define DM_DPAWN_H

#include "dm.h"

DLL_SCOPE P op_vmresize(void);
DLL_SCOPE void run_dpawn_mill(void) __attribute__ ((__noreturn__));
DLL_SCOPE void makeerror(P retc, B* error_source);
DLL_SCOPE P op_error(void);
DLL_SCOPE P op_errormessage(void);
DLL_SCOPE P op_toconsole(void);
DLL_SCOPE P op_groupconsole(void);
DLL_SCOPE P op_quit(void);

DLL_SCOPE P op_mpiprobe(void);
DLL_SCOPE P op_mpiiprobe(void);
DLL_SCOPE P op_mpisend(void);
DLL_SCOPE P op_mpirecv(void);
DLL_SCOPE P op_mpibroadcast(void);
DLL_SCOPE P op_mpibarrier(void);
DLL_SCOPE P op_rsend(void);
DLL_SCOPE P op_mpirank(void);
DLL_SCOPE P op_mpisize(void);

DLL_SCOPE P recur_dpawn_mill(B* execf);


#endif //DM_DPAWN_H
