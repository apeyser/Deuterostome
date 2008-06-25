#ifndef DQUEEN_H
#define DQUEEN_H

#include "dm.h"

#if DM_ENABLE_RTHREADS

DLL_SCOPE P op_rthreads(void);
DLL_SCOPE P op_checkrthreads(void);
DLL_SCOPE P op_makerthreads(void);
DLL_SCOPE P op_rsend(void);
DLL_SCOPE P op_rsendsig(void);

DLL_SCOPE void rthreads_init(void);
DLL_SCOPE P killrthreads(void);

#endif //DM_ENABLE_RTHREADS

#endif //DQUEEN_H
