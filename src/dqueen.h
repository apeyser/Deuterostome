#ifndef DQUEEN_H
#define DQUEEN_H

#include "dm.h"

#if DM_ENABLE_MPI

DLL_SCOPE P op_rthreads(void);
DLL_SCOPE P op_checkrthreads(void);
DLL_SCOPE P op_makerthreads(void);

#endif //DM_ENABLE_MPI

#endif //DQUEEN_H
