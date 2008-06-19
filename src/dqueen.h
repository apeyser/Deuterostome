#ifndef DQUEEN_H
#define DQUEEN_H

#include "dm.h"

#define RTHREADS_UNSET    (RTHREADS_ERRS+0)
#define RTHREADS_NUMTYPE  (RTHREADS_ERRS+1)
#define RTHREADS_NUMUNDF  (RTHREADS_ERRS+2)
#define RTHREADS_NUMRNG   (RTHREADS_ERRS+3)
#define RTHREADS_DICTTYPE (RTHREADS_ERRS+4)
#define RTHREADS_VALTYPE  (RTHREADS_ERRS+5)
#define RTHREADS_VALSIZE  (RTHREADS_ERRS+6)
#define RTHREADS_VALEMPTY (RTHREADS_ERRS+7)
#define RTHREADS_KEYSIZE  (RTHREADS_ERRS+8)
#define RTHREADS_VALATR   (RTHREADS_ERRS+9)


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
