#include "dm_rthreads-header.h"

PLUGIN_INTRO(1);

P ll_errc[] = { 
  RTHREADS_PARENT_RANK_MISSING, 
  RTHREADS_RANK_PRINTF_ERR, 
  RTHREADS_MPI_TYPE, 
  0L
};

B* ll_errm[] = { 
  (B*)"** rthreads: No parent rank passed in", 
  (B*)"** rthreads: Unable to sprintf parent rank", 
  (B*)"** rthreads: Illegal operand type", 
  NULL
};

B* ll_export[] = { 
  PLUGIN_OPS,
  PLUGIN_OP(makerthreads),
  PLUGIN_OP(killrthreads),
  PLUGIN_OP(FINI_),
  PLUGIN_OP(INIT_),
  (B*)"", NULL
};

P op_INIT_(void) {
  return init_();
}

P op_FINI_(void) {return fini_();}

P op_makerthreads(void) {return makerthreads();}

P op_killrthreads(void) {return killrthreads();}

