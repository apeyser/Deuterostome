#include "dm-stats-header.h"

PLUGIN_INTRO(0);

P ll_errc[] = { 
  0L
};

B* ll_errm[] = { 
  NULL
};

B* ll_export[] = { 
  PLUGIN_OPS,
  PLUGIN_OP(legendre),
  PLUGIN_OP(combinations),
  (B*)"", NULL
};

P op_legendre(void) {return legendre();}

P op_combinations(void) {return combinations();}

