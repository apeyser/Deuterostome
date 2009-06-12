#ifndef DM_STATS_H
#define DM_STATS_H

#define PLUGIN_NAME stats

#include "../src/plugin.h"


#define op_legendre EXPORTNAME(op_legendre)
P op_legendre(void);

#define op_combinations EXPORTNAME(op_combinations)
P op_combinations(void);

#endif //DM_STATS_H

