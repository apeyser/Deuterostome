#ifndef DM_RTHREADS_H
#define DM_RTHREADS_H

#define PLUGIN_NAME rthreads

#include "../src/plugin.h"

#define RTHREADS_PARENT_RANK_MISSING (1L)
#define RTHREADS_RANK_PRINTF_ERR (2L)
#define RTHREADS_MPI_TYPE (3L)

#define op_makerthreads EXPORTNAME(op_makerthreads)
P op_makerthreads(void);

#define op_killrthreads EXPORTNAME(op_killrthreads)
P op_killrthreads(void);

#endif //DM_RTHREADS_H

