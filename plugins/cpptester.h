#ifndef CPPTESTER_H
#define CPPTESTER_H

#define PLUGIN_NAME cpptester
#include "../src/plugin.h"



#define CPPTESTER_BAD_ALLOC (1L)
#define CPPTESTER_ABORT_ALLOC (2L)
#define CPPTESTER_LEAK_ALLOC (3L)
#define CPPTESTER_UNKNOWN_ALLOC (4L)

#define op_maketester EXPORTNAME(op_maketester)
L op_maketester(void);

#define op_runtester EXPORTNAME(op_runtester)
L op_runtester(void);

#endif
