#ifndef CPPTESTER_H
#define CPPTESTER_H

#define PLUGIN_NAME cpptester
#include "../src/plugin.h"

#define CPPTESTER_TESTER(frame) (INNERP_VAL(OPAQUE_MEM(frame, CPPTESTER_TESTER_N)))


#define CPPTESTER_BAD_ALLOC (1L)
#define CPPTESTER_STD_EXCEPTION (2L)
#define CPPTESTER_UNKNOWN_EXCEPTION (3L)
#define CPPTESTER_ILLEGAL_RETURN (4L)

#define op_maketester EXPORTNAME(op_maketester)
P op_maketester(void);

#define op_runtester EXPORTNAME(op_runtester)
P op_runtester(void);

#define op_randomtester EXPORTNAME(op_randomtester)
P op_randomtester(void);

#define op_bigtester EXPORTNAME(op_bigtester)
P op_bigtester(void);

#define op_killtester EXPORTNAME(op_killtester)
P op_killtester(void);

#endif
