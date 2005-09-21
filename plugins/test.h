#ifndef TEST_H
#define TEST_H

#define PLUGIN_NAME test
#include "../src/plugin.h"

#define op_say5 EXPORTNAME(op_say5)
#define op_sayerror EXPORTNAME(op_sayerror)

#define TEST_ERROR 0x00000001L

// -- | 5
L op_say5(void);
L op_sayerror(void);

#endif
