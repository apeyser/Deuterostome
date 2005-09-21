#ifndef TEST2_H
#define TEST2_H

#define PLUGIN_NAME test2
#include "../src/plugin.h"

#define op_say5 EXPORTNAME(op_say5)
#define op_sayerror EXPORTNAME(op_sayerror)

#define TEST2_ERROR 0x00000001L

// -- | 6
L op_say5(void);
L op_sayerror(void);

#endif
