#ifndef THROW_H
#define THROW_H

#define PLUGIN_NAME throw
#include "../src/plugin.h"

#define op_caught EXPORTNAME(op_caught)
#define op_throw  EXPORTNAME(op_throw)
#define x_op_uncaught PRIVATENAME(x_op_uncaught)
#define x_op_uncaught_name PRIVATENAME(x_op_uncaught_name)

L op_caught(void);
L op_throw(void);
L x_op_uncaught(void);
const char* x_op_uncaught_name;

#define INV_THROW 0x01L //invalid throw

#endif //THROW_H
