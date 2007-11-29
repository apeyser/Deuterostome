#ifndef THROW_H
#define THROW_H

#define PLUGIN_NAME throw
#include "../src/plugin.h"

#define op_thrown EXPORTNAME(op_thrown)
#define op_throw  EXPORTNAME(op_throw)
#define x_op_thrown PRIVATENAME(x_op_thrown)
#define x_op_thrown_name PRIVATENAME(x_op_thrown_name)

P op_throw(void);
P op_thrown(void);
P x_op_thrown(void);
const char* x_op_uncaught_name;

#define INV_THROW 0x01L //invalid throw

#endif //THROW_H
