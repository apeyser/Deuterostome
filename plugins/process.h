#ifndef PROCESS_H
#define PROCESS_H

#define PLUGIN_NAME process
#include "../src/plugin.h"

#define op_makeproc  EXPORTNAME(op_makeproc)
#define op_writeproc EXPORTNAME(op_writeproc)
#define op_readproc  EXPORTNAME(op_readproc)
#define op_waitproc  EXPORTNAME(op_waitproc)

// [(program) (argument)...] or (program) | null-handle
L op_makeproc(void);

// (string) null-handle | --
L op_writeproc(void);

// (buffer) index * or length null-handle | (buffer) index
L op_readproc(void);

// null-handle | --
L op_killproc(void);

// null-handle | true or return-code false
L op_liveproc(void);

// null-handle | --
L op_waitproc(void);

#define PROCESS_HANDLE 'P'
#define PROC_ARGS 0x01L //argument types are not strings
#define PROC_WAIT 0x02L //missed waitproc
#define PROC_SIZE 0x03L //argument length must be >0
#define PROC_SIG  0x04L //error setting signal handler
#define PROC_EOF  0x05L //unexpected eof

#endif
