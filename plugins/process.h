#ifndef PROCESS_H
#define PROCESS_H

#define PLUGIN_NAME process
#include "../src/plugin.h"

#define op_makeproc   EXPORTNAME(op_makeproc)
#define op_writeproc  EXPORTNAME(op_writeproc)
#define op_killproc   EXPORTNAME(op_killproc)
#define op_liveproc   EXPORTNAME(op_liveproc)
#define op_readproc   EXPORTNAME(op_readproc)
#define op_waitproc   EXPORTNAME(op_waitproc)
#define op_unwaitproc EXPORTNAME(op_unwaitproc)

// [(program) (argument)...] or (program) | handle
P op_makeproc(void);

// (string) handle | --
P op_writeproc(void);

// (buffer) index * or length handle | (buffer) index
P op_readproc(void);

// handle | --
P op_killproc(void);

// handle | true or return-code false
P op_liveproc(void);

// handle | --
P op_waitproc(void);

// handle | --
P op_unwaitproc(void);

#define PROCESS_HANDLE "PROCESS"
#define PROC_ARGS 0x01L //argument types are not strings
#define PROC_WAIT 0x02L //missed waitproc
#define PROC_SIZE 0x03L //argument length must be >0
#define PROC_SIG  0x04L //error setting signal handler
#define PROC_EOF  0x05L //unexpected eof
#define PROC_DEAD 0x06L //Process is dead

#define PROCESS_PID(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, PROCESS_PID_N)))
#define PROCESS_STDIN(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, PROCESS_STDIN_N)))
#define PROCESS_STDOUT(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, PROCESS_STDOUT_N)))
#define PROCESS_STATE(frame) (*(NUM_VAL(OPAQUE_MEM(frame, PROCESS_STATE_N))))
#define PROCESS_BUFFC(frame) (*(NUM_VAL(OPAQUE_MEM(frame, PROCESS_BUFFC_N))))

#define PROCESS_BUFFD 1
#define PROCESS_DEAD  2

#endif
