#ifndef DM_FD_H
#define DM_FD_H

#include "dm.h"

DLL_SCOPE P op_fork(void);
DLL_SCOPE P op_die(void);
DLL_SCOPE P op_makefd(void);
DLL_SCOPE P op_dupfd(void);
DLL_SCOPE P op_spawn(void);
DLL_SCOPE P op_setenv(void);
DLL_SCOPE P op_getenv(void);
DLL_SCOPE P op_persistfd(void);
DLL_SCOPE P op_pipefd(void);
DLL_SCOPE P op_killpid(void);
DLL_SCOPE P op_waitpid(void);
DLL_SCOPE P op_checkpid(void);
DLL_SCOPE P op_openfd(void);
DLL_SCOPE P op_readfd(void);
DLL_SCOPE P op_suckfd(void);
DLL_SCOPE P op_ungetfd(void);
DLL_SCOPE P op_closedfd(void);
DLL_SCOPE P op_writefd(void);
DLL_SCOPE P op_closefd(void);
DLL_SCOPE void setupfd(void);
DLL_SCOPE P op_lockfd(void);
DLL_SCOPE P op_unlockfd(void);
DLL_SCOPE P op_trylockfd(void);

#endif //DM_FD_H
