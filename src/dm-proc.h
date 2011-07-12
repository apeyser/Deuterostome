#ifndef DM_FD_H
#define DM_FD_H

#include "dm.h"

DLL_SCOPE P op_readdir(void);
DLL_SCOPE P op_stat(void);
DLL_SCOPE P op_makedir(void);
DLL_SCOPE P op_utimes(void);
DLL_SCOPE P op_rename(void);
DLL_SCOPE P op_cp(void);
DLL_SCOPE P op_realpath(void);
DLL_SCOPE P op_statvfs(void);
DLL_SCOPE P op_fork(void);
DLL_SCOPE P op_getppid(void);
DLL_SCOPE P op_makefd(void);
DLL_SCOPE P op_unmakefd(void);
DLL_SCOPE P op_readonlyfd(void);
DLL_SCOPE P op_dupfd(void);
DLL_SCOPE P op_spawn(void);
DLL_SCOPE P op_setenv(void);
DLL_SCOPE P op_getenv(void);
DLL_SCOPE P op_pipefd(void);
DLL_SCOPE P op_copyfd(void);
DLL_SCOPE P op_killpid(void);
DLL_SCOPE P op_waitpid(void);
DLL_SCOPE P op_checkpid(void);
DLL_SCOPE P op_openfd(void);
DLL_SCOPE P op_readfd(void);
DLL_SCOPE P op_readtomarkfd(void);
DLL_SCOPE P op_readtomarkfd_nb(void);
DLL_SCOPE P op_tmpfile(void);
DLL_SCOPE P op_tmpdir(void);
DLL_SCOPE P op_rmpath(void);
DLL_SCOPE P op_finddir(void);
DLL_SCOPE P op_suckfd(void);
DLL_SCOPE P op_getfd(void);
DLL_SCOPE P op_ungetfd(void);
DLL_SCOPE P op_closedfd(void);
DLL_SCOPE P op_writefd(void);
DLL_SCOPE P op_closefd(void);

DLL_SCOPE P op_lockfd(void);
DLL_SCOPE P op_lockfd_ex(void);
DLL_SCOPE P op_lockfd_sh(void);
DLL_SCOPE P op_unlockfd(void);
DLL_SCOPE P op_trylockfd(void);
DLL_SCOPE P op_trylockfd_ex(void);
DLL_SCOPE P op_trylockfd_sh(void);

DLL_SCOPE void setupfd(void);

#endif //DM_FD_H
