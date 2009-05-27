#ifndef DM7_H
#define DM7_H

#include "dm.h"

/*-- time/date and file access  */
DLL_SCOPE P op_gettime(void);
DLL_SCOPE P op_gettimeofday(void);
DLL_SCOPE P op_profiletime(void);
DLL_SCOPE P op_localtime(void);
DLL_SCOPE P op_getwdir(void);
DLL_SCOPE P op_setwdir(void);
DLL_SCOPE P op_writefile(void);
DLL_SCOPE P op_readfile(void);
DLL_SCOPE P op_findfiles(void);
DLL_SCOPE P op_findfile(void);
DLL_SCOPE P op_readboxfile(void);
DLL_SCOPE P op_writeboxfile(void); 
DLL_SCOPE P op_transcribe(void);
DLL_SCOPE P op_tostderr(void);

#endif //DM7_H
