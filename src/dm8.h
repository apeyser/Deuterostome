#ifndef DM_8_H
#define DM_8_H

#include "dm.h"

DM_HOT DLL_SCOPE P op_fax(void);
DLL_SCOPE P op_merge(void);
DLL_SCOPE P op_nextobject(void);
DLL_SCOPE P (*usedfd_func)(void);
DM_HOT DLL_SCOPE P op_used(void);
DM_HOT DLL_SCOPE P op_length(void); 
DM_HOT DLL_SCOPE P op_last(void);

#endif //DM_8_H
