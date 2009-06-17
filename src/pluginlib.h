#ifndef PLUGINLIB_H
#define PLUGINLIB_H

#include "dm.h"

#if __cplusplus
extern "C" {
#endif

DLL_SCOPE void initialize_plugins(void);
DLL_SCOPE void closealllibs(void);

#include "dm.h"
#include "dm2.h"

// for LL
DLL_SCOPE P op_loadlib(void);
DLL_SCOPE P op_nextlib(void);

#if DM_ENABLE_PLUGINS_SUPPORT

DLL_SCOPE BOOLEAN check_opaque_name(B* nameframe, B* dict);
// ... = null terminated list of nameframes to insert
// first set to be null objects -- you must initialize them OPAQUE_MEM_SET
DLL_SCOPE B* make_opaque_frame(P n, B* pluginnameframe, ...);

//globals -- used only in plugins and dnode_1.h
DLL_SCOPE B opaquename[FRAMEBYTES];
DLL_SCOPE B saveboxname[FRAMEBYTES];
DLL_SCOPE B fininame[FRAMEBYTES];
DLL_SCOPE B initname[FRAMEBYTES];
// name frame for allocated buffer with opaque object
DLL_SCOPE B buffernameframe[FRAMEBYTES];

#define INNERP_VAL(frame)  (*(P*)PF_PTR(frame,1))
#if DM_HOST_IS_32_BIT
#define INNERPTYPE LONG32TYPE
#else
#define INNERPTYPE LONG64TYPE
#endif //DM_HOST_IS_32_BIT

#endif //DM_ENABLE_PLUGINS_SUPPORT

#if __cplusplus
}
#endif		

#endif //PLUGINLIB_H
