#ifndef PLUGINLIB_H
#define PLUGINLIB_H

#if __cplusplus
extern "C" {
#endif

#define NO_PLUGINS PLUGIN_ERRS

void initialize_plugins(void);
void closealllibs(void);

#include "dm.h"

// for LL
P op_getplugindir(void);

#if DM_ENABLE_PLUGINS_SUPPORT

BOOLEAN check_opaque_name(B* nameframe, B* dict);
// ... = null terminated list of nameframes to insert
// first set to be null objects -- you must initialize them OPAQUE_MEM_SET
B* make_opaque_frame(P n, B* pluginnameframe, ...);

//globals -- used only in plugins and dnode_1.h
extern B opaquename[FRAMEBYTES];
extern B saveboxname[FRAMEBYTES];
extern B fininame[FRAMEBYTES];
extern B initname[FRAMEBYTES];
// name frame for allocated buffer with opaque object
extern B buffernameframe[FRAMEBYTES];

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
