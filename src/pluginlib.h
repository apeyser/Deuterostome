#ifndef PLUGINLIB_H
#define PLUGINLIB_H

void initialize_plugins(void);
void closealllibs(void);

#include "dm.h"

#ifdef PLUGINS_ENABLED

BOOLEAN check_opaque_name(B* nameframe, B* dict);
// ... = null terminated list of nameframes to insert
// first set to be null objects -- you must initialize them OPAQUE_MEM_SET
B* make_opaque_frame(L n, B* pluginnameframe, ...);

//globals -- used only in plugins and dnode_1.h
extern B opaquename[FRAMEBYTES];
extern B saveboxname[FRAMEBYTES];
extern B fininame[FRAMEBYTES];
extern B initname[FRAMEBYTES];
// name frame for allocated buffer with opaque object
extern B buffernameframe[FRAMEBYTES];

#endif //PLUGINS_ENABLED

#endif //PLUGINLIB_H
