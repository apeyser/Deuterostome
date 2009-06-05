#ifndef DM_NOTERM_H
#define DM_NOTERM_H

#include "dm.h"

DLL_SCOPE P op_socketdead(void);
DLL_SCOPE P op_toconsole(void);
DLL_SCOPE BOOLEAN pending(void);
DLL_SCOPE void setpending(void);
DLL_SCOPE P clientinput(void);
DLL_SCOPE P op_killsockets(void);
DLL_SCOPE P op_error(void);
DLL_SCOPE P op_errormessage(void);
DLL_SCOPE P op_Xconnect(void);
DLL_SCOPE P op_Xdisconnect(void);
DLL_SCOPE P op_getmyport(void);
DLL_SCOPE P op_Xwindows_(void);
DLL_SCOPE P op_vmresize(void);

DLL_SCOPE BOOLEAN masterinput(P* retc, B* bufferf);

#if ! X_DISPLAY_MISSING
DLL_SCOPE P wm_delete_window(XEvent* event, B* userdict);
DLL_SCOPE P wm_take_focus(XEvent* event, B* userdict);
DLL_SCOPE P wm_configure_notify(XEvent* event, B* userdict);
DLL_SCOPE P wm_expose(XEvent* event, B* userdict);
DLL_SCOPE P wm_button_press(XEvent* event, B* userdict);
#endif //! X_DISPLAY_MISSING

DLL_SCOPE P serverport;
DLL_SCOPE B hostname[256];

DLL_SCOPE void run_dnode_mill(void);

DLL_SCOPE P tosocket(P fd, B* buffer);
DLL_SCOPE void makeerror(P retc, B* error_source);


#endif //DM_NOTERM_H
