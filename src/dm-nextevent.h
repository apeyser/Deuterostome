#ifndef DM_NEXTEVENT_H
#define DM_NEXTEVENT_H

#include "dm.h"

DLL_SCOPE P nextevent(B* buffer);
DLL_SCOPE P makesocketdead(P retc, P socketfd, B* error_source);
DLL_SCOPE void noMoreX(void);
DLL_SCOPE P closeonexec(P socketfd);
DLL_SCOPE P nocloseonexec(P socketfd);

DLL_SCOPE P op_send(void);

#if ! DM_X_DISPLAY_MISSING

DLL_SCOPE P wm_take_focus_(XEvent* event, B* userdict);
DLL_SCOPE P wm_configure_notify_(XEvent* event, B* userdict);
DLL_SCOPE P wm_expose_(XEvent* event, B* userdict);
DLL_SCOPE P wm_button_press_(XEvent* event, B* userdict);

// everything below here is defined in the main modules (such as dvt or dnode)
// to be available in dm-nextevent.c
DLL_SCOPE P wm_delete_window(XEvent* event, B* userdict);
DLL_SCOPE P wm_take_focus(XEvent* event, B* userdict);
DLL_SCOPE P wm_configure_notify(XEvent* event, B* userdict);
DLL_SCOPE P wm_expose(XEvent* event, B* userdict);
DLL_SCOPE P wm_button_press(XEvent* event, B* userdict);

#endif //! DM_X_DISPLAY_MISSING

DLL_SCOPE BOOLEAN serverinput(P* retc, fd_set* read_fds);
DLL_SCOPE BOOLEAN consoleinput(P* retc, B* bufferf);
DLL_SCOPE P clientinput(void);
DLL_SCOPE BOOLEAN pending(void);
DLL_SCOPE void makeerror(P retc, B* error_source);

#endif //DM_NEXTEVENT_H
