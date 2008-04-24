#ifndef DM_DVT_H
#define DM_DVT_H

#include "dm.h"

DLL_SCOPE P toconsole(B *p, P atmost);
DLL_SCOPE P op_error(void);
DLL_SCOPE P op_errormessage(void);
DLL_SCOPE P op_aborted(void);
DLL_SCOPE P op_abort(void);
DLL_SCOPE P op_toconsole(void);
DLL_SCOPE P op_nextevent(void);
DLL_SCOPE BOOLEAN pending(void);
DLL_SCOPE BOOLEAN masterinput(P* retc, B* bufferf);
DLL_SCOPE void makeerror(P retc, B* error_socket);
DLL_SCOPE void run_dvt_mill(void) __attribute__ ((__noreturn__));

#if ! DM_X_DISPLAY_MISSING
DLL_SCOPE P wm_delete_window(XEvent* event, B* userdict);
DLL_SCOPE P wm_take_focus(XEvent* event, B* userdict);
DLL_SCOPE P wm_configure_notify(XEvent* event, B* userdict);
DLL_SCOPE P wm_expose(XEvent* event, B* userdict);
DLL_SCOPE P wm_button_press(XEvent* event, B* userdict);
#endif //! DM_X_DISPLAY_MISSING


#endif //DM_DVT_H
