/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
#ifndef DM_NOTERM_H
#define DM_NOTERM_H

#include "dm.h"

DLL_SCOPE P op_toconsole(void);
DLL_SCOPE BOOLEAN pending(void);
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
DLL_SCOPE void clearsocket_special(P fd);

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
