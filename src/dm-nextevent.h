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
#ifndef DM_NEXTEVENT_H
#define DM_NEXTEVENT_H

#include "dm.h"

#include <sys/select.h>

DLL_SCOPE P nextevent(B* buffer);
DLL_SCOPE P makesocketdead(P retc, P socketfd, B* error_source);
DLL_SCOPE P op_console(void);
DLL_SCOPE P op_setconsole(void);
DLL_SCOPE P op_send(void);

// There are defined externally
DLL_SCOPE BOOLEAN masterinput(P* retc, B* bufferf);
DLL_SCOPE P clientinput(void);
DLL_SCOPE BOOLEAN pending(void);
DLL_SCOPE void makeerror(P retc, B* error_source);

DLL_SCOPE P waitsocket(BOOLEAN ispending, fd_set* out_fds);
DLL_SCOPE P fromsocket(P socket, B* buffer);
DLL_SCOPE P nextXevent(void);
DLL_SCOPE BOOLEAN moreX(void);

#endif //DM_NEXTEVENT_H
