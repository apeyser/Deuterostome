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
#ifndef DM_PROP_H
#define DM_PROP_H

#include "dm.h"
#include <sys/un.h>

DLL_SCOPE P spawnsighandler(P sigsocket, P tcp_sigsocket, P serverport,
			    P (*closesockets_func)(void),
			    P* pid);
DLL_SCOPE UW getportoffset(void);

#if ENABLE_UNIX_SOCKETS
DLL_SCOPE P init_unix_sockaddr(struct sockaddr_un *name, 
			       UW port, BOOLEAN isseq);
#endif //ENABLE_UNIX_SOCKETS

#endif //DM_PROP_H
