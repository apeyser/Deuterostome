#ifndef DM_PROP_H
#define DM_PROP_H

#include "dm.h"
#include <sys/un.h>

DLL_SCOPE P spawnsighandler(P sigsocket, P serverport,
			    P (*closesockets_func)(void),
			    P* pid);
DLL_SCOPE UW getportoffset(void);

#if ENABLE_UNIX_SOCKETS
DLL_SCOPE P init_unix_sockaddr(struct sockaddr_un *name, 
			       UW port, BOOLEAN isseq);
#endif //ENABLE_UNIX_SOCKETS

#endif //DM_PROP_H
