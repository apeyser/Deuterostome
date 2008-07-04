#ifndef DM_PROP_H
#define DM_PROP_H

#include "dm.h"
#include <sys/un.h>

DLL_SCOPE void spawnsighandler(P sigsocket, P serverport,
			       void (*set_unixowner_func)(BOOLEAN state),
			       void (*closesockets_func)(void));

#if ENABLE_UNIX_SOCKETS
DLL_SCOPE P init_unix_sockaddr(struct sockaddr_un *name, 
			       UW port, BOOLEAN isseq);
#endif //ENABLE_UNIX_SOCKETS

DLL_SCOPE UW getportoffset(void);


#endif //DM_PROP_H
