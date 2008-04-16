#ifndef DM3_H
#define DM3_H

#include "dm.h"

DLL_SCOPE void makeDmemory(B *mem, L64 specs[5]);
DLL_SCOPE P make_socket(UW port);
DLL_SCOPE P make_unix_socket(UW port);
DLL_SCOPE P fromsocket(P socket, B* stringbuffer);
DLL_SCOPE P tosocket(P socket, B* rootf);
DLL_SCOPE P toconsole(B *string, P stringlength);

DLL_SCOPE P op_connect(void);
DLL_SCOPE P op_disconnect(void);
DLL_SCOPE P int_send(P* socketfd);
DLL_SCOPE P op_getsocket(void);
DLL_SCOPE P op_getmyname(void);
DLL_SCOPE P op_getmyfqdn(void);

DLL_SCOPE P closeonexec(P socket);
DLL_SCOPE P nocloseonexec(P socket);
DLL_SCOPE void addsocket(P socketfd);
DLL_SCOPE void delsocket(P socketfd);

DLL_SCOPE P readfd(P fd, B* where, P n, P secs);
DLL_SCOPE P writefd(P fd, B* where, P n, P secs);

#endif //DM3_H
