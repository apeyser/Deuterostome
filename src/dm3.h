#ifndef DM3_H
#define DM3_H

#include "dm.h"

void makeDmemory(B *mem, L64 specs[5]);
P make_socket(UW port);
P make_unix_socket(UW port);
P fromsocket(P socket, B *msf);
P tosocket(P socket, B *sf, B *cf);
P toconsole(B *string, P stringlength);

P op_connect(void);
P op_disconnect(void);
P op_send(void);
P op_getsocket(void);
P op_getmyname(void);
P op_getmyfqdn(void);

#endif //DM3_H
