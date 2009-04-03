#ifndef DM3_H
#define DM3_H

#include "dm.h"
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>

DLL_SCOPE P make_socket(UW port, BOOLEAN isseq, P* retc);
DLL_SCOPE P make_unix_socket(UW port, BOOLEAN isseq, P* retc);
DLL_SCOPE P toconsole(B *string, P stringlength);
DLL_SCOPE P fromsocket(P socket, B* buffer);
DLL_SCOPE P tosocket(P socket, B* rootf);

DLL_SCOPE P op_connect(void);
DLL_SCOPE P op_disconnect(void);
DLL_SCOPE P op_send(void);
DLL_SCOPE P op_sendsig(void);
DLL_SCOPE P op_getsocket(void);
DLL_SCOPE P op_getmyname(void);
DLL_SCOPE P op_getmyfqdn(void);

DLL_SCOPE P readfd(P fd, B* where, P n, P secs);
DLL_SCOPE P writefd(P fd, B* where, P n, P secs);

DLL_SCOPE P closeonexec(P socketfd);
DLL_SCOPE P nocloseonexec(P socketfd);

DLL_SCOPE P addsocket(P socketfd, 
		      P unixserverport, 
		      BOOLEAN protected,
		      BOOLEAN listener,
		      P sigfd);
DLL_SCOPE P delsocket(P socketfd);
DLL_SCOPE void closesockets(void);
DLL_SCOPE void set_closesockets_atexit(void);
DLL_SCOPE void set_unixowner(BOOLEAN state);
DLL_SCOPE P dm_setsockopts(P fd, P size);

DLL_SCOPE P init_sockaddr(struct sockaddr_in *name, 
			  const char *hostname, 
			  UW port);

DLL_SCOPE void forksighandler(P sigsocket, P serverport);


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

#endif //DM3_H
