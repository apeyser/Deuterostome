/*====================== D machine 3.0 (Linux): dm3.c =======================

   network operators and more:

    - connect
    - disconnect
    - send
    - getsocket
    - getmyname

*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#ifndef h_errno
extern int h_errno;
#endif

#include "dm3.h"
#include "paths.h"
#include "dm-nextevent.h"
#include "dm2.h"

#define SOCK_TIMEOUT (60)

/*---------------------------- support -------------------------------------*/


// -------- delsocket ---------------------------
// After a socket has been closed, call delsocket to cleanup maxsocket
// and recsocket, and clear flag from sock_fds
void delsocket(P socketfd) {
  FD_CLR(socketfd, &sock_fds);
  if (socketfd == maxsocket-1) {
    P i, j = -1;
    for (i = 0; i < socketfd; i++)
      if (FD_ISSET(i, &sock_fds)) j = i;
    maxsocket = j+1;
  }

  if (recsocket >= maxsocket) recsocket = maxsocket-1;
  close(socketfd);
}

// -------- addsocket -----------------------------
// After opening a socket, call addsocket to increase maxsocket,
// care for recsocket, and add socket to sock_fds.
void addsocket(P socketfd) {
  FD_SET(socketfd, &sock_fds);
  if (socketfd >= maxsocket) maxsocket = socketfd+1;
  if (recsocket < 0) recsocket = socketfd;
}

//-------------------------------- set close-on-exec value for sockets

P closeonexec(P socket) {
  int oldflags = fcntl(socket, F_GETFD, 0);
  if (oldflags < 0) return -errno;
  if (fcntl(socket, F_SETFD, oldflags | FD_CLOEXEC) < 0) return -errno;

  return OK;
}

P nocloseonexec(P socket) {
  int oldflags = fcntl(socket, F_GETFD, 0);
  if (oldflags < 0) return -errno;
  if (fcntl(socket, F_SETFD, oldflags & ~FD_CLOEXEC) < 0) return -errno;

  return OK;
}

#if X_DISPLAY_MISSING

P nextXevent(void) {return OK;}
BOOLEAN moreX(void) {return FALSE;}

#else // if ! X_DISPLAY_MISSING

#include "dmx.h"
#include "xhack.h"

BOOLEAN moreX(void) {
  return dvtdisplay && HXPending(dvtdisplay);
}

static Bool checkXmatch(Display* d __attribute__ ((__unused__)), 
			XEvent* e, XPointer a) {
  XEvent* ep = (XEvent*) a;
  return (e->xany.window == ep->xany.window) && (e->type == ep->type);
}

static Bool checkXmatch_client(Display* d, XEvent* e, XPointer a) {
  XEvent* ep = (XEvent*) a;
  return checkXmatch(d, e, a)
    && ((Atom) e->xclient.message_type == (Atom) ep->xclient.message_type)
    && ((Atom) e->xclient.data.l[0] == (Atom) ep->xclient.data.l[0]);
}

DM_INLINE_STATIC void findLastXEvent(XEvent* e, 
				Bool (*p)(Display* d, XEvent* e, XPointer a)) {
  while (HXCheckIfEvent(dvtdisplay, e, p, (XPointer) e));
}

// ---------- nextXevent ---------------------------
// Call when there is an X event pending, either on 
// the xsocket or in the pending queue.
// Gets one X event from the queue
// and then, if it is a recognized X event, it calls
// the associated handler (from dnode or dvt).
// the base for the handlers are defined below (name handler_)
P nextXevent(void) {
  XEvent event;
  B* userdict;
  if (! moreX()) return OK;

  userdict = (B *)VALUE_BASE(FLOORdicts + FRAMEBYTES);
  HXNextEvent(dvtdisplay, &event);

  switch(event.type) {
    case ClientMessage:
      if ((Atom) event.xclient.message_type 
	  != HXInternAtom(dvtdisplay, "WM_PROTOCOLS", False))
	return OK;

      findLastXEvent(&event, checkXmatch_client);

      if ((Atom) event.xclient.data.l[0] 
	  == HXInternAtom(dvtdisplay, "WM_DELETE_WINDOW", False))
	return wm_delete_window(&event, userdict);

      if ((Atom) event.xclient.data.l[0]
	  == HXInternAtom(dvtdisplay, "WM_TAKE_FOCUS", False))
	return wm_take_focus(&event, userdict);

      return OK;

    case ConfigureNotify:
      findLastXEvent(&event, checkXmatch);
      return wm_configure_notify(&event, userdict);

    case Expose:
      findLastXEvent(&event, checkXmatch);
      if (event.xexpose.count) return OK;
      return wm_expose(&event, userdict);

    case ButtonPress:
      return wm_button_press(&event, userdict);

    default:
      return OK;
  }
}

// ---------- wm_take_focus_ ---------------------
// for a focus XEvent, pushes take_input_focus on exec stack
// and pushes the windows dictionary on the dict stack
P wm_take_focus_(XEvent* event, B* userdict) {
  static B namestring[NAMEBYTES];
  static B namef[FRAMEBYTES];
  P wid = event->xclient.window;
  B* dictf;

  snprintf((char*)namestring, sizeof(namestring),
	   "w%lld", (long long) wid);
  makename(namestring, namef); ATTR(namef) = ACTIVE;
  if ((dictf = lookup(namef, userdict)) == 0L) return UNDF;
  if (FREEdicts >= CEILdicts) return DICTS_OVF;
  moveframe(dictf, FREEdicts); 
  FREEdicts += FRAMEBYTES;
  if (x1 >= CEILexecs) return EXECS_OVF;
  makename((B*)"take_input_focus", x1); 
  ATTR(x1) = ACTIVE;
  FREEexecs = x2;

  return OK;
}

// ---------- wm_configure_notify_ ---------------------
// for a configure XEvent, pushes windowsize on exec stack
// and pushes the windows dictionary on the dict stack
// and pushes width, height on op stack
P wm_configure_notify_(XEvent* event, B* userdict) {
  B namestring[NAMEBYTES];
  B namef[FRAMEBYTES];
  P wid = event->xconfigure.window;
  B* dictf;

  snprintf((char*)namestring, sizeof(namestring), 
	   "w%lld", (long long) wid);
  makename(namestring, namef); 
  ATTR(namef) = ACTIVE;
        
  if (! (dictf = lookup(namef, userdict))) return UNDF;
  if (FREEdicts >= CEILdicts) return DICTS_OVF;
  if (x1 >= CEILexecs) return EXECS_OVF;
  if (o2 >= CEILopds) return OPDS_OVF;

  moveframe(dictf, FREEdicts); 
  FREEdicts += FRAMEBYTES;

  makename((B*)"windowsize",x1); 
  ATTR(x1) = ACTIVE; 
  FREEexecs = x2;

  TAG(o1) = (NUM | LONGBIGTYPE); 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = event->xconfigure.width;

  TAG(o2) = (NUM | LONGBIGTYPE); 
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = event->xconfigure.height;
  FREEopds = o3;

  return OK;
}

// ---------- wm_expose_ ---------------------
// for an expose XEvent, pushes drawwindow on exec stack
// and pushes the windows dictionary on the dict stack
P wm_expose_(XEvent* event, B* userdict) {
  static B namestring[NAMEBYTES];
  static B namef[FRAMEBYTES];
  B* dictf;
  P wid = event->xexpose.window;

  snprintf((char*)namestring, sizeof(namestring), 
	   "w%lld", (long long) wid);
  makename(namestring, namef); 
  ATTR(namef) = ACTIVE;

  if ((dictf = lookup(namef, userdict)) == 0L) return UNDF;
  if (FREEdicts >= CEILdicts) return DICTS_OVF;
  if (x1 >= CEILexecs) return EXECS_OVF;
        
  moveframe(dictf, FREEdicts); 
  FREEdicts += FRAMEBYTES;

  makename((B*)"drawwindow",x1); 
  ATTR(x1) = ACTIVE; 
  FREEexecs = x2;

  return OK;
}

// ---------- wm_take_focus_ ---------------------
// for a mouse button press XEvent, pushes mouseclick on exec stack
// and pushes the windows dictionary on the dict stack
// and x-position, y-position, mod-mask on the operand stack.
// mod-mask is the X bit mask of buttons pushed (1-12?)
P wm_button_press_(XEvent* event, B* userdict) {
  static B namestring[NAMEBYTES];
  static B namef[FRAMEBYTES];
  B* dictf;
  P wid = event->xbutton.window;
  P mod = (event->xbutton.state & 0xFF)
    | (event->xbutton.button << 16);

  if (FREEdicts >= CEILdicts) return DICTS_OVF;
  if (x1 >= CEILexecs) return EXECS_OVF;
  if (o3 >= CEILopds) return OPDS_OVF;

  snprintf((char*)namestring, sizeof(namestring), 
	   "w%lld", (long long) wid);
  makename(namestring, namef); 
  ATTR(namef) = ACTIVE;
  if ((dictf = lookup(namef, userdict)) == 0L) return UNDF;
  moveframe(dictf, FREEdicts); 
  FREEdicts += FRAMEBYTES;

  makename((B*)"mouseclick",x1); 
  ATTR(x1) = ACTIVE; 
  FREEexecs = x2;

  TAG(o1) = (NUM | LONGBIGTYPE); 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = event->xbutton.x;

  TAG(o2) = (NUM | LONGBIGTYPE);
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = event->xbutton.y;

  TAG(o3) = (NUM | LONGBIGTYPE); 
  ATTR(o3) = 0;
  LONGBIG_VAL(o3) = mod;
  FREEopds = o4;

  return OK;
}
#endif //! X_DISPLAY_MISSING

P waitsocket(BOOLEAN ispending, fd_set* out_fds) {
  static const struct timeval zerosec_ = {0, 0};
  static struct timeval zerosec;
  fd_set read_fds, err_fds;
  P nact;
  P i;
  if (! maxsocket) return NEXTEVENT_NOEVENT;

  zerosec = zerosec_;
  read_fds = sock_fds;
  err_fds = sock_fds;
  
  if ((nact = select(maxsocket, &read_fds, NULL, &err_fds, 
		     ispending ? &zerosec : NULL)) == -1) {
    if (errno == EINTR) return NEXTEVENT_NOEVENT;
    error(EXIT_FAILURE, -errno, "select");
  }

#if ! X_DISPLAY_MISSING
  if (dvtdisplay) {
    if (FD_ISSET(xsocket, &read_fds)) {
      FD_CLR(xsocket, &read_fds);
      nact--;
    }
    if (FD_ISSET(xsocket, &err_fds)) {
      FD_CLR(xsocket, &err_fds);
      nact--;
    }
  }
#endif //! X_DISPLAY_MISSING

  if (! nact) return NEXTEVENT_NOEVENT;

  *out_fds = read_fds;
  for (i = 0; i < maxsocket; ++i)
    if (FD_ISSET(i, &err_fds)) FD_SET(i, out_fds);

  return OK;
}

//------------------- read/write a block from a file descriptor
// assumes that fd is blocking
// returns within secs seconds
// uses SIGALRM internally

// broken into readfd, writefd:
//   readfd -> read from fd n bytes into where
//   writefd -> write to fd n bytes from where
// The under score versions are for use in this modules,
//   the non under score version are externally linkable.

DM_INLINE_STATIC P readfd_(P fd, B* where, P n, P secs) {
  P r, off = 0;

  alarm(secs);
  timeout = 0;
  do {
    if (timeout) return TIMER;
    switch ((r = read(fd, where+off, n))) {
      case 0: 
	if (n) return LOST_CONN;
	break;

      case -1:
	if (errno == EINTR) continue;
	return -errno;

      default:
	n -= r;
	off += r;
    }
  } while (n > 0);

  return OK;
}

P readfd(P fd, B* where, P n, P secs) {
  return readfd_(fd, where, n, secs);
}

DM_INLINE_STATIC P writefd_(P fd, B* where, P n, P secs) {
  ssize_t r, off = 0;
  alarm(secs);
  timeout = 0;
  do {
    if (timeout) return TIMER;
    if ((r = write(fd, where+off, n)) < 0) switch (errno) {
      case EINTR: continue;
      case EPIPE: return LOST_CONN;
      default: return -errno;
    }

    n -= r;
    off += r;
  } while (n > 0);

  return OK;
}

P writefd(P fd, B* where, P n, P secs) {
  return writefd_(fd, where, n, secs);
}

/*--------------------------- initialize a socket address */

P init_sockaddr(struct sockaddr_in *name, 
                const char *hostname,
                UW port)
{
  struct hostent *hostinfo;
  memset(name, 0, sizeof(struct sockaddr_in));
  name->sin_family = AF_INET;
  name->sin_port = htons(port);
  hostinfo = gethostbyname(hostname);
  if (hostinfo == 0) return(-h_errno);
  name->sin_addr = *(struct in_addr *) hostinfo->h_addr;

  return OK;
}

#if ENABLE_UNIX_SOCKETS
P init_unix_sockaddr(struct sockaddr_un *name, UW port) {
  char* sock_path = getenv("DMSOCKDIR");
  memset(name, 0, sizeof(struct sockaddr_un));
  if (! sock_path || ! *sock_path) sock_path = DMSOCKDIR;
  if (sock_path[strlen(sock_path)-1] == '/')
    sock_path[strlen(sock_path)-1] = '\0';

  name->sun_family = AF_UNIX;
  snprintf(name->sun_path, sizeof(name->sun_path)-1, "%s/dnode-%i",
           sock_path, port - DM_IPPORT_USERRESERVED);

  return OK;
}
#endif //ENABLE_UNIX_SOCKETS

/*--------------------------- make a server socket */

P make_socket(UW port)
{
  P sock;
  struct sockaddr_in name;
  memset(&name, 0, sizeof(struct sockaddr_in));

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if (sock < 0) return(-1);
  name.sin_family = AF_INET;
  name.sin_port = htons(port);
  name.sin_addr.s_addr = htonl(INADDR_ANY);
  if (bind(sock, (struct sockaddr *) &name, sizeof(name)) < 0)
    return(-1);
  return(sock);
}

#if ENABLE_UNIX_SOCKETS
typedef struct port_list {
  UW port;
  struct port_list* next;
} port_list;
static port_list* ports_first = NULL;
static port_list* ports_last = NULL;

DM_INLINE_STATIC void unlink_socketfile(void) {
  struct sockaddr_un name;
  port_list* i;
  for (i = ports_first; i; i = i->next)
    if (init_unix_sockaddr(&name, i->port) >= 0)
      unlink(name.sun_path);
}

void set_atexit_socks(P port) {
  if (! ports_first) {
    if (atexit(unlink_socketfile))
      error(EXIT_FAILURE, 0, "Can't set exit function");
    ports_last = ports_first = malloc(sizeof(port_list));
  }
  else {
    if (! (ports_last->next = malloc(sizeof(port_list))))
      error(EXIT_FAILURE, errno, "Mem alloc error");
    ports_last = ports_last->next;
  };
  ports_last->port = port;
  ports_last->next = NULL;
}

P make_unix_socket(UW port) {
  char* sock_dir; char* i;
  P sock;
  struct sockaddr_un name;
  struct stat buf;
  mode_t mask;
  
  if (init_unix_sockaddr(&name, port) != OK) return -1;
  if ((sock = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) return -1;

  mask = umask(0);
  if (! (i = sock_dir = strdup(name.sun_path))) return -1;
  while ((i = strchr(++i, '/'))) {
    *i = '\0';
    if (stat(sock_dir, &buf)) {
      if ((errno != ENOTDIR && errno != ENOENT)
          || mkdir(sock_dir, ~(mode_t) 0)) {
	fprintf(stderr, "Unable to mkdir: %s\n", sock_dir);
        free(sock_dir);
        umask(mask);
        return -1;
      }
    }
    else if (! S_ISDIR(buf.st_mode)) {
      errno = ENOTDIR;
      free(sock_dir);
      umask(mask);
      return -1;
    }
    *i = '/';
  }
  free(sock_dir);

  if (! stat(name.sun_path, &buf) && unlink(name.sun_path)) {
      fprintf(stderr, "Unable to unlink: %s\n", name.sun_path);
      umask(mask);
      return -1;
  }
    
  if (bind(sock, (struct sockaddr *) &name, 
           sizeof(name.sun_family)+strlen(name.sun_path)+1)
      < 0) {
      fprintf(stderr, "Unable to bind: %s\n", name.sun_path);
      umask(mask);
      return -1;
  }
  
  set_atexit_socks(port);

  umask(mask);
  return sock;
}
#endif

/*--------------------------- read a message from a socket
 
 The message format is:
 
   frame   of the message string
   frame   of null or box object
   string  body of message string     (padded to ALIGN)
 [ box of message     ]               (padded to ALIGN)

 The contents of a box object are appended to the VM and the
 root object is pushed on the operand stack, whereas a null object is
 discarded. The string contents are written into the string buffer object,
 and an object representing the substring in the buffer is pushed on the
 operand stack. The return code reflects several overflow conditions,
 closure of the connection  from the other side, and misformatted or
 short messages; normal returns are:

  OK   - a message was read
  DONE - an 'end of file' message was received indicating disconnection
  LOST_CONN - lost connection while receiving message
   
*/
static P socket_int;
static P wrap_readfd(B* buffer, P size) {
  return readfd_(socket_int, buffer, size, SOCK_TIMEOUT);
}


P fromsocket(P socket, B *bufferf) {
  socket_int = socket;
  return fromsource(bufferf, wrap_readfd, wrap_readfd);
}

/*------------------------------- write a message to a socket
   receives a string frame or a composite frame;
   assembles a message in free VM (see 'fromsocket' above). 
   Sends the message to the socket and returns after the complete message has
   been sent. Besides error conditions, return codes are:

    OK        - the message has been sent
    LOST_CONN - the message could not be sent due to a broken connection

    The parameters should not be pointers into the operand stack,
    but safely stored away (i.e., side effects on rootf frame should
    be discarded).
*/

static P wrap_writefd(B* buffer, P size) {
  return writefd_(socket_int, buffer, size, SOCK_TIMEOUT);
}

P tosocket(P socket, B* rootf) {
  socket_int = socket;
  return tosource(rootf, TRUE, wrap_writefd, wrap_writefd);
}

/*----------------------------------------------- connect
    servername port | socket

    NOTE: the socket is returned in the form of a NULL object of type
    SOCKETTYPE; such an object can be made or evaluated only by the
    network operators.
*/

P op_connect(void)
{
  UW port;
  LBIG port_;
  P sock, retc, size = PACKET_SIZE;
  struct sockaddr_in serveraddr;

  if (o_2 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&port_)) return(UNDF_VAL);
  port_ += DM_IPPORT_USERRESERVED;
  if (DM_IPPORT_USERRESERVED != DM_IPPORT_USERRESERVED_STANDARD)
    fprintf(stderr, 
	    "Unusual value for IPPORT_USERRESERVED: %i instead of %i\n",
	    DM_IPPORT_USERRESERVED, DM_IPPORT_USERRESERVED_STANDARD);

  if (port_ >= WMAX) return RNG_CHK;
  port = (UW) port_;
  
  if ((FREEvm + ARRAY_SIZE(o_2) + 1) > CEILvm) return(VM_OVF);
  moveB((B *)VALUE_BASE(o_2),FREEvm,ARRAY_SIZE(o_2));
  FREEvm[ARRAY_SIZE(o_2)] = '\000';
  
#if ENABLE_UNIX_SOCKETS
  {
    struct sockaddr_un unixserveraddr;
    if (! strcmp("localhost", (char*)FREEvm)
        && init_unix_sockaddr(&unixserveraddr, port) == OK
        && (sock = socket(PF_UNIX, SOCK_STREAM, 0)) != -1) {
      if (connect(sock, (struct sockaddr *) &unixserveraddr, 
                  sizeof(unixserveraddr.sun_family)
                  + strlen(unixserveraddr.sun_path)))
        close(sock);
      else goto goodsocket;
    };
  };
#endif //ENABLE_UNIX_SOCKETS

  if ((retc = init_sockaddr(&serveraddr, (char*)FREEvm, port)) != OK) 
    return retc;
  if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1) return -errno;
  if (setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(P)) == -1
      || setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(P)) == -1
      || connect(sock, (struct sockaddr *)&serveraddr,
                 sizeof(serveraddr)) == -1) {
    int errno_ = errno;
    close(sock);
    return -errno_;
  };
  if ((retc = closeonexec(sock))) {
    close(sock);
    return retc;
  }
  
 goodsocket:
  //  if (fcntl(sock, F_SETFL, O_NONBLOCK) == -1)   /* make non-blocking  */
  //  error(EXIT_FAILURE, errno, "fcntl");
  addsocket(sock);
  TAG(o_2) = NULLOBJ | SOCKETTYPE; ATTR(o_2) = 0;
  LONGBIG_VAL(o_2) = sock;
  FREEopds = o_1;
  return OK;
}


/*----------------------------------------------- disconnect
    socket | --
*/

P op_disconnect(void)
{
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_1) != (NULLOBJ | SOCKETTYPE)) return(OPD_ERR);
  delsocket((P) LONGBIG_VAL(o_1));
  FREEopds = o_1;
  return(OK);
}

/*----------------------------------------------- send
    socket (string) | --
    socket [ num/\* bool? rootobj (string) ] | --

    op_send defined in dm-nextevent.c
    since op_send must handle calling makesocketdead.
    In case of error, returns the active fd as *socketfd.
*/

P op_send(void)
{
  P retc; 
  static B rootf[FRAMEBYTES];
  P socketfd;

  if (o_2 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_2) != (NULLOBJ | SOCKETTYPE)) return(OPD_ERR);
  socketfd = (P) LONGBIG_VAL(o_2);

  switch (CLASS(o_1)) {
    case ARRAY:
      if (TYPE(o_1) == BYTETYPE) ATTR(o_1) |= ACTIVE;
      // fall through intentional
    case DICT: case LIST: case NULLOBJ: case NUM: case NAME: case OP:
    case BOOL:
      moveframe(o_1, rootf);
      break;

    default:
      return OPD_CLA;
  }

  if (! (ATTR(rootf) & ACTIVE)) return OPD_ATR;
	  
  if ((retc = tosocket(socketfd, rootf)))
    return makesocketdead(retc, socketfd, "send");
 
  FREEopds = o_2; 
  return OK;
}

/*------------------------------------------- getsocket
    -- | socket

The socket is opaquely encoded in a null object of type socket.
*/

P op_getsocket(void)
{
  if (o1 >= CEILopds) return(OPDS_OVF);
  TAG(o1) = NULLOBJ | SOCKETTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = recsocket;
  FREEopds = o2;
  return(OK);
}

/*------------------------------------------- getmyname
  -- | string

  returns the host's name
*/

P op_getmyname(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  if (! myname_frame) return CORR_OBJ;
  moveframe(myname_frame, o1);
  FREEopds = o2;
  return OK;
}

/*------------------------------------------- getmyfqdn
    -- | string

    returns the host's name
*/

P op_getmyfqdn(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  if (! myfqdn_frame) return CORR_OBJ;
  moveframe(myfqdn_frame, o1);
  FREEopds = o2;
  return OK;
}
