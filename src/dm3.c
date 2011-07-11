/*====================== D machine 3.0 (Linux): dm3.c =======================

   network operators and more:

    - connect
    - disconnect
    - send
    - getsocket
    - getmyname

*/

#define DEBUG_ACTIVE 0
#include "dm.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>

#ifndef h_errno
extern int h_errno;
#endif

#include "dm3.h"
#include "paths.h"
#include "dm-nextevent.h"
#include "dm2.h"
#include "dm-vm.h"
#include "dmx.h"
#include "xhack.h"
#include "dm-signals.h"
#include "dm-prop.h"
#include "error-local.h"

#define SOCK_TIMEOUT (60)

/*---------------------------- support -------------------------------------*/

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
  if (! hostinfo) return -h_errno;
  name->sin_addr = *(struct in_addr *) hostinfo->h_addr;

  return OK;
}

static struct socketstore {
  int fd;
  union SocketInfo info;
  struct SocketType type;
  P pid;
  P redirector;
  struct socketstore* next;
  struct socketstore* last;
} *socketstore_tail = NULL, *socketstore_head = NULL;

//-------------------------------- set close-on-exec value for sockets
P closeonexec_simple(P fd) {
  int oldflags;
  if ((oldflags = fcntl(fd, F_GETFD, 0)) < 0) return -errno;
  if (fcntl(fd, F_SETFD, oldflags | FD_CLOEXEC) < 0) return -errno;
  return OK;
}

P closeonexec(P fd) {
  P retc;
  struct socketstore* next;

  if ((retc = closeonexec_simple(fd))) return retc;

  for (next = socketstore_head; next; next = next->next)
    if (next->fd == fd) {
      if (next->type.listener && next->info.listener.sigfd != -1
	  && (retc = closeonexec_simple(next->info.listener.sigfd)))
	return retc;
      break;
    }

  return OK;
}

P nocloseonexec_simple(P fd) {
  int oldflags;
  if ((oldflags = fcntl(fd, F_GETFD, 0)) < 0) return -errno;
  if (fcntl(fd, F_SETFD, oldflags & ~FD_CLOEXEC) < 0) return -errno;
  return OK;
}

P nocloseonexec(P fd) {
  P retc;
  struct socketstore* next;

  if ((retc = nocloseonexec_simple(fd))) return retc;
  
  for (next = socketstore_head; next; next = next->next)
    if (next->fd == fd) {
      if (next->type.listener && next->info.listener.sigfd != -1
	  && (retc = nocloseonexec_simple(next->info.listener.sigfd)))
	return retc;
      break;
    }

  return OK;
}

void clearsocket(P fd) {
  DEBUG("clearsocket %li", (long) fd);
  clearsocket_special(fd);
  FD_CLR((int) fd, &sock_fds);
  if (fd == maxsocket-1) {
    P i, j = -1;
    for (i = 0; i < fd; i++)
      if (FD_ISSET(i, &sock_fds)) j = i;
    maxsocket = j+1;
  }

  if (recsocket >= maxsocket) recsocket = maxsocket-1;

#if ! DM_X_DISPLAY_MISSING
  if (fd == xsocket) {
    xsocket = -1;
    dvtdisplay = NULL;
  }
#endif // ! DM_X_DISPLAY_MISSING
}

enum _DelMode {
  _DelModeFork,
  _DelModeExec,
  _DelModeForce,
  _DelModeResize,
  _DelModeProc,
  _DelModeCleanup,
};

DM_INLINE_STATIC void sockprintdebug(const char* mode, 
				     struct socketstore* sock) 
{
  DEBUG("%s: socket %li: f %s, "
	"e %s, l %s, r %s, r %li, p %li, rsig %li, trsig %li, esig %li, "
	"u %li\n",
	mode,
	(long) sock->fd,
	sock->type.fork ? "t" : "f",
	sock->type.exec ? "t" : "f",
	sock->type.listener ? "t" : "f",
	sock->type.resize ? "t" : "f",
	(long) sock->redirector,
	(long) sock->pid,
	sock->type.listener ? (long) sock->info.listener.recsigfd : -1,
	sock->type.listener ? (long) sock->info.listener.trecsigfd : -1,
	sock->type.listener ? (long) sock->info.listener.sigfd : -1,
	sock->type.listener ? (long) sock->info.listener.unixport : -1);
}

DM_INLINE_STATIC P _opendevnull(int fd, int flags) {
  int nfd;
  if ((nfd  = open("/dev/null", flags)) < 0) return -errno;
  if (dup2(nfd, fd) < 0) return -errno;
  if (close(nfd)) return -errno;

  return nocloseonexec_simple(fd);
}

// -------- delsocket ---------------------------
// After a socket has been closed, call delsocket to cleanup maxsocket
// and recsocket, and clear flag from sock_fds

DM_INLINE_STATIC P _delsocket(P fd, enum _DelMode delmode) {
  struct socketstore* next;
  P retc = OK;
  pid_t mypid = getpid();

  for (next = socketstore_head; next; next = next->next) 
    if (next->fd == fd) {
      switch (delmode) {
	case _DelModeForce: 
	  break;
	case _DelModeCleanup:
	  if (fd == DM_STDERR_FILENO) return OK;
	  break;
	case _DelModeFork:
	  if (! next->type.fork) return OK;
	  break;
	case _DelModeExec:
	  if (! next->type.exec) return OK;
	  break;
	case _DelModeResize:
	  if (! next->type.resize) return OK;
	  break;
	case _DelModeProc:
	  if (! next->type.proc) return OK;
	  break;
      };
      sockprintdebug("close", next);
      
      switch (next->type.stdin) {
	case  0: if ((retc = _opendevnull(fd, O_WRONLY))) return retc; break;
	case  1: if ((retc = _opendevnull(fd, O_RDONLY))) return retc; break;
	case -1: 
	  while (close(fd)) {
	    if (errno != EINTR) {
	      retc = -errno;
	      break;
	    };
	    checkabort();
	  };
	  break;
      };

      if (next->type.listener) {
	clearsocket(fd);
	if (next->info.listener.sigfd != -1)
	  close(next->info.listener.sigfd);
	if (next->info.listener.recsigfd != -1)
	  close(next->info.listener.recsigfd);
	if (next->info.listener.trecsigfd != -1)
	  close(next->info.listener.trecsigfd);
	if (mypid == next->pid) {
	  if (next->redirector != -1) {
	    if (kill(next->redirector, SIGQUIT)) {
	      dm_error_msg(errno, "Unable to kill redirector %li",
			   (long) next->redirector);
	      if (! retc) retc = -errno;
	    }
	    else while (waitpid((pid_t) next->redirector, NULL, 0) == -1) {
		if (errno == EINTR) {
		  if (abortflag || recvd_quit) break;
		}
		else {
		  if (! retc) retc = -errno;
		  break;
		}
	      }
	  }
#if ENABLE_UNIX_SOCKETS
	  if (next->info.listener.unixport >= 0) {
	    struct sockaddr_un name;
	    if (init_unix_sockaddr(&name, next->info.listener.unixport, TRUE))
	      if (unlink(name.sun_path) && ! retc) retc = -errno;
	  }
#endif //ENABLE_UNIX_SOCKETS
	}
      }

      if (! next->next)
	socketstore_tail = next->last;
      else 
	next->next->last = next->last;
      if (! next->last)
	socketstore_head = next->next;
      else
	next->last->next = next->next;
      free(next);
      break;
    }
  
  return retc;
}

P delsocket_fork(P fd) {
  return _delsocket(fd, _DelModeFork);
}

P delsocket_exec(P fd) {
  return _delsocket(fd, _DelModeExec);
}

P delsocket_force(P fd) {
  return _delsocket(fd, _DelModeForce);
}

P delsocket_proc(P fd) {
  return _delsocket(fd, _DelModeProc);
}

/****************** socketdead ************************
 * ... bool socket | --
 * 
 * Default socket dead. If bool is true, then ... is the
 *  stack for an error. Overridden by startup files.
 */
P op_socketdead(void) {
  P retc;

  if (o_2 < FLOORopds) return DEAD_SOCKET;
  if (CLASS(o_2) != BOOL) return DEAD_SOCKET;

  if ((retc = op_disconnect())) return retc;

  FREEopds = o_1;
  if (BOOL_VAL(o1)) {
    if (x1 >= CEILexecs) return EXECS_OVF;
    makename((B*) "error", x1);
    ATTR(x1) |= ACTIVE;
    FREEexecs = x2;
    return OK;
  }

  return DEAD_SOCKET;
}

// -------- addsocket -----------------------------
// After opening a socket, call addsocket to increase maxsocket,
// care for recsocket, and add socket to sock_fds.
P addsocket(P fd, const struct SocketType* type, const union SocketInfo* info) {
  P retc;
  struct socketstore* next;

  for (next = socketstore_head; next; next = next->next)
    if (next->fd == fd) {
      if (type->listener || next->type.listener)
	return SOCK_STATE;
      return OK;
    }

  if (! (next
	 = (struct socketstore*) malloc(sizeof(struct socketstore))))
    dm_error(errno, "Malloc failure creating socketstore");

  next->fd = fd;
  next->type = *type;
  if (info) next->info = *info;
  next->pid = getpid();
  next->redirector = -1;
  next->next = NULL;
  next->last = socketstore_tail;
  if (socketstore_tail) socketstore_tail->next = next;
  else                  socketstore_head = next;
  socketstore_tail = next;

  if (! type->fork) {
    if ((retc = nocloseonexec(fd))) {
      delsocket_force(fd);
      return retc;
    }
  }
  else if ((retc = closeonexec(fd))) {
    delsocket_force(fd);
    return retc;
  }

  if (type->listener) {
    DEBUG("add: %li", fd);
    FD_SET(fd, &sock_fds);
    if (fd >= maxsocket) maxsocket = fd+1;
    if (recsocket < 0) recsocket = fd;

    if (info->listener.recsigfd != -1 || info->listener.trecsigfd != -1) {
      if ((retc = forksighandler(info->listener.recsigfd,
				 info->listener.trecsigfd,
				 info->listener.unixport, 
				 &next->redirector))) {
	delsocket_force(fd);
	return retc;
      }
      next->info.listener.recsigfd = -1;
      next->info.listener.trecsigfd = -1;
    }
  }

  sockprintdebug("open", next);
  return OK;
}

DM_INLINE_STATIC P _closesockets(enum _DelMode delmode) {
  int retc = OK, retc_;
  struct socketstore* next = socketstore_head;
  while (next) {
    struct socketstore* last = next;
    int fd = last->fd;
    next = next->next;
    if ((retc_ = _delsocket(fd, delmode))){
      if (! retc) retc = retc_;
      dm_error_msg(retc < 0 ? -retc : 0,
		   "Deleting socket %li, forking %s, "
		   "execing %s, force %s, resize %s, cleanup %s",
		   (long) fd,
		   delmode == _DelModeFork ? "yes" : "no",
		   delmode == _DelModeExec ? "yes" : "no",
		   delmode == _DelModeForce ? "yes" : "no",
		   delmode == _DelModeResize ? "yes" : "no",
		   delmode == _DelModeCleanup ? "yes" : "no");
    }
  }
  return retc;
}

P closesockets_force(void) {
  return _closesockets(_DelModeForce);
}

P closesockets_proc(void) {
  return _closesockets(_DelModeProc);
}

P closesockets_exec(void) {
  return _closesockets(_DelModeExec);
}

P closesockets_fork(void) {
  return _closesockets(_DelModeFork);
}

P closesockets_resize(void) {
  return _closesockets(_DelModeResize);
}

P closesockets_cleanup(void) {
  return _closesockets(_DelModeCleanup);
}

void closedisplay(void) {
#if ! X_DISPLAY_MISSING
  if (dvtdisplay) {
    DEBUG("close display%s", "");
    HXCloseDisplay(dvtdisplay);
    dvtdisplay = NULL;
  }
  if (xsocket != -1) {
    delsocket_force(xsocket);
    xsocket = -1;
  }
  displayname[0] = '\0';
  xkbext = 0;
#endif
}

static void _closesockets_cleanup(void) {
  closesockets_cleanup();
}

void set_closesockets_atexit(void) {
  if (atexit(_closesockets_cleanup))
    dm_error(-errno, "Setting atexit closesockets");
  if (atexit(closedisplay))
    dm_error(-errno, "Setting atexit closedisplay");
}

#if X_DISPLAY_MISSING

P nextXevent(void) {return OK;}
BOOLEAN moreX(void) {return FALSE;}

#else // if ! X_DISPLAY_MISSING

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
  makename(namestring, namef);
  ATTR(namef) = ACTIVE;

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

  DEBUG("select max: %li", maxsocket);
  if (DEBUG_ACTIVE)
    for (i = 0; i < maxsocket; i++) {
      read_fds = sock_fds;
      if (FD_ISSET(i, &read_fds)) {
	struct timeval onesec = {0, 0};
	DEBUG("select: %li", i);
	FD_ZERO(&read_fds);
	FD_SET(i, &read_fds);
	err_fds = read_fds;
	if (select(i+1, &read_fds, NULL, &err_fds, &onesec) == -1)
	  DEBUG("bad socket: %li", i);
      }
    }

  zerosec = zerosec_;
  read_fds = sock_fds;
  err_fds = sock_fds;
  
  if ((nact = select(maxsocket, &read_fds, NULL, &err_fds, 
		     ispending ? &zerosec : NULL)) == -1) {
    if (errno == EINTR) return NEXTEVENT_NOEVENT;
    dm_error(errno, "select");
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

DM_INLINE_STATIC P readfd_(P fd, B* where, P n, 
			   P secs __attribute__ ((__unused__)) ) {
  P r, off = 0;

  //  alarm(secs);
  //  timeout = 0;
  do {
    //if (timeout) return TIMER;
    switch ((r = read(fd, where+off, n))) {
      case 0: 
	checkabort();
	if (n) return LOST_CONN;
	break;

      case -1:
	if (errno != EINTR) return -errno;
	checkabort();
	continue;

      default:
	if (r < n) checkabort();
	n -= r;
	off += r;
    }
  } while (n > 0);

  return OK;
}

P readfd(P fd, B* where, P n, P secs) {
  return readfd_(fd, where, n, secs);
}

DM_INLINE_STATIC P writefd_(P fd, B* where, P n, 
			    P secs __attribute__ ((__unused__)) ) {
  ssize_t r, off = 0;
  //  alarm(secs);
  //timeout = 0;
  do {
    //if (timeout) return TIMER;
    if ((r = write(fd, where+off, n)) < 0) switch (errno) {
      case EINTR: checkabort(); continue;
      case EPIPE: return LOST_CONN;
      default: return -errno;
    }

    if (r < n) checkabort();
    n -= r;
    off += r;
  } while (n > 0);

  return OK;
}

P writefd(P fd, B* where, P n, P secs) {
  return writefd_(fd, where, n, secs);
}

/*--------------------------- make a server socket */

P make_socket(UW* port, BOOLEAN tcp, P packet_size, P* retc)
{
  P sock;
  struct sockaddr_in name;
  socklen_t s = sizeof(name);;
  memset(&name, 0, sizeof(struct sockaddr_in));

  if ((sock = socket(PF_INET, tcp ? SOCK_STREAM : SOCK_DGRAM, 0)) < 0) {
    *retc = -errno;
    return -1;
  }

  if ((*retc = dm_setsockopts(sock, packet_size)))
    return -1;

  name.sin_family = AF_INET;
  name.sin_port = htons(*port);
  name.sin_addr.s_addr = htonl(INADDR_ANY);
  if (bind(sock, (struct sockaddr *) &name, s) < 0) {
    *retc = -errno;
    return -1;
  }

  if (tcp && listen(sock, 5) < 0) {
    *retc = -errno;
    close(sock);
    return -1;
  }

  if (getsockname(sock, (struct sockaddr*) &name, &s) < 0) {
    *retc = -errno;
    return -1;
  }

  *port = name.sin_port;
  DEBUG("made %i: (%i, %i), %s, %li", 
	(int) sock, (int) *port, (int) ntohs(*port),
	tcp ? "tcp" : "udp", (long) packet_size);
  return sock;
}

#if ENABLE_UNIX_SOCKETS

P make_unix_socket(UW port, BOOLEAN isseq, P* retc) {
  char* sock_dir; char* i;
  P sock;
  struct sockaddr_un name;
  struct stat buf;
  mode_t mask;
  
  if ((*retc = init_unix_sockaddr(&name, port, isseq))) 
    return -1;
  if ((sock = socket(PF_UNIX, isseq ? SOCK_STREAM : SOCK_DGRAM, 0)) < 0) {
    *retc = -errno;
    return -1;
  }

  if ((*retc = dm_setsockopts(sock, isseq ? PACKET_SIZE : 1)))
    return -1;

  mask = umask(0);
  if (! (i = sock_dir = strdup(name.sun_path))) {
    close(sock);
    return -1;
  }

  while ((i = strchr(++i, '/'))) {
    *i = '\0';
    if (stat(sock_dir, &buf)) {
      if ((errno != ENOTDIR && errno != ENOENT)
          || mkdir(sock_dir, ~(mode_t) 0)) {
	dm_error_msg(errno, "Unable to mkdir: %s", sock_dir);
        free(sock_dir);
        umask(mask);
	close(sock);
        return -1;
      }
    }
    else if (! S_ISDIR(buf.st_mode)) {
      errno = ENOTDIR;
      free(sock_dir);
      umask(mask);
      close(sock);
      return -1;
    }
    *i = '/';
  }
  free(sock_dir);

  if (! stat(name.sun_path, &buf) && unlink(name.sun_path)) {
    dm_error_msg(errno, "Unable to unlink: %s", name.sun_path);
    umask(mask);
    close(sock);
    return -1;
  }
    
  if (bind(sock, (struct sockaddr *) &name, 
           sizeof(name.sun_family)+strlen(name.sun_path)+1)
      < 0) {
    dm_error_msg(errno, "Unable to bind: %s", name.sun_path);
    umask(mask);
    close(sock);
    return -1;
  }
  
  if (isseq) {
    if (listen(sock, 5) < 0) {
      *retc = -errno;
      close(sock);
      unlink(name.sun_path);
      return -1;
    }
  }

  umask(mask);
  DEBUG("made unix %i: %i, %s", 
	(int) sock, (int) port, 
	isseq ? "tcp" : "udp");
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

/////////////////////////// dm_setsockopts //////////////////
P dm_setsockopts(P sock, P size) {
  if (setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(P)) == -1
      || setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(P)) == -1) {
    close(sock);
    return -errno;
  }

  return OK;
}

DM_INLINE_STATIC P read_port(P sock, UW eport[1]) {
  ssize_t n = 0, n_;

  do {
    while ((n_ = read(sock, ((B*) eport)+n, sizeof(eport)-n)) == -1)
      if (errno == EINTR) checkabort();
      else return -errno;
  } while ((n += n_) < sizeof(eport));

  DEBUG("ports: (%i, %i)", 
	eport[0], ntohs(eport[0]));

  return OK;
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
  P sock, dgram, retc;
  struct sockaddr_in serveraddr;
  union SocketInfo info;
  UW eport[1];

  if (o_2 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&port_)) return(UNDF_VAL);
  port_ += getportoffset();

  if (port_ >= WMAX) return RNG_CHK;
  port = (UW) port_;
  
  if ((FREEvm + ARRAY_SIZE(o_2) + 1) > CEILvm) return(VM_OVF);
  moveB((B *)VALUE_BASE(o_2),FREEvm,ARRAY_SIZE(o_2));
  FREEvm[ARRAY_SIZE(o_2)] = '\000';
  
#if ENABLE_UNIX_SOCKETS
  {
    struct sockaddr_un unixserveraddr;
    sock = -1;
    dgram = -1;
    if (strcmp("localhost", (char*)FREEvm) 
	|| init_unix_sockaddr(&unixserveraddr, port, TRUE)
	|| (sock = socket(PF_UNIX, SOCK_STREAM, 0)) == -1
	|| dm_setsockopts(sock, PACKET_SIZE))
      goto ipsocket;

    if (connect(sock, (struct sockaddr *) &unixserveraddr, 
		sizeof(unixserveraddr.sun_family)
		+ strlen(unixserveraddr.sun_path))
	|| init_unix_sockaddr(&unixserveraddr, port, FALSE)
	|| (dgram = socket(PF_UNIX, SOCK_DGRAM, 0)) == -1
	|| dm_setsockopts(dgram, 1)) {
      close(sock);
      goto ipsocket;
    }

    if (connect(dgram, (struct sockaddr *) &unixserveraddr,
		sizeof(unixserveraddr.sun_family)
		+ strlen(unixserveraddr.sun_path))) {
      dm_error_msg(errno, "unix dgram");
      close(sock);
      close(dgram);
      goto ipsocket;
    }

    if ((retc = read_port(sock, eport))) {
      close(sock);
      return retc;
    }    
    goto goodsocket;
  };
 ipsocket:
#endif //ENABLE_UNIX_SOCKETS

  if ((retc = init_sockaddr(&serveraddr, (char*)FREEvm, port)))
    return retc;
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) 
    return -errno;
  if ((retc = dm_setsockopts(sock, PACKET_SIZE)))
    return retc;
  if (connect(sock, (struct sockaddr *)&serveraddr,
	      sizeof(serveraddr)) == -1) {
    int errno_ = errno;
    close(sock);
    return -errno_;
  };

  if ((retc = read_port(sock, eport))) {
    close(sock);
    return retc;
  }

  if ((dgram = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    retc = -errno;
    close(sock);
    return retc;
  }

  if ((retc = dm_setsockopts(dgram, 1))) {
    close(dgram);
    close(sock);
    return retc;
  }
    
  serveraddr.sin_port = eport[0];
  if (! connect(dgram, (struct sockaddr *)&serveraddr,
		sizeof(serveraddr)))
    goto goodsocket;

  retc = -errno;
  close(dgram);
  close(sock);
  return -retc;
  
 goodsocket:
  info.listener.unixport = -1;
  info.listener.recsigfd = -1;
  info.listener.trecsigfd = -1;
  info.listener.sigfd = dgram;
  if ((retc = addsocket(sock, &sockettype, &info)))
    return retc;
  TAG(o_2) = NULLOBJ | SOCKETTYPE; 
  ATTR(o_2) = 0;
  SOCKET_VAL(o_2) = sock;
  DGRAM_VAL(o_2) = dgram;
  FREEopds = o_1;
  return OK;
}


/*----------------------------------------------- disconnect
    socket | --
*/

P op_disconnect(void)
{
  P retc;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (NULLOBJ | SOCKETTYPE)) return OPD_ERR;
  
  DEBUG("disconnecting: %li", (long) SOCKET_VAL(o_1));
  if ((retc = delsocket_force(SOCKET_VAL(o_1)))) return retc;
  FREEopds = o_1;
  return OK;
}

/*------------------------------------------------ sendsig
  socket sig | --

  sends signal to dnode, where signal is defined by a mapping 
  in dm-signals.c (sigmap), and is between 0 and up to SIGMAP_LEN (as defined 
  in sigmap)
*/

P op_sendsig(void) {
  B sig;
  P sig_;
  P fd;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (TAG(o_2) != (NULLOBJ | SOCKETTYPE)) return OPD_ERR;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &sig_)) return UNDF_VAL;
  if (sig_ < 0 || sig_ >= SIGMAP_LEN) return RNG_CHK;

  sig = (B) sig_;
  if ((fd = DGRAM_VAL(o_2)) == -1) return ILL_SOCK;

  while (send(fd, &sig, 1, 0) == -1) {
    if (errno != EINTR) return -errno;
    checkabort();
  };

  FREEopds = o_2;
  return OK;
}

/*----------------------------------------------- send
    socket (string) | --
    socket [ num/\* bool? rootobj (string) ] | --

    op_send defined in dm-nextevent.c
    since op_send must handle calling makesocketdead.
    In case of error, returns the active fd as *fd.
*/

P op_send(void)
{
  P retc; 
  static B rootf[FRAMEBYTES];
  P fd;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (TAG(o_2) != (NULLOBJ | SOCKETTYPE)) return OPD_ERR;
  fd = SOCKET_VAL(o_2);

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
	  
  if ((retc = tosocket(fd, rootf)))
    return makesocketdead(retc, fd, "send");
 
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
  SOCKET_VAL(o1) = recsocket;
  DGRAM_VAL(o1) = -1;
  FREEopds = o2;
  return(OK);
}

// closes the signal socket passed in.
P forksighandler(P sigsocket, P tcp_sigsocket, P serverport, P* pid) {
  return spawnsighandler(sigsocket, tcp_sigsocket, 
			 serverport, closesockets_exec, pid);
}

// nothing but STDIN, STDOUT, and STDERR will have been defined before this.
// after this STDIN=0, STDOUT=1, STDERR=2, DEVNULLREAD=3, DEVNULLWRITE=4
//   will be available.
void initfds(void) {
  P retc;

  createfds();

/*-------------------- prime the socket table -----------------------
  We use a fd_set bit array to keep track of active sockets. Hence,
  the number of active sockets is limited to the FD_SET_SIZE of
  the host system. 
*/
  FD_ZERO(&sock_fds);
  if ((retc = addsocket(STDIN_FILENO, &stdintype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add STDIN");
  if ((retc = addsocket(STDOUT_FILENO,  &stdouttype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add STDOUT");
  if ((retc = addsocket(STDERR_FILENO,  &stderrtype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add STDERR");

  if ((retc = addsocket(DM_NULLR_FILENO, &stdtype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add NULL read");
  if ((retc = addsocket(DM_NULLW_FILENO, &stdtype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add NULL write");

  if ((retc = addsocket(DM_STDIN_FILENO, &stdtype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add DM_STDIN");
  if ((retc = addsocket(DM_STDOUT_FILENO, &stdtype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add DM_STDOUT");
  if ((retc = addsocket(DM_STDERR_FILENO, &stderrtype, NULL)))
    dm_error(retc < 0 ? -retc : 0, "Failed to add DM_STDERR");
}
