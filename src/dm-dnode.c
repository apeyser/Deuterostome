#define DEBUG_ACTIVE 0
#include "dm.h"

#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>

#include "dm-dnode.h"
#include "dm-nextevent.h"
#include "pluginlib.h"
#include "dm-dnode.h"
#include "dm-vm.h"
#include "dmx.h"
#include "dm2.h"
#include "dm3.h"
#include "dm4.h"
#include "dm5.h"
#include "dqueen.h"
#include "dm-proc.h"
#include "dm-prop.h"
#include "error-local.h"
#include "dnode.h"

#if ! X_DISPLAY_MISSING
#include "xhack.h"
#endif

/*-- the X corner */
char* defaultdisplay = NULL;

P serverport = -1;
B hostname[256] = {};

/*-------------------------------------- 'error'
  - expects on operand stack:
     error code    (top)
     errsocket string
     port#
     hostname string
     pid int
  - prints message on current console or startup
    terminal (default)
  - aborts on corrupted error info
  - halts after uncorrupted error
*/

P op_error(void)
{
  LBIG e, pid, port;
  P nb, atmost; 
  B *m, strb[256], *p;

  p = strb; 
  atmost = 255;
  if (o_5 < FLOORopds) goto baderror;
  if (CLASS(o_5) != NUM) goto baderror;
  if (! VALUE(o_5, &pid)) goto baderror;
  if (TAG(o_4) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_3) != NUM) goto baderror;
  if (! VALUE(o_3, &port)) goto baderror;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_1) != NUM) goto baderror;
  if (! VALUE(o_1, &e)) goto baderror;

  nb = dm_snprintf((char*) p, atmost, "\033[31mOn %*s port %llu, pid %llu: ",
                   (int) ARRAY_SIZE(o_4),
		   (char*) VALUE_BASE(o_4), 
                   (unsigned long long) port,
		   (unsigned long long) pid);

  p += nb;
  atmost -= nb;
  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*) p, atmost, "%s",
		     (char*) strerror((P) -e));
  else { /* one of our error codes: decode */
    m = geterror((P) e);
    nb = dm_snprintf((char*) p, atmost, "%s",
		     (char*) m);
  }
  p += nb;
  atmost -= nb;

  nb = dm_snprintf((char*) p, atmost, " in %*s\033[0m\n",
		   (int) ARRAY_SIZE(o_2),
		   (char*) VALUE_BASE(o_2));
  nb += (P)(p - strb);

  TAG(o_5) = ARRAY | BYTETYPE; 
  ATTR(o_5) = READONLY;
  VALUE_BASE(o_5) = (P) strb; 
  ARRAY_SIZE(o_5) = nb;
  FREEopds = o_4;
  op_toconsole();
  if (op_halt() == DONE) return DONE;

  nb = dm_snprintf((char*) p, atmost, "%s",
		   "** Error in internal halt!\n");
  goto baderror2;

 baderror: 
  nb = dm_snprintf((char*)p, atmost,
                   "**Error with corrupted error info on operand stack!\n");
 baderror2:
  op_abort();
  nb += (P)(p - strb);
  TAG(o1) = ARRAY | BYTETYPE; 
  ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P) strb; 
  ARRAY_SIZE(o1) = nb;
  FREEopds = o2;
  return op_toconsole();
}

/*-------------------------------------- 'errormessage'
  - expects on operand stack:
     string buffer (top)
     error code
     errsocket string
     port#
     hostname string
     pid int
  - composes an error message and returns it in a subarray of string buffer
*/

P op_errormessage(void)
{
  LBIG e, pid, port;
  P nb, tnb; 
  B *m, *s;

  if (o_6 < FLOORopds) goto baderror;
  if (CLASS(o_6) != NUM) goto baderror;
  if (! VALUE(o_6, &pid)) goto baderror;
  if (TAG(o_5) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_4) != NUM) goto baderror;
  if (! VALUE(o_4, &port)) goto baderror;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_2) != NUM) goto baderror;
  if (!VALUE(o_2, &e)) goto baderror;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) goto baderror;

  s = (B *)VALUE_BASE(o_1); 
  tnb = ARRAY_SIZE(o_1);
  nb = dm_snprintf((char*) s, tnb, "On %*s port %llu, pid %llu: ",
		   (int) ARRAY_SIZE(o_5),
		   (char*) VALUE_BASE(o_5),
                   (unsigned long long) port,
		   (unsigned long long) pid);
  s += nb;
  tnb -= nb;

  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*) s, tnb, "%s",
		     (char*) strerror(-e));
  else { /* one of our error codes: decode */
    m = geterror((P) e);
    nb = strlen((char*) m);
    if (nb > tnb) nb = tnb;
    moveB(m, s, nb);
  }
  s += nb;
  tnb -= nb;

  nb = dm_snprintf((char*)s, tnb, " in %*s\n",
		   (int) ARRAY_SIZE(o_3),
		   (char*) VALUE_BASE(o_3));

  ARRAY_SIZE(o_1) = (P)(s + nb) - VALUE_BASE(o_1);
  moveframe(o_1,o_6);
  FREEopds = o_5;
  return OK;

 baderror:
  printf("**Error with corrupted error info on operand stack!\n");
  return op_halt();
}

/*------------------------------------------- Xconnect
     (hostname:screen#) | --

  - establishes an X windows connection to the specified screen of
    the specified host (hostname is the plain host name)
*/

static P int_Xdisconnect(BOOLEAN nocheck) {
#if X_DISPLAY_MISSING
  return NO_XWINDOWS;
#else
  if (! nocheck && dvtdisplay) nocheck = TRUE;

  closedisplay();
  if (nocheck)  {
    if (defaultdisplay) setenv("DISPLAY", defaultdisplay, 1);
    else unsetenv("DISPLAY");
  }
  return OK;
#endif
}


#if ! X_DISPLAY_MISSING
static int xioerrorhandler(Display* display) {
  char msg[80] = "";
  HXGetErrorDatabaseText(display, "dnode", 
			 "XProtoError", "Connection Dead", msg, sizeof(msg));
  fprintf(stderr, "Proto Xerror: %s\n", msg);
  HXGetErrorDatabaseText(display, "dnode", 
			 "XRequest", "Connection Dead", msg, sizeof(msg));
  fprintf(stderr, "Fatal Xerror: %s\n", msg);
  HXGetErrorDatabaseText(display, "dnode", 
			 "XlibMessage", "Connection Dead", msg, sizeof(msg));
  fprintf(stderr, "Internal Xerror: %s\n", msg);

  dvtdisplay = NULL;
  int_Xdisconnect(TRUE);
  xhack_longjmp();
  return 0;
}

static int xerrorhandler(Display* display, XErrorEvent* event) {
  char msg[80] = "";
  HXGetErrorText(display, event->error_code, msg, sizeof(msg));
  fprintf(stderr, "Xerror: %s\n", msg);
  if (x2 <= CEILexecs) {
    makename((B*)"Xdisconnect", x1); ATTR(x1) = ACTIVE;
    FREEexecs = x1;
  }
  return 0;
}
#endif
	

P op_Xconnect(void)
{
#if X_DISPLAY_MISSING
  return NO_XWINDOWS;
#else
  P retc;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) > (P) sizeof(displayname)-1) return RNG_CHK;
  DEBUG("open display%s", "");
  if (ARRAY_SIZE(o_1) > 0) {
    moveB((B *)VALUE_BASE(o_1), displayname, ARRAY_SIZE(o_1));
    displayname[ARRAY_SIZE(o_1)] = '\000';
    dvtdisplay = XOpenDisplay((char*)displayname);
  }
  else if ((dvtdisplay = XOpenDisplay(NULL))) {
    strncpy((char*)displayname, DisplayString(dvtdisplay), 
	    sizeof(displayname)-1);
    displayname[sizeof(displayname)-1] = '\000';
  };

  if (! dvtdisplay) {
    *displayname = '\0';
    return X_BADHOST;
  };

  setenv("DISPLAY", (char*)displayname, 1);
  dvtscreen = HXDefaultScreenOfDisplay(dvtdisplay);
  dvtrootwindow = HXDefaultRootWindow(dvtdisplay);
  if (HXGetWindowAttributes(dvtdisplay,dvtrootwindow,&rootwindowattr) == 0)
    error_local(EXIT_FAILURE,0,"Xwindows: no root window attributes");
  ndvtwindows = 0; 
  ncachedfonts = 0;
  dvtgc = HXCreateGC(dvtdisplay,dvtrootwindow,0,NULL);
  xsocket = ConnectionNumber(dvtdisplay);
  if ((retc = addsocket(xsocket, &sockettype, &defaultsocketinfo))) {
    if (retc == SOCK_STATE) {
      if (close(xsocket)) return -errno;
      xsocket = -1;
    }
    int_Xdisconnect(TRUE);
    return retc;
  }
  FREEopds = o_1; 
  XSetErrorHandler(xerrorhandler);
  XSetIOErrorHandler(xioerrorhandler);
  return OK;
#endif
}

/*------------------------------------------- Xdisconnect
     -- | --

 - breaks an existing connection to an X windows server (thus
   removing all windows existing in that connection)
*/

P op_Xdisconnect(void)
{
  return int_Xdisconnect(FALSE);
}

/*------------------------------------------- getmyport
    | serverport/l

returns the host's port (such as for error)
*/

P op_getmyport(void)
{
  if (CEILopds < o2) return OPDS_OVF;
  TAG(o1) = (NUM | LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = serverport - getportoffset();
  FREEopds = o2;

  return OK;
}

/*----------------------------------------------- Xwindows_

   -- | bool

  - reports whether Xwindows has been built for initial connection
	  (if not, X windows operators
    will return the error NO_XWINDOWS)
*/

P op_Xwindows_()
{
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = BOOL; 
  ATTR(o1) = 0;
#if X_DISPLAY_MISSING
	BOOL_VAL(o1) = FALSE;
#else
  BOOL_VAL(o1) = TRUE;
#endif
  FREEopds = o2;
  return OK;
}

#if ENABLE_UNIX_SOCKETS
static P unixserversocket = -1;
static P unixsigsocket = -1;
#endif
static P serversocket = -1;
static P sigsocket = -1;
static P tcp_sigsocket = -1;
static UW eport[1] = {0};

DM_INLINE_STATIC P write_port(P sock, UW eport[1]) {
  ssize_t n = 0, n_;

  do {
    while ((n_ = write(sock, ((B*) eport)+n, sizeof(eport)-n)) == -1)
      if (errno == EINTR) checkabort();
      else return -errno;
  } while ((n += n_) < sizeof(eport));

  return OK;
}


DM_INLINE_STATIC P handleserverinput(void) {
  struct sockaddr clientname;
  socklen_t size = sizeof(clientname);
  P newfd; 
  P retc;

  if ((newfd = accept(recsocket, &clientname, &size)) == -1) {
    retc = -errno;
    delsocket_force(recsocket);
    return retc;
  }

  if ((retc = dm_setsockopts(newfd, PACKET_SIZE))) {
    delsocket_force(recsocket);
    return retc;
  }
  
  DEBUG("Adding %li", (long) newfd);
  if ((retc = write_port(newfd, eport))) {
    close(newfd);
    return retc;
  }

  if ((retc = addsocket(newfd, &sockettype, &defaultsocketinfo)))
    return retc;
  
  return OK;
}

void clearsocket_special(P fd) {
#if ENABLE_UNIX_SOCKETS
  if (fd == unixserversocket) unixserversocket = -1;
  else
#endif //ENABLE_UNIX_SOCKETS
    if (fd == serversocket) serversocket = -1;
}

BOOLEAN masterinput(P* retc, B* bufferf DM_UNUSED) {
#if ENABLE_UNIX_SOCKETS
  if (unixserversocket == recsocket) {
    *retc = handleserverinput();
    return TRUE;
  }
#endif //ENABLE_UNIX_SOCKETS

  if (serversocket == recsocket) {
    *retc = handleserverinput();
    return TRUE;
  }

  return FALSE;
}

#if ! X_DISPLAY_MISSING

DM_INLINE_STATIC P wrap_lock(P retc) {
  if (retc) return retc;

  if (o2 >= CEILopds) return OPDS_OVF;
  if (x_1 < FLOORexecs) return EXECS_UNF;

  moveframe(x_1, o1);

  TAG(o2) = OP;
  ATTR(o2) = ACTIVE;
  OP_NAME(o2) = "stopped";
  OP_CODE(o2) = op_stopped;

  FREEopds = o3;

  TAG(x_1) = OP;
  ATTR(x_1) = ACTIVE;
  OP_NAME(x_1) = "pop";
  OP_CODE(x_1) = op_pop;

  return op_lock();
}

DM_INLINE_STATIC P wm_delete_window_(XEvent* event, B* userdict) {
  P wid = event->xclient.window;
  static B namestring[NAMEBYTES];
  static B namef[FRAMEBYTES];
  B* dictf;

  snprintf((char*)namestring, sizeof(namestring), 
	   "w%lld", (long long) wid);
  makename(namestring, namef); 
  ATTR(namef) = ACTIVE;
  
  if ((dictf = lookup(namef, userdict)) == 0L) return UNDF;
  if (x1 >= CEILexecs) return EXECS_OVF;
  if (FREEdicts >= CEILdicts) return DICTS_OVF;
  moveframe(dictf, FREEdicts); FREEdicts += FRAMEBYTES;
  
  makename((B*)"delete_window", x1); 
  ATTR(x1) = ACTIVE;
  FREEexecs = x2;

  return OK;
}

P wm_delete_window(XEvent* event, B* userdict) {
  return wrap_lock(wm_delete_window_(event, userdict));
}


P wm_take_focus(XEvent* event, B* userdict) {
  return wrap_lock(wm_take_focus_(event, userdict));
}

P wm_configure_notify(XEvent* event, B* userdict) {
  return wrap_lock(wm_configure_notify_(event, userdict));
}

P wm_expose(XEvent* event, B* userdict) {
  return wrap_lock(wm_expose_(event, userdict));
}

P wm_button_press(XEvent* event, B* userdict) {
  return wrap_lock(wm_button_press_(event, userdict));
}
#endif //X_DISPLAY_MISSING

/* push on operand stack:
   error code    (top)
   errsocket string
   port#
   hostname string
   pid
   and push active name 'error' on execution stack
*/
void makeerror(P retc, B* error_source) {   
  if (o5 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;

  TAG(o1) = (NUM | LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (LBIG) getpid();

  TAG(o2) = (ARRAY | BYTETYPE);
  ATTR(o2) = READONLY;
  VALUE_BASE(o2) = (P) hostname;
  ARRAY_SIZE(o2) = strlen((char*) hostname);

  TAG(o3) = (NUM | LONGBIGTYPE);
  ATTR(o3) = 0;
  LONGBIG_VAL(o3) = serverport - getportoffset();

  TAG(o4) = (ARRAY | BYTETYPE);
  ATTR(o4) = READONLY;
  VALUE_PTR(o4) = (B*) error_source; 
  ARRAY_SIZE(o4) = strlen((char*) error_source);

  TAG(o5) = (NUM | LONGBIGTYPE);
  ATTR(o5) = 0;
  LONGBIG_VAL(o5) = retc;

  moveframe(errorframe, x1);
  FREEopds = o6;
  FREEexecs = x2;
}

/*-------------------------------------- 'toconsole'
   (message) | -

  - sends a command to print the message string to the current
    console node
  - if there is no console socket assigned or if the console
    socket fails, we default to 'stderr' and simply print the
    message
  - if 'stderr' fails, we give up and abort
*/

P op_toconsole(void)
{
  static B stringf[FRAMEBYTES];
  B *p; 
  B *p_;
  B *oldFREEvm;
  P nb, atmost, retc;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (consolesocket != PINF) { 
    B* max_ = VALUE_PTR(o_1) + ARRAY_SIZE(o_1);
    if ((FREEvm + ARRAY_SIZE(o_1) + 25) > CEILvm) return VM_OVF;
    p_ = VALUE_PTR(o_1);
    do {
      B* max = FREEvm + DVTSTRINGBUFSIZE - 20;
      if (max > CEILvm) max = CEILvm;

      p = FREEvm; 
      moveB((B*)"save (", p, 6); p += 6;    
      for (; p_ <  max_ && p < max; p_++) {
        switch (*p_) {
          case ')': case '\\': 
            p += dm_snprintf((char*)p, max - p, "\\%c", (unsigned int) *p_);
            break;
            
          case 0: case 1: case 2: case 3: case 4: case 5: 
          case 6: case 7: case 8: case 9: case 10: case 11: 
          case 12: case 13: case 14: case 15: case 16: case 17: 
          case 18: case 19: case 20: case 21: case 22: case 23: 
          case 24: case 25: case 26: case 27: case 28: case 29: 
          case 30: case 31: case 127:
            p += dm_snprintf((char*)p, max - p, "\\%.3o", (unsigned int) *p_);
            break;

          default:
            *(p++) = *p_;
            break;
        }
        if (p == CEILvm) return VM_OVF;
      }
      if (p + 19 > CEILvm) return VM_OVF;
      moveB((B*)") toconsole restore",p,19); 
      p += 19;
      TAG(stringf) = ARRAY | BYTETYPE; 
      ATTR(stringf) = 0;
      VALUE_PTR(stringf) = FREEvm; 
      ARRAY_SIZE(stringf) = p - FREEvm;
      oldFREEvm = FREEvm; 
      FREEvm = (B*)DALIGN(p);
      if ((retc = tosocket(consolesocket, stringf))) {
        FREEvm = oldFREEvm;
	error_local(0, 0, "Unable to propagate: %s", stringf);
        return makesocketdead(retc, consolesocket, "toconsole");
      }
      FREEvm = oldFREEvm;
    } while (p_ < max_);
  }
  else {
    p = (B *)VALUE_BASE(o_1); 
    atmost = ARRAY_SIZE(o_1);
    while (atmost > 0) { 
      while ((nb = write(STDERR_FILENO, p, atmost)) < 0)
        if (errno != EINTR) return op_abort();
      atmost -= nb;
      p += nb;
    }
  }
  FREEopds = o_1;
  return OK;
}


BOOLEAN pending(void) {
  if (halt_flag) {
    if (x_1 >= FLOORexecs
	&& TAG(x_1) == OP
	&& OP_CODE(x_1) == x_op_halt)
      return FALSE;
  }

  return (recvd_quit || FREEexecs != FLOORexecs);
}

P clientinput(void) {
  if (x1 >= CEILexecs) return EXECS_OVF;
  if (o_1 < FLOORopds) return OPDS_UNF;

  moveframe(o_1, x1);
  FREEopds = o_1;
  FREEexecs = x2;

  return OK;
}

/****************** killsockets ***********************
 * call right after vmresize, to reset sockets if
 * vmresize failed
 * --- | --- <<all non-server sockets closed>>
 */
P op_killsockets(void) {
  int retc = OK, retc_ = OK;
  retc = op_Xdisconnect();
  retc_ = closesockets_resize();
  return retc ? retc : retc_ ? retc_ : ABORT;
}


DM_INLINE_STATIC void restart(void) __attribute__((noreturn));
void restart(void) {
  if (sigqueue(getppid(), DM_RESTART, DM_RESTART_VAL))
    error_local(EXIT_FAILURE, errno, "sigqueue");
  exit(0);
}

P op_vmresize(void) {
  P retc;
  if (o_1 >= FLOORopds && CLASS(o_1) == NULLOBJ)
    restart();

  if ((retc = op_vmresize_())) return retc;
  if ((retc = closesockets_resize())) return retc;
#if DM_ENABLE_RTHREADS
  if ((retc = killrthreads())) return retc;
#endif
#if ! X_DISPLAY_MISSING
  return op_Xdisconnect();
#else
  return OK;
#endif //X_DISPLAY_MISSING
}

DM_INLINE_STATIC void sock_error(BOOLEAN ex, P errno_, const char* msg) {
  if (ex && serversocket != -1) delsocket_force(serversocket);

  serversocket = -1;
  sigsocket = -1;
  tcp_sigsocket = -1;

#if ENABLE_UNIX_SOCKETS
  if (unixserversocket != -1) delsocket_force(unixserversocket);
  unixserversocket = -1;
  unixsigsocket = -1;
#endif //ENABLE_UNIX_SOCKETS

  error_local(ex ? EXIT_FAILURE : 0, errno_, "%s", msg);
}

void run_dnode_mill(void) {
  P retc;
  B abortframe[FRAMEBYTES];
  union SocketInfo socketinfo = defaultsocketinfo;
  UW port = (UW) serverport;

#if ! X_DISPLAY_MISSING
  defaultdisplay = getenv("DISPLAY");
#endif

  setuphandlers();
  set_closesockets_atexit();
  setupfd();
  maketinysetup();

  if ((serversocket = make_socket(&port, TRUE, PACKET_SIZE, &retc)) == -1)
    sock_error(TRUE, retc < 0 ? -retc : 0, "making internet server socket");

  if ((sigsocket = make_socket(eport, TRUE, 1, &retc)) == -1)
    sock_error(TRUE, retc < 0 ? -retc : 0, "making internet signal socket");
  socketinfo.listener.recsigfd = -1;
  socketinfo.listener.trecsigfd = sigsocket;

  if ((retc = addsocket(serversocket, &sockettype, &socketinfo)))
    sock_error(TRUE, retc < 0 ? -retc : 0, "adding sockets");

#if ENABLE_UNIX_SOCKETS
  if ((unixserversocket = make_unix_socket(serverport, TRUE, &retc)) < 0) {
    sock_error(FALSE, retc < 0 ? -retc : 0, "making unix server socket");
    goto socksdone;
  }

  if ((unixsigsocket = make_unix_socket(serverport, FALSE, &retc)) < 0) {
    sock_error(FALSE, errno, "making unix signal socket");
    delsocket_force(unixserversocket);
    goto socksdone;
  }

  socketinfo.listener.unixport = serverport;
  socketinfo.listener.recsigfd = unixsigsocket;
  socketinfo.listener.trecsigfd = -1;
  if ((retc = addsocket(unixserversocket, &sockettype, &socketinfo))) {
    sock_error(FALSE, retc < 0 ? -retc : 0, "adding sockets");
    goto socksdone;
  }

#endif //ENABLE_UNIX_SOCKETS

socksdone:
#if DM_ENABLE_RTHREADS
  rthreads_init();
#endif //DM_ENABLE_RTHREADS

/*----------------- construct frames for use in execution of D code */
  makename((B*) "error", errorframe); 
  ATTR(errorframe) = ACTIVE;

  makename((B*) "abort", abortframe); 
  ATTR(abortframe) = ACTIVE;

/*-------------- you are entering the scheduler -------------------*/\
/* We start with no D code on the execution stack, so we doze
   while waiting for source (or console) activity.

   We scan sources always in round-robin fashion across snapshots;
   so we maintain a rotating source index.
*/

  moveframe(msf, cmsf);
  locked = FALSE;
  serialized = FALSE;
  while (1) {
    switch (retc = exec(100)) {
      case DONE:
	locked = FALSE; 
	serialized = FALSE;
	if (FREEexecs == FLOORexecs) {
	  moveframe(msf,cmsf);
	  halt_flag = FALSE;
	}

      // intentional fall-through
      case MORE:
	retc = nextevent(cmsf);
	break;

      case TERM: die();
	
      default: break;
    }

    switch (retc) {
      case OK: continue;

      case ABORT:
	abortflag = FALSE;
	if (x1 >= CEILexecs) {
	  retc = EXECS_OVF;
	  errsource = (B*) "supervisor";
	  break;
	}

	moveframe(abortframe, x1);
	FREEexecs = x2;
	continue;

      case QUIT:
	if ((retc = quit())) {
	  errsource = (B*) "supervisor";
	  break;
	}
	continue;

      default: break;
    }

    /*------------------------------------ report an error */
    makeerror(retc, errsource);
  }  /* we never return */
}
