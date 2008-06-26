#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>

#include "dm-dnode.h"
#include "dm-nextevent.h"
#include "pluginlib.h"
#include "dm3.h"
#include "dm-dnode.h"
#include "dm-vm.h"
#include "dmx.h"
#include "dm2.h"
#include "dqueen.h"

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
  - prints message on current console or startup
    terminal (default)
  - aborts on corrupted error info
  - halts after uncorrupted error
*/

P op_error(void)
{
  LBIG e;
  P nb, atmost; 
  B *m, strb[256], *p;
  P ret;

  p = strb; 
  atmost = 255;
  if (o_4 < FLOORopds) goto baderror;
  if (TAG(o_4) != (ARRAY | BYTETYPE)) goto baderror;
  if (TAG(o_3) != (NUM | LONGBIGTYPE)) goto baderror;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_1) != NUM) goto baderror;
  if (!VALUE(o_1,&e)) goto baderror;

  nb = dm_snprintf((char*)p,atmost,"\033[31mOn %*s port %lld: ",
                   (int) ARRAY_SIZE(o_4), (char*)VALUE_BASE(o_4), 
                   (long long) LONGBIG_VAL(o_3));

  p += nb; atmost -= nb;
  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*)p,atmost,(char*)strerror((P)-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = dm_snprintf((char*)p,atmost,(char*)m);
  }
  p += nb; atmost -= nb;
  nb = dm_snprintf((char*)p,atmost," in %s\033[0m\n", (char*)VALUE_BASE(o_2));
  nb += (P)(p - strb);
  TAG(o_4) = ARRAY | BYTETYPE; 
  ATTR(o_4) = READONLY;
  VALUE_BASE(o_4) = (P)strb; 
  ARRAY_SIZE(o_4) = nb;
  FREEopds = o_3;
  op_toconsole();
  if ((ret = op_halt()) == DONE) return DONE;

  nb = dm_snprintf((char*)p, atmost, "** Error in internal halt!\n");
  goto baderror2;

 baderror: 
  nb = dm_snprintf((char*)p,atmost,
                   "**Error with corrupted error info on operand stack!\n");
 baderror2:
  op_abort();
  nb += (P)(p - strb);
  TAG(o1) = ARRAY | BYTETYPE; 
  ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)strb; 
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
  - composes an error message and returns it in a subarray of string buffer
*/

P op_errormessage(void)
{
  LBIG e;
  P nb, tnb; 
  B *m, *s;

  if (o_5 < FLOORopds) goto baderror;
  if (TAG(o_5) != (ARRAY | BYTETYPE)) goto baderror;
  if (TAG(o_4) != (NUM | LONGBIGTYPE)) goto baderror;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_2) != NUM) goto baderror;
  if (!VALUE(o_2,&e)) goto baderror;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) goto baderror;

  s = (B *)VALUE_BASE(o_1); 
  tnb = ARRAY_SIZE(o_1);
  nb = dm_snprintf((char*)s,
		   tnb,"On %*s port %lld: ", (int) ARRAY_SIZE(o_5),
                   (char*)VALUE_BASE(o_5), 
                   (long long) LONGBIG_VAL(o_4));
  s += nb; tnb -= nb;

  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*)s,tnb,(char*)strerror(-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = strlen((char*)m);
    if (nb > tnb) nb = tnb;
    moveB(m,s,nb);
  }
  s += nb; tnb -= nb;
  nb = dm_snprintf((char*)s,tnb," in %s\n", (char*)VALUE_BASE(o_3));
  ARRAY_SIZE(o_1) = (P)(s + nb) - VALUE_BASE(o_1);
  moveframe(o_1,o_5);
  FREEopds = o_4;
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
  if (nocheck || dvtdisplay)  {
    if (dvtdisplay) HXCloseDisplay(dvtdisplay);
    delsocket(xsocket);
    xsocket = -1;
    if (defaultdisplay) setenv("DISPLAY", defaultdisplay, 1);
    else unsetenv("DISPLAY");
    dvtdisplay = NULL;
  }
  *displayname = '\0';
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
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) > (P) sizeof(displayname)-1) return RNG_CHK;
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
    error(EXIT_FAILURE,0,"Xwindows: no root window attributes");
  ndvtwindows = 0; 
  ncachedfonts = 0;
  dvtgc = HXCreateGC(dvtdisplay,dvtrootwindow,0,NULL);
  xsocket = ConnectionNumber(dvtdisplay);
  addsocket(xsocket);
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

DM_INLINE_STATIC P handleserverinput(void) {
  struct sockaddr clientname;
  socklen_t size = sizeof(clientname);
  P newfd; 
  P psize = PACKET_SIZE;
  P retc;

  if ((newfd = accept(recsocket, &clientname, &size)) == -1)
    goto ERR;
  if (setsockopt(newfd, SOL_SOCKET, SO_SNDBUF, (B *)&psize, sizeof(P)) == -1) 
    goto ERR;
  if (setsockopt(newfd, SOL_SOCKET, SO_RCVBUF, (B *)&psize, sizeof(P)) == -1)
    goto ERR;
  if ((retc = closeonexec(newfd)))
    goto ERR;

  addsocket(newfd);
  return OK;

  ERR: {
    P e = errno;
    delsocket(recsocket);
    if (newfd != -1) close(newfd);
    return -e;
  }
}

BOOLEAN masterinput(P* retc, B* bufferf __attribute__ ((__unused__)) ) {
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

P killsockets(void) {
  int i;
  op_Xdisconnect();

  for (i = 0; i < FD_SETSIZE; ++i)
    if (FD_ISSET(i, &sock_fds) 
#if ENABLE_UNIX_SOCKETS
	&& (i != unixserversocket)
#endif //ENABLE_UNIX_SOCKETS
	&& (i != serversocket)) {
      delsocket(i);
    }

  if (x1 >= CEILexecs)
    return EXECS_OVF; 

  TAG(x1) = OP; 
  ATTR(x1) = ACTIVE;
  OP_NAME(x1) = "abort"; 
  OP_CODE(x1) = op_abort;
  FREEexecs = x2;
  
  return OK;
}

/* push on operand stack:
   error code    (top)
   errsocket string
   port#
   hostname string
   and push active name 'error' on execution stack
*/
void makeerror(P retc, B* error_source) {   
  if (o4 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;
  TAG(o1) = ARRAY | BYTETYPE; 
  ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)hostname; 
  ARRAY_SIZE(o1) = strlen((char*)hostname);
  TAG(o2) = NUM | LONGBIGTYPE; 
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = serverport - getportoffset();
  TAG(o3) = ARRAY | BYTETYPE; 
  ATTR(o3) = READONLY;
  VALUE_BASE(o3) = (P)error_source; 
  ARRAY_SIZE(o3) = strlen((char*)error_source);
  TAG(o4) = NUM | LONGBIGTYPE; 
  ATTR(o4) = 0; 
  LONGBIG_VAL(o4) = retc;
  moveframe(errorframe,x1);
  FREEopds = o5; 
  FREEexecs = x2;
}

/*------------------------------------- 'setconsole'
   consolesocket | -

  - 'consolesocket' is a null object of type 'socket'
    or a plain null object (to select the default, 'stderr')
  - selects a socket to receive console output
  - this socket is used until another socket is selected
  - if the designated socket connection breaks, console output
    is directed to the default, 'stderr'
*/

P op_setconsole(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NULLOBJ) return OPD_CLA;
  if (TYPE(o_1) == SOCKETTYPE) consolesocket = SOCKET_VAL(o_1);
  else  consolesocket = PINF;
  FREEopds = o_1;
  return OK;
}

/*-------------------------------------- 'console'
    -- | consolesocket or null (if stderr)

  - returns a null object of type 'socket' that refers to
    the current console socket (or 'stderr' for default)
*/

P op_console(void) {
  if (o1 > CEILopds) return (OPDS_OVF);
  TAG(o1) = NULLOBJ;
  if (consolesocket != PINF) {
    TAG(o1) |= SOCKETTYPE;
    SOCKET_VAL(o1) = consolesocket;
    DGRAM_VAL(o1) = -1;
  }
  FREEopds = o2;
  return OK;
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

  return (FREEexecs != FLOORexecs);
}

P clientinput(void) {
  if (x1 >= CEILexecs) return EXECS_OVF;
  if (o_1 < FLOORopds) return OPDS_UNF;

  moveframe(o_1, x1);
  FREEopds = o_1;
  FREEexecs = x2;
  
  return OK;
}

/*---------- singnal handler: quit's */
static void quithandler_(int sig) __attribute__ ((__noreturn__));
static void quithandler_(int sig) {
  int i;
  fprintf(stderr, "Exiting dnode on signal: %i\n", sig);
#if ! X_DISPLAY_MISSING
  if (dvtdisplay) {
    HXCloseDisplay(dvtdisplay); 
    displayname[0] = '\0';
  }
#endif
    
  for (i = 0; i < FD_SETSIZE; i++)
    if (FD_ISSET(i, &sock_fds) && (i != xsocket)) close(i);
  exit(0);
}

static void quithandler_exit(void) {quithandler_(0);}

/**********************************************vmreset
 * call right after vmresize, to reset sockets if
 * vmresize failed
 * --- | --- <<all non-server sockets closed>>
 */
P op_killsockets(void) {return KILL_SOCKETS;}

P op_socketdead(void) {return DEAD_SOCKET;}

P op_vmresize(void) {
  P retc = op_vmresize_();
  if (retc) return retc;
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
  if (ex && serversocket != -1) close(serversocket);
  if (ex && sigsocket != -1) close(sigsocket);

#if ENABLE_UNIX_SOCKETS
  if (unixserversocket != -1) delsocket(unixserversocket);
  if (unixsigsocket != -1) close(unixsigsocket);
  unixserversocket = -1;
  unixsigsocket = -1;
#endif //ENABLE_UNIX_SOCKETS

  error(ex ? EXIT_FAILURE : 0, errno_, msg);
}

static pid_t redirector = -1;
void redirector_killer(void) {
  if (kill(redirector, SIGTERM)) {
    error(0, errno, "Killing redirector");
    return;
  }

  while (waitpid(redirector, NULL, 0) == -1 
	 && errno == EINTR);
  return;
}

__attribute__ ((__noreturn__))
static void redirector_quit(int sig __attribute__ ((__unused__)) ) {
  close(sigsocket);
#if ENABLE_UNIX_SOCKETS
  if (unixsigsocket != -1) close(unixsigsocket);
#endif

  exit(0);
}

static void redirect_sig(int sig) {
  if (kill(getppid(), sig))
    error(EXIT_FAILURE, errno, "redirector unable to signal parent");
}

static void handle_redirect(P socket) {
  B sig;

  switch (recv(socket, &sig, 1, 0)) {
    case 0:
      error(1, 0, "redirector received close on recv");
    case -1:
      error(EXIT_FAILURE, errno, "redirector recv");
  };

  propagate_sig(sig, redirect_sig);
}


static void sethandler(int sig, void (*handler)(int sig)) {
  struct sigaction sa;
  
  sa.sa_handler = handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 0;
  if (sigaction(sig, &sa, NULL))
    error(1, errno, "Unable to set signal handler for %i", sig);
}

__attribute__ ((noreturn))
static void redirectorf(void) {
  fd_set rin, rout;
  int nfds = 1;
  int quitsigs[] = {SIGQUIT, SIGTERM, SIGINT, 0};
  int *i;
  for (i = quitsigs; *i; i++) sethandler(*i, redirector_quit);

  FD_ZERO(&rin);
  FD_SET(sigsocket, &rin);
#if ENABLE_UNIX_SOCKETS
  if (unixsigsocket != -1) {
    nfds++;
    FD_SET(sigsocket, &rin);
  }
#endif // ENABLE_UNIX_SOCKET

  for (rout = rin; 
       select(nfds, &rout, NULL, NULL, NULL) != -1
	 || errno == EINTR;
       rout = rin) {
    if (FD_ISSET(sigsocket, &rout)) handle_redirect(sigsocket);
#if ENABLUE_UNIX_SOCKETS
    if (unixsigsocket != -1 && FD_ISSET(unixsigsocket, &rout))
      handle_redirect(unixsigsocket);
#endif //ENABLE_UNIX_SOCKET
  }

  error(EXIT_FAILURE, errno, "select failed in redirector");
  exit(1);
}

static void sig_redirect(void) {
  redirector = fork();
  if (redirector == -1) 
    sock_error(TRUE, errno, "forking for signal redirector");
  if (redirector) {
    if (atexit(redirector_killer))
      sock_error(TRUE, errno, "atexit for signal redirector");
    
    close(sigsocket);
    sigsocket = -1;
#if ENABLE_UNIX_SOCKETS
    if (unixsigsocket != -1) close(unixsigsocket);
    unixsigsocket = -1;
#endif //ENABLE_UNIX_SOCKETS
    return;
  }

  redirectorf();
}

void run_dnode_mill(void) {
  P retc;

#if ! X_DISPLAY_MISSING
  defaultdisplay = getenv("DISPLAY");
#endif

  if ((serversocket = make_socket(serverport, TRUE)) < 0)
    sock_error(TRUE, errno, "making internet server socket");
  if ((retc = closeonexec(serversocket)))
    sock_error(TRUE, -retc, "setting close on exec on server socket");
  addsocket(serversocket);
  if (listen(serversocket,5) < 0) 
    sock_error(TRUE, errno,"listen");

  if ((sigsocket = make_socket(serverport, FALSE)) < 0)
    sock_error(TRUE, errno, "making internet signal socket");
  if ((retc = closeonexec(sigsocket)))
    sock_error(TRUE, -retc, "setting close on exec on signal socket");

#if ENABLE_UNIX_SOCKETS
  if ((unixserversocket = make_unix_socket(serverport, TRUE)) < 0) {
    sock_error(FALSE, errno, "making unix server socket");
    goto socksdone;
  }
  else if ((retc = closeonexec(unixserversocket)))
    sock_error(TRUE, -retc, "setting close on exec on unix server socket");

  addsocket(unixserversocket);
  if (listen(unixserversocket,5) < 0)
    sock_error(TRUE,errno,"listen");

  if ((unixsigsocket = make_unix_socket(serverport, FALSE)) < 0)
    sock_error(FALSE, errno, "making unix signal socket");
  else if ((retc = closeonexec(unixsigsocket)))
    sock_error(TRUE, -retc, "setting close on exec on unix signal socket");
#endif //ENABLE_UNIX_SOCKETS

socksdone:
  sig_redirect();
  maketinysetup(quithandler_);
  if (atexit(quithandler_exit))
    error(1, errno, "Unable to add quit exit handler");
#if DM_ENABLE_RTHREADS
  rthreads_init();
#endif //DM_ENABLE_RTHREADS

/*----------------- construct frames for use in execution of D code */
  makename((B*)"error", errorframe); 
  ATTR(errorframe) = ACTIVE;

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
      case MORE: 
	if (locked) continue; 
	retc = nextevent(cmsf);
	break;

      case DONE: 
	locked = FALSE; 
	serialized = FALSE;
	if (FREEexecs == FLOORexecs) {
	  moveframe(msf,cmsf);
	  halt_flag = FALSE;
	}
	retc = nextevent(cmsf);
	break;

      case KILL_SOCKETS:
	retc = killsockets();
	break;

      default:
	break;
    }
    switch (retc) {
      case OK: continue;
      case ABORT: op_abort(); continue;
      default: break;
    }

    /*------------------------------------ report an error */
    makeerror(retc, errsource);
  }  /* we never return */
}
