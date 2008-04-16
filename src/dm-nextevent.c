#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include "dm-nextevent.h"
#include "error-local.h"
#include "dm3.h"


// ------------------------------------- centralize socket handler

// moreX is true iff there are X events pending, but dequeued from
// the xsocket
static BOOLEAN moreX = FALSE;
// call this if all X events pending have been dequeued.
// Called from Xdisconnect in dnode to signal that all pending X events
// have been cancelled by an Xdisconnect
void noMoreX(void) {moreX = FALSE;}

// -------- makesocketdead ------------------------
// This closes a socket, 
// then, if an error is pending:
//     pushes an error info on the opstack, and error on the exec stack
// and then, in all cases:
//     pushes the socket on the opstack, and ~socketdead on the exec stack
P makesocketdead(P retc, P socketfd, B* error_source) {
  if (socketfd == consolesocket) consolesocket = PINF;
  if (retc && retc != LOST_CONN) makeerror(retc, error_source);

  close(socketfd);
  delsocket(socketfd);

  if (x3 > CEILexecs) return EXECS_OVF;
  if (o2 > CEILopds) return OPDS_OVF;

  TAG(o1) = NULLOBJ | SOCKETTYPE; 
  ATTR(o1) = 0; 
  LONGBIG_VAL(o1) = socketfd;
  FREEopds = o2;

  makename((B*)"socketdead", x1); 
  ATTR(x1) = ACTIVE; 
  TAG(x2) = BOOL; 
  ATTR(x2) = (ABORTMARK | ACTIVE); 
  BOOL_VAL(x2) = FALSE;
  FREEexecs = x3;

  return OK;
}

//------------------------ op_send
// see dm3.c (int_send) for comments.
// This just intercepts errors.

P op_send(void) {
  P socketfd;
  P retc = int_send(&socketfd);
  if (retc) return makesocketdead(retc, socketfd, "send");
  return OK;
}

#if ! DISABLE_NEXTEVENT

// ---------- nexsocket -----------------------------
// returns true if a socket is pending in read_fds
// goes through all read_fds pending sockets,
// starting from recsocket+1 % maxsocket to recsocket % maxsocket
// in a circular fashion.
DM_INLINE_STATIC BOOLEAN nextsocket(fd_set* read_fds) {
  P i;

  for (i=0; i < maxsocket; i++) { /* we go up to a full period, round robin */
    if (++recsocket >= maxsocket) recsocket = 0;

    if (FD_ISSET(recsocket, read_fds)) {
      FD_CLR(recsocket, read_fds);
      return TRUE;
    }
  }
  return FALSE;
}

#if ! X_DISPLAY_MISSING
#include "dmx.h"
#include "xhack.h"


// ---------- nextXevent ---------------------------
// Call when there is an X event pending, either on 
// the xsocket or in the pending queue.
// Gets one X event from the queue, updates moreX
// and then, if it is a recognized X event, it calls
// the associated handler (from dnode or dvt).
// the base for the handlers are defined below (name handler_)
DM_INLINE_STATIC P nextXevent(void) {
  XEvent event;
  B* userdict = (B *)VALUE_BASE(FLOORdicts + FRAMEBYTES);

  HXNextEvent(dvtdisplay, &event);
  moreX = HQLength(dvtdisplay) ? TRUE : FALSE;

  switch(event.type) {
    case ClientMessage:
      if ((Atom) event.xclient.message_type 
	  != HXInternAtom(dvtdisplay, "WM_PROTOCOLS", False))
	return OK;

      if ((Atom) event.xclient.data.l[0] 
	  == HXInternAtom(dvtdisplay, "WM_DELETE_WINDOW", False))
	return wm_delete_window(&event, userdict); 

      if ((Atom) event.xclient.data.l[0]
	  == HXInternAtom(dvtdisplay, "WM_TAKE_FOCUS", False))
	return wm_take_focus(&event, userdict);

      return OK;

    case ConfigureNotify:
      return wm_configure_notify(&event, userdict);

    case Expose:
      if (event.xexpose.count != 0) return OK;
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
        
  if ((dictf = lookup(namef, userdict)) == 0L) return UNDF;
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

//------------------------ nextevent ------------
// For the mill:
// buffer is a string frame to buffer strings that come in on
// socket events.
//
// This:
// 1) check for more X events.
// 2) looks for socket activity. If none, no X pending, and no
//    activity in the d-machine pending, it blocks until something
//    happens on a socket. Activity in the d-machine is defined by
//    the pending() function, which is defined in dvt.o or dnode.o.
// 3) If there is activity on the X socket, moreX is set
// 4) If no other activity is seen, handle the X event and return.
// 5) Check for new connections via serverinput. If some exists, begin again.
// 6) Check for activity on the rest of the sockets. The next active socket
//       is handled round-robin style.
// 7)    Handle console activity via consoleinput, and begin anew.
// 8)    Otherwise, read a d object from the socket and call clientinput
//       to handle it.
//       If we have an error on the socket, call makesocketdead error handling.
// 9) Loop again: if we have an error return the error. If we have d-activity pending
//        bail out.

P nextevent(B* buffer) {
  P nact, retc = OK;
  fd_set read_fds;
  static const struct timeval zerosec_ = {0, 0};
  static struct timeval zerosec;

  do {
    if (abortflag) return ABORT;
    read_fds = sock_fds;
#if ! X_DISPLAY_MISSING
    if (dvtdisplay != NULL && ! moreX) {
      HXFlush(dvtdisplay);
      moreX = HQLength(dvtdisplay);
    }
#endif
 
    zerosec = zerosec_;
    if (maxsocket 
	&& ((nact = select(maxsocket, &read_fds, NULL, NULL,
			  moreX || pending() ? &zerosec : NULL))
	    < 0)) {
      if (errno == EINTR) continue;
      else error(EXIT_FAILURE, errno, "select");
    }

#if ! X_DISPLAY_MISSING
    if (dvtdisplay != NULL && FD_ISSET(xsocket, &read_fds)) {
      moreX = TRUE;
      FD_CLR(xsocket, &read_fds);
      nact--;
    }
    if (! nact) {
      if (moreX && (retc = nextXevent())) errsource = "X service";
      continue;
    }
#endif
 
  /* starting from the first socket after the last serviced socket, we
     find the next active socket and service it */

    if (serverinput(&retc, &read_fds)) {
      if (retc) {
	errsource = "Server service";
	continue;
      }
      if (!--nact) continue;
    }

    if (nextsocket(&read_fds)) {
      if (consoleinput(&retc, buffer)) {
	errsource = "Console service";
	continue;
      }
    
      switch(retc = fromsocket(recsocket, buffer)) {
	case OK:
	  retc = clientinput();
	  break;
	
	default:
	  retc = makesocketdead(retc, recsocket, "Client service");
	  break;
      }
    }
  } while (! retc && ! pending());

  return retc;
}

#endif //DISABLE_NEXTEVENT
