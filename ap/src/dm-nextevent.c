#include "dm.h"
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>

#include "dm-nextevent.h"
#include "error-local.h"
#include "dm3.h"
#include "dm2.h"

// ------------------------------------- centralize socket handler

// -------- makesocketdead ------------------------
// This closes a socket, 
// then, if an error is pending:
//     pushes an error info on the opstack, and error on the exec stack
//     and true
//     replaces error with ~socketdead
// else
//    false on opstack
//    ~socketdead on execstack
// and then, in all cases:
//     pushes the socket on the opstack.
P makesocketdead(P retc, P socketfd, B* error_source) {
  if (retc == QUIT) return QUIT;

  if (socketfd == consolesocket) {
    consolesocket = PINF;
    consolesigsocket = PINF;
  }
  switch (retc) {
    case OK: case LOST_CONN: break;
    default: 
      makeerror(retc, error_source);
      if (FLOORexecs == FREEexecs) return EXECS_OVF;
      break;
  }

  delsocket_force(socketfd);

  if (o3 > CEILopds) return OPDS_OVF;

  TAG(o1) = BOOL;
  ATTR(o1) = 0;

  TAG(o2) = NULLOBJ | SOCKETTYPE; 
  ATTR(o2) = 0; 
  SOCKET_VAL(o2) = socketfd;
  DGRAM_VAL(o2) = -1;

  switch (retc) {
    case OK: case LOST_CONN: 
      if (x2 > CEILexecs) return EXECS_OVF;
      makename((B*)"socketdead", x1);
      ATTR(x1) = ACTIVE; 
      BOOL_VAL(o1) = FALSE;
      FREEexecs = x2;
      break;

    default:
      makename((B*) "socketdead", x_1);
      ATTR(x_1) = ACTIVE;
      BOOL_VAL(o1) = TRUE;
      break;
  }
  FREEopds = o3;
    
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
  ATTR(o1) = 0;
  if (consolesocket != PINF) {
    TAG(o1) |= SOCKETTYPE;
    SOCKET_VAL(o1) = consolesocket;
    DGRAM_VAL(o1) = consolesigsocket;
  }
  FREEopds = o2;
  return OK;
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
  if (TYPE(o_1) == SOCKETTYPE) {
    consolesocket = SOCKET_VAL(o_1);
    consolesigsocket = DGRAM_VAL(o_1);
  }
  else  {
    consolesocket = PINF;
    consolesigsocket = PINF;
  }
  FREEopds = o_1;
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
  if (! maxsocket) return FALSE;

  for (i=0; i < maxsocket; i++) { /* we go up to a full period, round robin */
    if (++recsocket >= maxsocket) recsocket = 0;

    if (FD_ISSET(recsocket, read_fds)) {
      FD_CLR(recsocket, read_fds);
      return TRUE;
    }
  }
  return FALSE;
}

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
  P retc = OK;
  fd_set read_fds;

  do {
    if (abortflag) return ABORT;
    if ((retc = waitsocket(moreX() || pending(), &read_fds))) {
      if (retc == NEXTEVENT_NOEVENT && (retc = nextXevent()))
	errsource = "X service";
      continue;
    }
 
  /* starting from the first socket after the last serviced socket, we
     find the next active socket and service it */

    if (nextsocket(&read_fds)) {
      if (masterinput(&retc, buffer)) {
	if (retc)
	  retc = makesocketdead(retc, recsocket, "Master service");
	continue;
      }
    
      if ((retc = fromsocket(recsocket, buffer)) ||
	  (retc = clientinput()))
	retc = makesocketdead(retc, recsocket, "Client service");
    }
  } while (! retc && ! pending());

  return retc;
}

#endif //DISABLE_NEXTEVENT
