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
// and then, in all cases:
//     pushes the socket on the opstack, and ~socketdead on the exec stack
P makesocketdead(P retc, P socketfd, B* error_socket) {
  if (socketfd == consolesocket) consolesocket = PINF;
  if (retc != LOST_CONN && retc >= 0) return retc;

  if (retc && retc != LOST_CONN) makeerror(retc, error_socket);

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

  do {
    if (abortflag) return ABORT;
    if ((nact = waitsocket(pending(), &read_fds)) < 0) {
      if (errno == EINTR) continue;
      else error(EXIT_FAILURE, errno, "select");
    }
    if (! nact) continue;
 
  /* starting from the first socket after the last serviced socket, we
     find the next active socket and service it */

    if (nextsocket(&read_fds)) {
      if (masterinput(&retc, buffer)) {
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
