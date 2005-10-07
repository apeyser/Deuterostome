
/*---------------- D machine 3.0 (Linux) dvt_1.c -------------------------

 This is an 'include' module for dvt.c and contains dvt-specific
 operators plus their support:

     - error
     - aborted
     - abort
     - toconsole
     - nextevent
     - send
     - getsocket
     - connect
     - disconnect

*/

/*-------------------- operator support ------------------------------*/

#include <string.h>

L toconsole(B *p, L atmost)
{
  L nb;
  if (atmost == -1) atmost = strlen(p);
  while (atmost > 0)
   { tc1:
  if (abortflag) { abortflag = FALSE; return(ABORT); }
     if ((nb = write(1, p, atmost)) < 0)
       { if ((errno == EINTR) || (errno == EAGAIN)) goto tc1;
            else return(-errno);
       }
     atmost -= nb; p += nb;
   }
  return(OK);
}

/*--------------------------- read a line from the console keyboard
    Tries to read a full line (terminated by '\n') from the console
    into the provided string buffer object. On success, the substring
    representing the line minus the '\n' character is inserted into
    the string buffer frame supplied to 'fromconsole'. Several abnormal
    returns can occur.
 */

static L fromconsole(void)
{
L nb, nsbuf, atmost;
B *p, *sbuf;

 if (o_1 < FLOORopds) return(OPDS_UNF);
 if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
 if (ARRAY_SIZE(o_1) < 8192) return(RNG_CHK);

 nsbuf = ARRAY_SIZE(o_1);
 sbuf = (B *)VALUE_BASE(o_1);

/*----- we give ourselves 10 sec */
 alarm(10);
 timeout = FALSE;
/* we read until we have a \n-terminated string */
 p = sbuf; atmost = nsbuf;
rc1:
 if (atmost <= 0) return(RNG_CHK);
 if (timeout) return(BAD_MSG);
 nb = read(0, p, atmost);
 if (nb < 0)
   {
     if ((errno == EAGAIN) || (errno == EINTR)) goto rc1;
     else return(-errno);
   }
 if (nb == 0) { p++; goto ec1; }   
 p += nb; atmost -= nb;
 if (*(p-1) != '\n') goto rc1;
 /* we trim the buffer string object on the operand stack */
 ec1:
 ARRAY_SIZE(o_1) = p - sbuf - 1;
 return(OK);
}

/*-------------------- DVT-specific operators -------------------------*/

/*-------------------------------------- 'error'
   use: instance_string error_numeral | (->abort)

  - Clib error numerals are negative errno of Clib
  - decodes the error numeral and writes message
  - executes 'abort'
  - NOTE: you do not want to report errors of D nodes this way
    because 'error' aborts; use 'nodeerror' instead, which simply reports
*/

L op_error(void)
{
L e; B *m;
B *p, strb[256];
L nb, atmost; 

if (o_2 < FLOORopds) goto baderror;
if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
if (CLASS(o_1) != NUM) goto baderror;
if (!VALUE(o_1,&e)) goto baderror;

 p = strb; atmost = 255;
 nb = snprintf(p,atmost,"\033[31m");
 p += nb; atmost -= nb;
 
if (e < 0) 
   { /*Clib error */
       nb = snprintf(p,atmost,(B*)strerror(-e));
   }
 else
   { /* one of our error codes: decode */
       m = geterror(e);
       nb = snprintf(p,atmost,m);
   }

 p += nb; atmost -= nb;
 nb = snprintf(p,atmost," in %s\033[0m\n", (B*)VALUE_BASE(o_2));
 nb += (L) (p - strb);
 toconsole(strb, nb);
FREEopds = o_2;
//return(op_abort());
return ABORT;

baderror: 
toconsole("Error with corrupted error info on operand stack!\n", -1L);
//return(op_abort());
return ABORT;
}

/*-------------------------------------- 'errormessage'
  use: instance_string error-numeral stringbuf | substring_of_stringbuf

  - composes an error message and returns it in a subarray of string buffer
*/

L op_errormessage(void)
{
L e, nb, tnb; B *m, *s;

if (o_3 < FLOORopds) return(OPDS_UNF);
if (TAG(o_3) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (CLASS(o_2) != NUM) return(OPD_CLA);
if (!VALUE(o_2,&e)) return(UNDF_VAL);
if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
s = (B *)VALUE_BASE(o_1); tnb = ARRAY_SIZE(o_1);
if (e < 0) 
   { /*Clib error */
     nb = snprintf(s,tnb,(B *)strerror(-e));
     if (nb > tnb) nb = tnb;
   }
 else
 { /* one of our error codes: decode */
     m = geterror(e);
     nb = strlen(m);
     if (nb > tnb) nb = tnb;
     moveB(m,s,nb);
 }
s += nb; tnb -= nb;
nb = snprintf(s,tnb," in %s\n", (B *)VALUE_BASE(o_3));
 if (nb > tnb) nb = tnb;
ARRAY_SIZE(o_1) = (L)(s + nb) - VALUE_BASE(o_1);
moveframe(o_1,o_3);
FREEopds = o_2;
return(OK);
}

/*--------------------------------------- aborted
   use:  active_object | --

   - pushes boolean FALSE with ABORTMARK attribute on execution stack
   - pops operand and pushes it on execution stack

*/

L op_aborted(void)
{
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if ((ATTR(o_1) & ACTIVE) == 0) return(OPD_ATR);
  if (x3 > CEILexecs) return(EXECS_OVF);
  TAG(x1) = BOOL; ATTR(x1) = (ABORTMARK | ACTIVE);
  BOOL_VAL(x1) = FALSE;
  moveframe(o_1,x2); FREEopds = o_1;
  FREEexecs = x3;
  return(OK);
}

/*--------------------------------------- abort
   - drops execution stack to level above nearest ABORTMARK object (a BOOL)
   - sets the boolean object that carries ABORTMARK to TRUE
*/

L op_abort(void)
{
  B *frame; 

  frame = FREEexecs;
  while ((frame -= FRAMEBYTES) >= FLOORexecs)
    {
      if ((ATTR(frame) & ABORTMARK) != 0)
	{ 
	  BOOL_VAL(frame) = TRUE; ATTR(frame) &= (~ABORTMARK);
	  FREEexecs = frame + FRAMEBYTES; return(OK);
	}
    }
  toconsole("**Execution stack of dvt exhausted!\n", -1L);
  return(QUIT);
}

/*---------------------------------------------------- toconsole
     string | ---

  - prints string operand on console
*/
L op_toconsole(void)
{
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  FREEopds = o_1;
  return(toconsole((B *)VALUE_BASE(o1), ARRAY_SIZE(o1)));
}

/*------------------------------------------------ makesocketdead
 * | socketobj
 * pushes "socketdead" on exec
 * pushes FALSE ABORTMARK on exec
 */
static L makesocketdead(L ret, L socket) {
	if (x3 > CEILexecs) return EXECS_OVF;
	if (o2 > CEILopds) return OPDS_OVF;

	TAG(o1) = NULLOBJ | SOCKETTYPE; ATTR(o1) = 0; LONG_VAL(o1) = socket;
	FREEopds = o2;

	makename("socketdead", x1); ATTR(x1) = ACTIVE; 
	TAG(x2) = BOOL; ATTR(x2) = (ABORTMARK | ACTIVE); BOOL_VAL(x2) = FALSE;
	FREEexecs = x3;

	return ret;
}

/*---------------------------------------------------- nextevent
    string_obj | eventarguments...

  Note: the behavior of this operator is somewhat complex as it affects
        all stacks:

  - the operator blocks until an event occurs and then schedules a response
    by pushing an active name that is specific for the event on the execution
    stack; the name is one of:

       consoleline  - a line is available from the console keyboard
       nodemessage  - a message from another node is available *
       windowsize   - the window dimensions have been changed
       drawwindow   - an X window needs to be (re)drawn
       mouseclick   - a mouseclick into an X window has occurred

       * Closing of a connection is not reported as an event; it is
         dealt with silently, within 'nextevent'

  - in events concerning X windows, a dictionary associated with the
    window's name is pushed on the dictionary stack; this dictionary must
    be defined in userdict and associated there with a name that is 
    concatenated from "w" followed by digits that specify the window#
  - other parameters needed to respond to the event are placed on the
    operand stack; these are event-specific:

       | string                     (consoleline)
       | string                     (nodemessage)
       | rootobj string             (nodemessage)
       | width height               (windowsize)
       | --                         (drawwindow)
       | x y modifier               (mouseclick) 

     - the string received with 'consoleline' or 'nodemessage' is
       buffered in string_obj (which should be 8 kB); the returned
       string is a substring of string_obj.

     - mouse clicks are interpreted in conjunction with modifier keys
       that are simultaneously held down on the keyboard; the modifier
       is reported as binary pattern (starting with the LSB: shift,
       capslock, modifier1,.., modifier5, where the modifier keys
       depend on the specific keyboard). If the mouse has more than
       one button (up to 5), these are reported (from left to right) by
       modifier values of 0,1,2,4, or 8.
*/

L op_nextevent(void)
{
 L nround, nact, i, retc, mod, wid;
 B namef[FRAMEBYTES], *dictf, *userdict, namestring[20];
 fd_set read_fds;
#if ! X_DISPLAY_MISSING
 XEvent event;
#endif

 if (o_1 < FLOORopds) return(OPDS_UNF);
 if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
 if (ARRAY_SIZE(o_1) < 8192) return(RNG_CHK);
 nround = 0;

 nextloop: 
/* we use zero timeout for the first 10 scans; thereafter we use
   0.2 sec timeout
*/
 if (abortflag) return(ABORT);
 read_fds = sock_fds;
#if ! X_DISPLAY_MISSING
 if (dvtdisplay != NULL && ! moreX) {
     XFlush(dvtdisplay);
     moreX = QLength(dvtdisplay);
 }
#endif
 
 nact = select(FD_SETSIZE, &read_fds, NULL, NULL,
               moreX ? &zerosec : NULL);

 if (nact < 0) 
     {
       if (errno == EINTR) goto nextloop;
       else error(EXIT_FAILURE, errno, "select");
     }

#if ! X_DISPLAY_MISSING
 if (dvtdisplay != NULL && FD_ISSET(xsocket, &read_fds)) {
     moreX = TRUE;
     FD_CLR(xsocket, &read_fds);
     nact--;
 }
 if (nact == 0) goto nextXwindows;
#endif
 

/* starting from the first socket after the last serviced socket, we
   find the next active socket and service it */

 for (i=0; i < FD_SETSIZE; i++) 
   { recsocket++; if (recsocket >= FD_SETSIZE) recsocket = 0;
     if (FD_ISSET(recsocket, &read_fds))
       {
	 if (recsocket == 0)
	   { /* we have console input */
	     if ((retc = fromconsole()) != OK) return(retc);
             if (x1 >= CEILexecs) return(EXECS_OVF);
             makename("consoleline",x1); ATTR(x1) = ACTIVE;
             FREEexecs = x2;
	     return(OK);
	   }
	 else
	   { /* we have node input */
             FREEopds = o_1;
             switch(retc = fromsocket(recsocket, o1))
             {
             case DONE: close(recsocket); FD_CLR(recsocket, &sock_fds);
							 return makesocketdead(OK, recsocket);
             case OK: if (x1 >= CEILexecs) return(EXECS_OVF);
               makename("nodemessage",x1); ATTR(x1) = ACTIVE;
               FREEexecs = x2;
               return(OK);
             default:   close(recsocket); FD_CLR(recsocket, &sock_fds);
							 return makesocketdead(retc, recsocket);
             }
	   }
       }
    }
 goto nextloop;

#if ! X_DISPLAY_MISSING
 nextXwindows:
 if (moreX) {
     userdict = (B *)VALUE_BASE(FLOORdicts + FRAMEBYTES);
     XNextEvent(dvtdisplay, &event);
     moreX = QLength(dvtdisplay);
     switch(event.type) {
		 case ClientMessage:
			 if (event.xclient.message_type 
					 == XInternAtom(dvtdisplay, "WM_PROTOCOLS", False)) {
				 if (event.xclient.data.l[0] 
						 == XInternAtom(dvtdisplay, "WM_DELETE_WINDOW", False))
					 XBell(dvtdisplay, 0);
				 else if (event.xclient.data.l[0]
									== XInternAtom(dvtdisplay, "WM_TAKE_FOCUS", False)) {
					 wid = event.xclient.window;
					 snprintf(namestring, sizeof(namestring), "w%d", wid);
					 makename(namestring, namef); ATTR(namef) = ACTIVE;
					 if ((dictf = lookup(namef, userdict)) == 0L) return UNDF;
					 if (FREEdicts >= CEILdicts) return DICTS_OVF;
					 moveframe(dictf, FREEdicts); FREEdicts += FRAMEBYTES;
					 if (x1 >= CEILexecs) return EXECS_OVF;
					 makename("take_input_focus", x1); ATTR(x1) = ACTIVE;
					 FREEexecs = x2;
				 }
			 }
			 return OK;

     case ConfigureNotify:
       wid = event.xconfigure.window;
       snprintf(namestring, sizeof(namestring), "w%d", wid);
       makename(namestring, namef); ATTR(namef) = ACTIVE;
       if ((dictf = lookup(namef, userdict)) == 0L) return(UNDF);
       if (FREEdicts >= CEILdicts) return(DICTS_OVF);
       if (x1 >= CEILexecs) return(EXECS_OVF);
       if (o1 >= CEILopds) return(OPDS_OVF);
       moveframe(dictf, FREEdicts); FREEdicts += FRAMEBYTES;
       makename("windowsize",x1); ATTR(x1) = ACTIVE; FREEexecs = x2;
       TAG(o_1) = (NUM | LONGTYPE); ATTR(o_1) = 0;
       LONG_VAL(o_1) = event.xconfigure.width;
       TAG(o1) = (NUM | LONGTYPE); ATTR(o1) = 0;
       LONG_VAL(o1) = event.xconfigure.height;
       FREEopds = o2;
       return(OK);
     case Expose:
       if (event.xexpose.count != 0) break;
       wid = event.xexpose.window;
       snprintf(namestring, sizeof(namestring), "w%d", wid);
       makename(namestring, namef); ATTR(namef) = ACTIVE;
       if ((dictf = lookup(namef, userdict)) == 0L) return(UNDF);
       if (FREEdicts >= CEILdicts) return(DICTS_OVF);
       if (x1 >= CEILexecs) return(EXECS_OVF);
       moveframe(dictf, FREEdicts); FREEdicts += FRAMEBYTES;
       makename("drawwindow",x1); ATTR(x1) = ACTIVE; FREEexecs = x2;
       FREEopds = o_1;
       return(OK);
     case ButtonPress:
       wid = event.xbutton.window;
       mod = (event.xbutton.state & 0xFF)
           | (event.xbutton.button << 16);
       if (FREEdicts >= CEILdicts) return(DICTS_OVF);
       if (x1 >= CEILexecs) return(EXECS_OVF);
       if (o2 >= CEILopds) return(OPDS_OVF);
       snprintf(namestring, sizeof(namestring), "w%d", wid);
       makename(namestring, namef); ATTR(namef) = ACTIVE;
       if ((dictf = lookup(namef, userdict)) == 0L) return(UNDF);
       moveframe(dictf, FREEdicts); FREEdicts += FRAMEBYTES;
       makename("mouseclick",x1); ATTR(x1) = ACTIVE; FREEexecs = x2;
       TAG(o_1) = (NUM | LONGTYPE); ATTR(o_1) = 0;
       LONG_VAL(o_1) = event.xbutton.x;
       TAG(o1) = (NUM | LONGTYPE); ATTR(o1) = 0;
       LONG_VAL(o1) = event.xbutton.y;
       TAG(o2) = (NUM | LONGTYPE); ATTR(o2) = 0;
       LONG_VAL(o2) = mod;
       FREEopds = o3;
       return(OK);
     }
 }

 goto nextloop;
#endif
}
