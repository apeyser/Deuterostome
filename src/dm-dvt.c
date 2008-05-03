#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "dmx.h"
#if ! X_DISPLAY_MISSING
#include "xhack.h"
#endif //! X_DISPLAY_MISSING

#include "dm-dvt.h"
#include "dm-nextevent.h"
#include "dm3.h"
#include "dm-vm.h"
#include "dm2.h"

static LBIG memsetup[5] = { 1000, 100, 20, 10, 200 };


P toconsole(B *p, P atmost)
{
  P nb;
  if (atmost == -1) atmost = strlen((char*)p);
  while (atmost > 0) {
  tc1:
    if (abortflag) {abortflag = FALSE; return ABORT;}
    if ((nb = write(1, p, atmost)) < 0) {
      if ((errno == EINTR) || (errno == EAGAIN)) goto tc1;
      else return -errno;
    }
    atmost -= nb; p += nb;
  }
  return OK;
}

/*--------------------------- read a line from the console keyboard
    Tries to read a full line (terminated by '\n') from the console
    into the provided string buffer object. On success, the substring
    representing the line minus the '\n' character is inserted into
    the string buffer frame supplied to 'fromconsole'. Several abnormal
    returns can occur.
 */

DM_INLINE_STATIC P fromconsole(void)
{
  P nb, nsbuf, atmost;
  B *p, *sbuf;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) < DVTSTRINGBUFSIZE) return RNG_CHK;

  nsbuf = ARRAY_SIZE(o_1);
  sbuf = (B *)VALUE_BASE(o_1);

/*----- we give ourselves 10 sec */
  alarm(10);
  timeout = FALSE;
/* we read until we have a \n-terminated string */
  p = sbuf; 
  atmost = nsbuf;
 rc1:
  if (atmost <= 0) return RNG_CHK;
  if (timeout) return BAD_MSG;
  nb = read(0, p, atmost);
  if (nb < 0) {
    if ((errno == EAGAIN) || (errno == EINTR)) goto rc1;
    else return -errno;
  }
  if (nb == 0) {p++; goto ec1;}
  p += nb; 
  atmost -= nb;
  if (*(p-1) != '\n') goto rc1;
 /* we trim the buffer string object on the operand stack */
 ec1:
  ARRAY_SIZE(o_1) = p - sbuf - 1;
  return OK;
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

P op_error(void)
{
  LBIG e; 
  B *m;
  B *p, strb[256];
  P nb, atmost; 

  if (o_2 < FLOORopds) goto baderror;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_1) != NUM) goto baderror;
  if (!VALUE(o_1,&e)) goto baderror;

  p = strb; 
  atmost = 255;
  nb = dm_snprintf((char*)p,atmost,"\033[31m");
  p += nb; 
  atmost -= nb;
 
  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*)p,atmost,(char*)strerror(-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = dm_snprintf((char*)p,atmost,(char*)m);
  }

  p += nb; 
  atmost -= nb;
  nb = dm_snprintf((char*)p,atmost," in %s\033[0m\n", (char*)VALUE_BASE(o_2));
  nb += (P) (p - strb);
  toconsole(strb, nb);
  FREEopds = o_2;
  //return op_abort();
  return ABORT;

 baderror: 
  toconsole((B*)"Error with corrupted error info on operand stack!\n", -1L);
  //return op_abort();
  return ABORT;
}

/*-------------------------------------- 'errormessage'
  use: instance_string error-numeral stringbuf | substring_of_stringbuf

  - composes an error message and returns it in a subarray of string buffer
*/

P op_errormessage(void)
{
  LBIG e;
  P nb, tnb; 
  B *m, *s;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (!VALUE(o_2,&e)) return UNDF_VAL;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  s = (B *)VALUE_BASE(o_1); tnb = ARRAY_SIZE(o_1);
  if ((P)e < 0)/*Clib error */
    nb = dm_snprintf((char*)s,tnb,(char*)strerror(-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = strlen((char*)m);
    if (nb > tnb) nb = tnb;
    moveB(m,s,nb);
  }
  s += nb; 
  tnb -= nb;
  nb = dm_snprintf((char*)s,tnb," in %s\n", (char*)VALUE_BASE(o_3));
  ARRAY_SIZE(o_1) = (P)(s + nb) - VALUE_BASE(o_1);
  moveframe(o_1,o_3);
  FREEopds = o_2;
  return OK;
}

/*--------------------------------------- aborted
   use:  active_object | --

   - pushes boolean FALSE with ABORTMARK attribute on execution stack
   - pops operand and pushes it on execution stack

*/

P op_aborted(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (x3 > CEILexecs) return EXECS_OVF;
  TAG(x1) = BOOL; 
  ATTR(x1) = (ABORTMARK | ACTIVE);
  BOOL_VAL(x1) = FALSE;
  moveframe(o_1,x2); 
  FREEopds = o_1;
  FREEexecs = x3;
  return OK;
}

/*--------------------------------------- abort
   - drops execution stack to level above nearest ABORTMARK object (a BOOL)
   - sets the boolean object that carries ABORTMARK to TRUE
*/

P op_abort(void)
{
  B *frame; 

  frame = FREEexecs;
  while ((frame -= FRAMEBYTES) >= FLOORexecs) {
    if ((ATTR(frame) & ABORTMARK) != 0){ 
      BOOL_VAL(frame) = TRUE;
      FREEexecs = frame + FRAMEBYTES; return OK;
    }
  }
  toconsole((B*)"**Execution stack of dvt exhausted!\n", -1L);
  return QUIT;
}

/*---------------------------------------------------- toconsole
     string | ---

  - prints string operand on console
*/
P op_toconsole(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  FREEopds = o_1;
  return toconsole((B *)VALUE_BASE(o1), ARRAY_SIZE(o1));
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

static BOOLEAN ispending;

P op_nextevent(void)
{
  B bufferf[FRAMEBYTES];
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) < DVTSTRINGBUFSIZE) return RNG_CHK;
  
  moveframe(o_1, bufferf);
  FREEopds = o_1;

  ispending = FALSE;
  return nextevent(bufferf);
}

BOOLEAN pending(void) {return ispending;}

P clientinput(void) {
  if (x1 >= CEILexecs) return EXECS_OVF;

  makename((B*)"nodemessage", x1);
  ATTR(x1) = ACTIVE;
  FREEexecs = x2;
  ispending = TRUE;
  return OK;
}


BOOLEAN masterinput(P* retc, B* bufferf) {
  if (recsocket != 0) return FALSE;
  if (o2 > CEILopds) {
    *retc = OPDS_OVF;
    return TRUE;
  }

  moveframe(bufferf, o1);
  FREEopds = o2;

  if ((*retc = fromconsole()) != OK) return TRUE;
  if (x1 >= CEILexecs) {
    *retc = EXECS_OVF;
    return TRUE;
  }

  makename((B*)"consoleline",x1);
  ATTR(x1) = ACTIVE;
  FREEexecs = x2;
  ispending = TRUE;
  return TRUE;
}

#if ! X_DISPLAY_MISSING
P wm_delete_window(XEvent* event __attribute__ ((__unused__)), 
		   B* userdict __attribute__ ((__unused__)) ) {
  HXBell(dvtdisplay, 0); 
  return OK;
}

DM_INLINE_STATIC P wrap_stop(P retc) {
  if (retc) return retc;

  if (o1 >= CEILopds) return OPDS_OVF;
  if (x_1 < FLOORexecs) return EXECS_UNF;

  moveframe(x_1, o1);
  FREEopds = o2;

  TAG(x_1) = OP;
  ATTR(x_1) = ACTIVE;
  OP_NAME(x_1) = (P) "pop";
  OP_CODE(x_1) = (P) op_pop;

  ispending = TRUE;
  return op_stopped();
}

P wm_take_focus(XEvent* event, B* userdict) {
  return wrap_stop(wm_take_focus_(event, userdict));
}

P wm_configure_notify(XEvent* event, B* userdict) {
  return wrap_stop(wm_configure_notify_(event, userdict));
}

P wm_expose(XEvent* event, B* userdict) {
  return wrap_stop(wm_expose_(event, userdict));
}

P wm_button_press(XEvent* event, B* userdict) {
  return wrap_stop(wm_button_press_(event, userdict));
}

#endif //! X_DISPLAY_MISSING

/* we push the errsource string followed by the error code on
   the operand stack, and 'error' on the execution stack 
*/

void makeerror(P retc, B* error_source) {
  if (retc == OPDS_OVF) FREEopds = FLOORopds;
  if (retc == EXECS_OVF) FREEexecs = FLOORexecs;
  if (o2 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;
  TAG(o1) = ARRAY | BYTETYPE; ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)error_source; 
  ARRAY_SIZE(o1) = strlen((char*)error_source);
  TAG(o2) = NUM | LONGBIGTYPE; 
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = retc;
  moveframe(errorframe,x1);
  FREEopds = o3; 
  FREEexecs = x2;
}

void run_dvt_mill(void) {
  P retc;
  B abortframe[FRAMEBYTES], *sf;
  B* startup_dvt;
  P nb, tnb;
  B* sysdict;
  B* userdict;
  B* p;
  int sufd;
  B *Dmemory;

  nb = FRAMEBYTES * (memsetup[0] + memsetup[1] + memsetup[2])
    + memsetup[3] * 1000000;
  Dmemory = (B *)malloc(nb+9);
  if (Dmemory == 0) error(EXIT_FAILURE, 0, "D memory");
  makeDmemory(Dmemory,memsetup);

  setuphandlers(NULL);
/* The system dictionary is created in the workspace of the tiny D machine.
   If the operator 'makeVM' is used to create a large D machine, this larger
   machine inherits the system dictionary of the tiny machine. We memorize
   the pointers of the tiny D memory so we can revert to the tiny setup.
*/
  if ((sysdict = makeopdict((B *)sysop, syserrc,  syserrm)) == (B *)(-1L))
    error(EXIT_FAILURE,0,"Cannot make system dictionary");
  if ((userdict = makedict(memsetup[4])) == (B *)(-1L))
    error(EXIT_FAILURE,0,"Cannot make user dictionary");

/* The first two dictionaries on the dicts are systemdict and userdict;
   they are not removable
*/
  moveframe (sysdict-FRAMEBYTES,FREEdicts); 
  FREEdicts += FRAMEBYTES;
  moveframe (userdict-FRAMEBYTES,FREEdicts); 
  FREEdicts += FRAMEBYTES;

  setupdirs();
/*----------------- construct frames for use in execution of D code */
  makename((B*)"error",errorframe); 
  ATTR(errorframe) = ACTIVE;
  makename((B*)"abort",abortframe); 
  ATTR(abortframe) = ACTIVE;

/*----------- read startup_dvt.d and push on execs ----------*/
  startup_dvt 
    = (B*) strcat(strcpy(malloc(strlen((char*)startup_dir) 
				+ strlen("/startup_dvt.d") + 1),
			 startup_dir),
		  "/startup_dvt.d");
 
  if ((sufd = open((char*)startup_dvt, O_RDONLY)) == -1)
    error(EXIT_FAILURE,errno,"Opening startup_dvt.d");
  tnb = 0; 
  sf = FREEvm; 
  p = sf + FRAMEBYTES;
  TAG(sf) = ARRAY | BYTETYPE; 
  ATTR(sf) = READONLY | ACTIVE | PARENT;
  VALUE_BASE(sf) = (P)p;
 
  while (((nb = read(sufd,p,CEILvm-p)) > 0) && (p <= CEILvm)) { 
    tnb += nb; 
    p += nb; 
  }
  if (nb == -1) error(EXIT_FAILURE,errno,"Reading startup_dvt.d");
  if (p == CEILvm) error(EXIT_FAILURE, ENOMEM,"startup_dvt.d > VM");
  ARRAY_SIZE(sf) = tnb;
  FREEvm += DALIGN(FRAMEBYTES + tnb);
  moveframe(sf,x1);
  FREEexecs = x2;

  while (1) {
    switch(retc = exec(1000)) {
      case MORE: case DONE: continue;

      case QUIT:     
#if ! X_DISPLAY_MISSING
	if (dvtdisplay) HXCloseDisplay(dvtdisplay);
	*displayname = '\0';
#endif
	exit(EXIT_SUCCESS);

      case ABORT:    
	if (x1 < CEILexecs) {
	  moveframe(abortframe,x1); 
	  FREEexecs = x2; 
	  continue;
	};

	retc = EXECS_OVF; 
	errsource = (B*)"supervisor"; 
	break;

      default: break;
    }

/*----------------------- error handler ---------------------------*/
    makeerror(retc, errsource);
  }
}
