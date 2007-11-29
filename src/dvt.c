/* ---------------- D machine 3.0 (Linux) dvt.c ----------------------- 

   This is the root module of the D Virtual Terminal. It involves
   dvt-specific include modules that provide operators of the dvt.
*/

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <string.h>
#include "dm.h"
#include "dmx.h"

P init_sockaddr(struct sockaddr_in *name, const char *hostname, P port);

/*----------------- DM global variables -----------------------------*/

/*-- the X corner */
#if X_DISPLAY_MISSING
const
#endif
BOOLEAN moreX = FALSE;

/*----------------------- for the dvt module ------------------------*/

static struct timeval zerosec, somesec; /* timeouts for 'nextevent'  */
static LBIG memsetup[5] = { 1000, 100, 20, 10, 200 };

/*------------------- include modules of the dvt ---------------------*/

#include "dvt_0.h"
#include "dvt_1.h"

/*------------------------- root tools -------------------------------*/

/*--------- signal handler: SIGFPE */

static void SIGFPEhandler(int sig)
{
  numovf = TRUE;
  signal(sig, SIGFPEhandler);
}

/*---------- signal handler: SIGALRM */

static void SIGALRMhandler(int sig)
{
  timeout = TRUE;
  signal(sig, SIGALRMhandler);
}

/*---------- signal handler: SIGINT */

static void SIGINThandler(int sig)
{
  abortflag = TRUE;
  signal(sig, SIGINThandler);
}

/*------------------------------ main ----------------------------------

- usage: dvt

Run dvt in the foreground from an X terminal (in the absence of an X
Windows server, dvt can provide only basic console services).

'dvt' stands for D Virtual Terminal. The dvt is a D machine whose startup
code emulates a terminal for a cluster of D nodes. This includes a shared
text console and ad hoc interactive X windows. 

The D machine in the dvt is also available for normal use. Its typical
use will be in supervising a project that is executed in D nodes. The dvt
D machine deals with requests (made via the local keyboard or mouse or made
by supervised D nodes) sequentially, executing one request completely before
dealing with the next (this modus operandi is different from that of D
nodes, which take interrupts). D code written for the dvt must be 
apportioned to execute in brief, request-driven bursts if the terminal
functions of the dvt are to be kept available within reasonable response
times.

The most basic service of the dvt is to provide a console interface to
D machines. This console is the terminal from which dvt is started up.
Keyboard input is delivered to D machines in portions of a full line
(more specifically, in portions delimited by 'newline'). Editing and
type-ahead features of the original terminal continue to be usable.

Portions of keyboard input can be tagged for specific uses by the first
character following 'newline'. These characters are from the set
(!@#$%^&). Their effects are programmable at the level of the D code
that implements the virtual terminal.

One keyboard signal has a hard-wired effect: control_c will force the
execution of the 'abort' operator, which typically reprimes the D code
that implements the virtual terminal.

The basal activity of the dvt is a polling loop that is implemented
in D code. A 'nextevent' operator recognizes service requests that arise
in the dvt or D nodes that it serves. All services are provided by D
procedures.

When started up, the dvt executes the file startup_dvt.d contained in the
current directory.

*/

int main(void)
{
  B* startup_dvt;
    
  B errorframe[FRAMEBYTES], abortframe[FRAMEBYTES], *sf;
  P nb, retc,tnb;
  B *sysdict, *userdict, *Dmemory, *p;
  int sufd;

  serialized = TRUE; // no serialize operator

/*---------------- time out settings for 'nextevent' */
  zerosec.tv_sec = 0; zerosec.tv_usec = 0;
  somesec.tv_sec = 0; somesec.tv_usec = 200000;

/*----------------- SIGNALS that we wish to handle -------------------*/
/* FPU indigestion is recorded in the numovf flag;
   we do not wish to be killed by it
*/
  numovf = FALSE;
  signal(SIGFPE, SIGFPEhandler);

/* The broken pipe signal is ignored, so it cannot kill us;
   it will pop up in attempts to send on a broken connection
*/
  signal(SIGPIPE, SIG_IGN);

/* We use alarms to time-limit read/write operations on sockets  */
  timeout = FALSE;
  signal(SIGALRM, SIGALRMhandler);

/* The interrupt signal is produced by the control-c key of the
   console keyboard, it triggers the execution of 'abort'
*/
  abortflag = FALSE;
  signal(SIGINT, SIGINThandler);

/*-------------------- prime the socket table -----------------------
  We use a fd_set bit array to keep track of active sockets. Hence,
  the number of active sockets is limited to the FD_SET_SIZE of
  the host system.
*/
  FD_ZERO(&sock_fds);

 /*----------------- include stdin into socket table */ 
  FD_SET(0, &sock_fds);                 /* we monitor console input */
  recsocket = 0;

 /*-------------- fire up Xwindows (if there is) -----------------------*/
#if ! X_DISPLAY_MISSING
  dvtdisplay = XOpenDisplay(NULL);  /* use the DISPLAY environment */
  if (dvtdisplay && DisplayString(dvtdisplay)) {
    dvtscreen = XDefaultScreenOfDisplay(dvtdisplay);
    dvtrootwindow = XDefaultRootWindow(dvtdisplay);
    if (XGetWindowAttributes(dvtdisplay,dvtrootwindow,&rootwindowattr) == 0)
      error(EXIT_FAILURE,0,"Xwindows: no root window attributes");
    ndvtwindows = 0; 
    ncachedfonts = 0;
    dvtgc = XCreateGC(dvtdisplay,dvtrootwindow,0,NULL);
    xsocket = ConnectionNumber(dvtdisplay);
    FD_SET(xsocket, &sock_fds);
  } 
  else {
    dvtdisplay = NULL;
    *displayname = 0;
  }
#endif
 

/*--------------------- set up the tiny D machine -------------------
   Not so tiny for the dvt, this should be good for most work
*/

  nb = FRAMEBYTES * (memsetup[0] + memsetup[1] + memsetup[2])
    + memsetup[3] * 1000000;
  Dmemory = (B *)malloc(nb+9);
  if (Dmemory == 0) error(EXIT_FAILURE, 0, "D memory");
  makeDmemory(Dmemory,memsetup);

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
  makename("error",errorframe); 
  ATTR(errorframe) = ACTIVE;
  makename("abort",abortframe); 
  ATTR(abortframe) = ACTIVE;

/*----------- read startup_dvt.d and push on execs ----------*/
  startup_dvt 
    = strcat(strcpy(malloc(strlen(startup_dir) + strlen("/startup_dvt.d") + 1),
                    startup_dir),
             "/startup_dvt.d");
 
  if ((sufd = open(startup_dvt, O_RDONLY)) == -1)
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
  
 /*-------------------------- run the D mill --------------------- */
 more:
  switch(retc = exec(1000)) {
    case MORE:     goto more;
    case DONE:     goto more;

    case QUIT:     
#if ! X_DISPLAY_MISSING
      if (dvtdisplay) XCloseDisplay(dvtdisplay);
      *displayname = '\0';
#endif
      exit(EXIT_SUCCESS);

    case ABORT:    
      if (x1 >= CEILexecs) goto execsovfl;
      moveframe(abortframe,x1); 
      FREEexecs = x2; 
      goto more;

    default:       goto derror;
  }

/*----------------------- error handler ---------------------------*/
 execsovfl:
  retc = EXECS_OVF; 
  errsource = "supervisor"; 
  goto derror;

 derror:
/* we push the errsource string followed by the error code on
   the operand stack, and 'error' on the execution stack 
*/
  if (retc == OPDS_OVF) FREEopds = FLOORopds;
  if (retc == EXECS_OVF) FREEexecs = FLOORexecs;
  if (o2 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;
  TAG(o1) = ARRAY | BYTETYPE; ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)errsource; 
  ARRAY_SIZE(o1) = strlen(errsource);
  TAG(o2) = NUM | LONGBIGTYPE; 
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = retc;
  moveframe(errorframe,x1);
  FREEopds = o3; 
  FREEexecs = x2;
  goto more;
} /* of main */

