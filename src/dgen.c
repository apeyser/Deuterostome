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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <string.h>
#include "dm.h"
#include "dmx.h"

/*---------- include modules of dgen --------------*/

B inputframe[FRAMEBYTES];
P op_fromconsole(void);

#include "dgen_0.h"
#include "dgen_1.h"

/*----------------- DM global variables -----------------------------*/

extern B *sysop[];
extern P errc[];
extern B *errm[];

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

int main(void)
{
  LBIG memsetup[5] = { 1000, 100, 20, 10, 200 };
  B* startup_dvt;    
  B errorframe[FRAMEBYTES], fromconsoleframe[FRAMEBYTES], *sf;
  P nb, retc,tnb;
  B *sysdict, *userdict, *Dmemory, *p;
  int sufd;

  serialized = TRUE; // no serialize operator

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

 /*--------------------- set up the tiny D machine -------------------
   Not so tiny for the dvt, this should be good for most work
*/

  nb = FRAMEBYTES * (memsetup[0] + memsetup[1] + memsetup[2])
    + memsetup[3] * 1000000;
  Dmemory = (B *)malloc(nb+9);
  if (Dmemory == 0) error(EXIT_FAILURE, 0, "D memory");
  makeDmemory(Dmemory,memsetup);
  
/*----------------- construct frames for use in execution of D code */
  makename("error",errorframe); ATTR(errorframe) = ACTIVE;
  makename("fromconsole",fromconsoleframe); ATTR(fromconsoleframe) = ACTIVE;
  TAG(FREEvm) = STRING;
  ARRAY_SIZE(FREEvm) = 1024;
  VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
  ATTR(FREEvm) = ACTIVE;
  moveframe(FREEvm, inputframe);
  FREEvm += FRAMEBYTES + 1024;

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

/*----------- read startup_dvt.d and push on execs ----------*/
  startup_dvt 
    = strcat(strcpy(malloc(strlen(startup_dir) 
                           + strlen("/startup_dgen.d") + 1),
                    startup_dir),
             "/startup_dgen.d");

  if ((sufd = open(startup_dvt, O_RDONLY)) == -1)
    error(EXIT_FAILURE, errno,"Opening %s", startup_dvt);
  tnb = 0; sf = FREEvm; p = sf + FRAMEBYTES;
  TAG(sf) = ARRAY | BYTETYPE; ATTR(sf) = READONLY | ACTIVE | PARENT;
  VALUE_BASE(sf) = (P)p;
  
  while (((nb = read(sufd,p,CEILvm-p)) > 0) && (p <= CEILvm)) { 
    tnb += nb; 
    p += nb; 
  }
  if (nb == -1) error(EXIT_FAILURE,errno,"Reading %s", startup_dvt);
  if (p == CEILvm) error(EXIT_FAILURE, ENOMEM,"%s > VM", startup_dvt);
  ARRAY_SIZE(sf) = tnb;
  FREEvm += DALIGN(FRAMEBYTES + tnb);
  moveframe(sf,x1);
  FREEexecs = x2;
  
  /*-------------------------- run the D mill --------------------- */
 more:
  switch(retc = exec(1000)) {
    case MORE:     goto more;
    case DONE:     moveframe(fromconsoleframe,x1); FREEexecs=x2; goto more;
    case QUIT:     printf("Success..\n"); exit(EXIT_SUCCESS);
    case ABORT:    printf("Failure...\n"); exit(EXIT_FAILURE);
    default:       goto derror;
 }

/*----------------------- error handler ---------------------------*/

 derror:
/* we push the errsource string followed by the error code on
   the operand stack, and 'error' on the execution stack 
*/
  if (retc == OPDS_OVF) FREEopds = FLOORopds;
  if (retc == EXECS_OVF) FREEexecs = FLOORexecs;
  if (o2 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;
  TAG(o1) = ARRAY | BYTETYPE; 
  ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)errsource; 
  ARRAY_SIZE(o1) = strlen(errsource);
  TAG(o2) = NUM | LONGBIGTYPE; ATTR(o2) = 0;
  LONGBIG_VAL(o2) = retc;
  moveframe(errorframe,x1);
  FREEopds = o3; FREEexecs = x2;
  goto more; 
} /* of main */

