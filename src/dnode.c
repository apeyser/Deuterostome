/*=================== D machine 3.0 (Linux) dnode.c ===================== 

                      Root module of 'dnode' D machines

*/

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>

#include "dm.h"
#include "dmx.h"
#include "xhack.h"
#include "dm3.h"
#include "dregex.h"
#include "dm-nextevent.h"

/*----------------- DM global variables -----------------------------*/


/*-------------------------- DM globals --------------------------------*/

/*-- setup: opds, execs, dicts, VM/MB, userdict */
LBIG tinysetup[5] = { 100, 50, 10, 1 , 100 };
LBIG setup[5];
B *tinyDmemory, *Dmemory = NULL;

/*-- the X corner */

char* defaultdisplay = NULL;

/*------------------------- for this module --------------------------*/

static BOOLEAN halt_flag;          /* execution block due to 'halt'     */
static B msf[FRAMEBYTES], cmsf[FRAMEBYTES];

/*------------------------- include modules of dnode -------------------*/

P op_loadlib(void);
P op_nextlib(void);
P op_getplugindir(void);
P op_makethreads(void);
P op_threads(void);
P op_Xconnect(void);
P op_socketdead(void) {return OK;}
#include "matrix.h"
#include "dm-convert.h"
#include "pluginlib.h"
#include "dnode_0.h"

// original directory for vmresize
char* original_dir;
P serverport;
B hostname[256];

#include "dnode_1.h"

/*----------------------- supervisor tools -------------------------*/

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

static void SIGABRThandler(int sig)
{
  fprintf(stderr, "Aborting on SIGABORT\n");
  abortflag = TRUE;
  signal(sig, SIGABRThandler);
}

/*---------- singnal handler: quit's */
static void quithandler(int sig) {
  int i;
  fprintf(stderr, "Exiting dnode on signal: %i\n", sig);
#if ! X_DISPLAY_MISSING
  if (dvtdisplay != NULL)
    {HXCloseDisplay(dvtdisplay); displayname[0] = '\0';}
#endif
    
  for (i = 0; i < FD_SETSIZE; i++)
    if (FD_ISSET(i, &sock_fds) && (i != xsocket)) close(i);
  exit(EXIT_SUCCESS);
}

static void makequithandler(void) 
{
  struct sigaction sa;
  int quitsigs[] = {SIGQUIT, SIGTERM, SIGINT, 0};
  int* i;
    
  sa.sa_handler = quithandler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 0;
  for (i = quitsigs; *i; i++) sigaction(*i, &sa, NULL);
}

static void ignorequithandler(void)
{
  struct sigaction sa;
  int quitsigs[] = {SIGHUP, 0};
  int* i;

  sa.sa_handler = SIG_IGN;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = 0;
  for (i = quitsigs; *i; i++) sigaction(*i, &sa, NULL);
}

		

/*------------------------------ main ----------------------------------

- usage: dnode portnumber

'dnode' runs as a TCP demon that serves port 'portnumber' until
it is killed. 'portnumber' is an offset into the range of port numbers
that is reserved to 'users' in the Linux system.

Dnode supports a model of distributed computing in projects that
involve multiple D machines. These communicate in a network such that
any node can send messages to any other, thereby providing data and 
instructions to fellow nodes. Typically, a cluster of D nodes is
controlled from a D virtual terminal (dvt), which is a specialized D
machine.

When a dnode starts up, it creates stacks and a VM of minimal capacities for
its D machine (it becomes a 'tiny' dnode). The tiny dnode provides an
'error' operator that prints error messages on the terminal used to start
the dnode process. Stack and VM capacities can be changed by the 'vmresize'
operator.

The typical first connection request made to a dnode comes from a dvt. The
dvt will initiate two actions: (1) using the 'vmresize' operator of the dnode,
the memory resources of the dnode's D machine will be set to sizes as 
needed for the job at hand. (2) startup code will be either transmitted or
read from the file system. In particular, the file 'startup_dnode.d' will be
included, which provides, among many other things, procedures that emulate
virtual terminal operators in the dnode, including a procedure 'error' that
monitors errors arising in the dnode on the console screen of the dvt.

Another action that alters the behavior of the dnode mill is connection
to an X windows server (usually that of the dvt), which can be established
and removed by the 'Xconnect' and 'Xdisconnect' operators ('vmresize'
includes the effect of 'Xdisconnect'). While an X server is connected, the
mill will monitor also X events (such as relevant mouse clicks; the dozing
behavior of the mill is modified (see below).

The D machine of a node is either running (that is, executing the objects
on its execution stack), or it is idle (when the execution stack is
exhausted or blocked following 'halt').

Unless a connectio to an X server exists, the idle D node dozes (it takes
no CPU time) until its port receives a message from another dnode or a dvt.
There are two kinds of node message: (1) connection/disconnection requests,
and (2) normal messages. If a message from an X server is detected, it is
dealt with as described below.

A connection request to the server port has priority over other messages and 
is honored by creating a socket for communication with the new client (dvt or
dnode). The socket will be removed when a message of zero length (indicating
that the client has disconnected) is received.

Normal messages (as transmitted by the 'send' operator) consist of a string
object and an optional object tree that is folded in a box. The supervisor code
of the receiving dnode (or the 'nextobject' operator of a receiving dvt) first
puts a socket-type 'null' object on the operand stack (this object opaquely
specifies the socket that received the message), followed by the root object of
the received object tree (or a simple 'null' object if no tree was included in
the message). The received string object is made executable and pushed on
the execution stack, thereby forcing its execution as the next object in
sequence.

Received messages use the following memory ressources. A received object tree
is appended to the VM (its removal requires explicit use of save/capsave/
restore). The contents of the received string object are buffered in a
pre-existing string object. The size of a received tree object is limited only
by the VM capacity, whereas the string object is limited by the string buffer
size (100 kB in a dnode).

A message that has an invalid format or that does not fit into the memory 
ressources of the receiving D machine, is discarded
 
A message that is received in this way by a dnode in effect interrupts the
ongoing execution of objects by executing the message string with top priority.
Each time the D mill is given control (so it can execute objects), it is allowed
to execute up to 100 objects before returning control to the supervisor (and
thus allowing another message to be received). In this way, brief activities
requested by a message have a fair chance to be fully executed before their
execution can be interrupted by a subsequent message. If a message takes less
than the 100 cycles to execute, the remainder of the 100 cycles goes to a
previous message whose execution has been interrupted.

While an X server is connected to the dnode, the idle dnode will not doze
indefinitely. Instead it will check for X events every 0.2 sec for up to
1 min, and then increase the scan interval to 1 sec. Socket messages from
dnodes or a dvt are detected without delay.

Three types of X server messages are propagated to the D level by pushing
one of the following active names on the execution stack:

       windowsize   - the window dimensions have been changed
       drawwindow   - an X window needs to be (re)drawn
       mouseclick   - a mouseclick into an X window has occurred

Furthermore, a dictionary associated with the window's name is pushed on the
dictionary stack; this dictionary must be defined in userdict and associated
there with a name that is concatenated from "w" followed by digits that
specify the window#. Other parameters needed to respond to the event are
placed on the operand stack; these are event-specific:

       | string                     (consoleline)
       | string                     (nodemessage)
       | rootobj string             (nodemessage)
       | width height               (windowsize)
       | --                         (drawwindow)
       | x y modifier               (mouseclick) 

Mouse clicks are interpreted in conjunction with modifier keys that are
simultaneously held down on the keyboard; the modifier is reported as
binary pattern (starting with the LSB: shift, capslock, modifier1,..,
modifier5, where the modifier keys depend on the specific keyboard).
If the mouse has more than one button (up to 5), these are reported
(from left to right) by modifier values of 0,1,2,4, or 8.

*/

int main(int argc, char *argv[])
{
  P nb, retc;
  char* endptr;

#if HAVE_SETSID
  // separate from current session - don't die if term closed.
  if (argc < 3) setsid(); 
  else if (argc == 3) {
    char* endptr;
    long ss = strtol(argv[2], &endptr, 10);
    if (! argv[2] || *endptr) usage_error(0);
    if (ss) setsid();
  }
#endif

#if ! X_DISPLAY_MISSING
  dvtdisplay = NULL;
  defaultdisplay = getenv("DISPLAY");
#endif

/*------------------------ get host name */

  if (gethostname((char*)hostname,255) == -1) 
    error(EXIT_FAILURE,errno,"gethostname");
  if (! (original_dir = getcwd(NULL, 0))) 
    error(EXIT_FAILURE,errno,"getcwd");

/*------------------------ parse arguments */
  if (argc < 2) usage_error(0);
  switch (serverport = strtol(argv[1], &endptr, 0)) {
    case LONG_MAX: case LONG_MIN: 
      if (errno) usage_error(errno);
  }
  if (! *(argv[1]) || *endptr) usage_error(errno);

  if (DM_IPPORT_USERRESERVED != DM_IPPORT_USERRESERVED_STANDARD)
    fprintf(stderr, 
	    "Unusual value for IPPORT_USERRESERVED: %i instead of %i\n",
	    DM_IPPORT_USERRESERVED, DM_IPPORT_USERRESERVED_STANDARD);
  serverport += DM_IPPORT_USERRESERVED;

/*----------------- SIGNALS that we wish to handle */
/* FPU indigestion is recorded in the numovf flag;
   we do not wish to be killed by it
*/

  numovf = FALSE;
  signal(SIGFPE, SIGFPEhandler);

/* The broken pipe signal is ignored, so it cannot kill us;
   it will pop up in attempts to send on a broken connection
*/

  signal(SIGPIPE, SIG_IGN);

/* We use alarms to limit read/write operations on sockets  */

  timeout = FALSE;
  signal(SIGALRM, SIGALRMhandler);

// Switched the following to SIGABRT, produced by kill -ABRT
// rather than control-c, that normally terminates the job
//
/* The interrupt signal is produced by the control-c key of the
   console keyboard, it triggers the execution of 'abort'
*/
  abortflag = FALSE;
  signal(SIGABRT, SIGABRThandler);
  
  makequithandler();
  ignorequithandler();

/*-------------------- prime the socket table -----------------------
  We use a fd_set bit array to keep track of active sockets. Hence,
  the number of active sockets is limited to the FD_SET_SIZE of
  the host system. 
*/
  FD_ZERO(&sock_fds);

/*------------- make server socket and listen on it -----------------

Note: all communication errors that can only be due to faulty code
(rather than an external condition) are reported through 'error' and
thus lead to termination of this process.
*/
  if ((serversocket = make_socket(serverport)) < 0) 
    error(EXIT_FAILURE, errno, "making internet server socket");
  if ((retc = closeonexec(serversocket)))
    error(EXIT_FAILURE, -retc, "setting close on exec on server socket");
  addsocket(serversocket);
  if (listen(serversocket,5) < 0) error(EXIT_FAILURE,errno,"listen");

#if ENABLE_UNIX_SOCKETS
  if ((unixserversocket = make_unix_socket(serverport)) < 0)
    error(0, errno, "making unix server socket");
  else if ((retc = closeonexec(unixserversocket)))
    error(EXIT_FAILURE, -retc, "setting close on exec on unix server socket");

  addsocket(unixserversocket);
  if (listen(unixserversocket,5) < 0) error(EXIT_FAILURE,errno,"listen");
#endif //ENABLE_UNIX_SOCKETS

/*--------------------- set up the tiny D machine -------------------*/
  nb = FRAMEBYTES * (tinysetup[0] + tinysetup[1] + tinysetup[2])
    + tinysetup[3] * 1000000;
  tinyDmemory = (B *)malloc(nb+FRAMEBYTES/2+1);
  if (tinyDmemory == 0) error(EXIT_FAILURE, 0, "not enough memory");
  maketinysetup();

/*----------------- construct frames for use in execution of D code */
  makename((B*)"error", errorframe); 
  ATTR(errorframe) = ACTIVE;

/*-------------- you are entering the scheduler -------------------*/\
/* We start with no D code on the execution stack, so we doze
   while waiting for socket (or console) activity.

   We scan sockets always in round-robin fashion across snapshots;
   so we maintain a rotating socket index.
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

      case KILL_SOCKS:
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

