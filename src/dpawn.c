/*=================== D machine 3.0 (Linux) dnode.c ===================== 

                      Root module of 'dnode' D machines

*/

#include "dm.h"

#include <unistd.h>
#include <time.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>
#include <mpi.h>

#include "dm.h"
#include "dm-nextevent.h"
#include "dm-mpi.h"
#include "dm-vm.h"
#include "dm7.h"

/*----------------- DM global variables -----------------------------*/


/*-------------------------- DM globals --------------------------------*/


/*------------------------- include modules of dnode -------------------*/

#include "pluginlib.h"
#include "matrix.h"
#include "dm-convert.h"
#include "pluginlib.h"
#include "threads.h"
#include "dm-dpawn.h"
#include "dregex.h"
#include "dm6.h"
#include "dm5.h"


#include "dpawn_0.h"

/*------------------------------ main ----------------------------------

- usage: dpawn

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
the memory resockets of the dnode's D machine will be set to sizes as 
needed for the job at hand. (2) startup code will be either transmitted or
read from the file system. In particular, the file 'startup_dpawn.d' will be
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

Received messages use the following memory ressockets. A received object tree
is appended to the VM (its removal requires explicit use of save/capsave/
restore). The contents of the received string object are buffered in a
pre-existing string object. The size of a received tree object is limited only
by the VM capacity, whereas the string object is limited by the string buffer
size (100 kB in a dnode).

A message that has an invalid format or that does not fit into the memory 
ressockets of the receiving D machine, is discarded
 
A message that is received in this way by a dnode in effect interrupts the
ongoing execution of objects by executing the message string with top priority.
Each time the D mill is given control (so it can execute objects), it is allowed
to execute up to 100 objects before returning control to the supervisor (and
thus allowing another message to be received). In this way, brief activities
requested by a message have a fair chance to be fully executed before their
execution can be interrupted by a subsequent message. If a message takes less
than the 100 cycles to execute, the remainder of the 100 cycles goes to a
previous message whose execution has been interrupted.

*/

int main(int argc DM_UNUSED, 
	 char *argv[] DM_UNUSED)
{
  sysop = _sysop;
  syserrm = _syserrm;
  syserrc = _syserrc;

  createfds();
  run_dpawn_mill();
}
