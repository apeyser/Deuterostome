/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
/* ---------------- D machine 3.0 (Linux) dvt.c ----------------------- 

   This is the root module of the D Virtual Terminal. It involves
   dvt-specific include modules that provide operators of the dvt.
*/

#include "dm.h"

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
#include "xhack.h"
#include "dm3.h"
#include "dregex.h"
#include "dm-nextevent.h"
#include "dm-vm.h"
#include "dm-dvt.h"
#include "dm2.h"
#include "dm4.h"
#include "dm-proc.h"
#include "dm-glob.h"
#include "dm7.h"
#include "dm6.h"
#include "dm5.h"
#include "dm8.h"
#include "error-local.h"

/*----------------- DM global variables -----------------------------*/

/*----------------------- for the dvt module ------------------------*/

/*------------------- include modules of the dvt ---------------------*/

#include "dvt_0.h"

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
  P retc;

  sysop = _sysop;
  syserrm = _syserrm;
  syserrc = _syserrc;

  serialized = TRUE; // no serialize operator

 /*----------------- include stdin into socket table */ 
  initfds();
  /* we monitor console input */
  if ((consolesocket = dup(STDIN_FILENO)) == -1)
    dm_error(errno, "Unable to dup stdin");
  if ((retc = addsocket(consolesocket, &sockettype, &defaultsocketinfo)))
    dm_error(retc < 0 ? -retc : 0, "Unable to add console socket");

 /*-------------- fire up Xwindows (if there is) -----------------------*/
#if ! X_DISPLAY_MISSING
  dvtdisplay = XOpenDisplay(NULL);  /* use the DISPLAY environment */
  if (dvtdisplay && HDisplayString(dvtdisplay)) {
    strncpy(displayname, HDisplayString(dvtdisplay), sizeof(displayname)-1);
    displayname[sizeof(displayname)-1] = '\0';
    dvtscreen = HXDefaultScreenOfDisplay(dvtdisplay);
    dvtrootwindow = HXDefaultRootWindow(dvtdisplay);
    if (HXGetWindowAttributes(dvtdisplay,dvtrootwindow,&rootwindowattr) == 0)
      dm_error(0, "Xwindows: no root window attributes");
    ndvtwindows = 0; 
    ncachedfonts = 0;
    dvtgc = HXCreateGC(dvtdisplay,dvtrootwindow,0,NULL);
    xsocket = ConnectionNumber(dvtdisplay);
    if ((retc = addsocket(xsocket, &sockettype, &defaultsocketinfo)))
      dm_error(retc < 0 ? -retc : 0, "Unable to add x socket");
  }
  else {
    dvtdisplay = NULL;
    *displayname = '\0';
  }
#endif
 

/*--------------------- set up the tiny D machine -------------------
   Not so tiny for the dvt, this should be good for most work
*/

  run_dvt_mill();
 /*-------------------------- run the D mill --------------------- */
} /* of main */

