| Copyright 2011 Alexander Peyser & Wolfgang Nonner
|
| This file is part of Deuterostome.
|
| This program is free software: you can redistribute it and/or modify
| it under the terms of the GNU General Public License as published by
| the Free Software Foundation, either version 2 of the License, or
| (at your option) any later version.
|
| This program is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
| GNU General Public License for more details.
|
| You should have received a copy of the GNU General Public License
| along with this program.  If not, see <http://www.gnu.org/licenses/>.
/MASTER module

100 dict dup begin

|
| (hostname) port# SOCKET status
|
/nodelist [
  [ (master) null null false ]
  9 {[null null null false ]} repeat
] def

| /wrapper ~wrappee
/wrapdef {
  ~[MASTER ~begin ~[5 -1 roll] ~stopped ~end] bind userdict 3 -1 roll put
} bind def

|------------------------------------------ conn
|
| hostname port# | --
|

/_c_ {
  false 0 1 nodelist length 1 sub {/knode name
    nodelist knode get /node name
    node 0 get class /nullclass eq {pop true exit} if
  } for not { (Too many nodes!\n) toconsole stop} if
  2 copy connect node 2 put node 1 put node 0 put
  false node 3 put
  printnode
} bind def

/_c ~_c_ wrapdef

/_dc_ {
  /knode name
  nodelist knode get /node name
  node 0 get class /nullclass ne {
    node 2 get disconnect
    null node 0 put false node 3 put
    printnode
  } if
} bind def

/_dc ~_dc_ wrapdef

/pbuf 80 /b array def

/printnode_ {
  otherXwindows {drawnode} {
    node 0 get class /nullclass ne {
      pbuf 0 -4 knode * number node 0 get fax
      knode 0 ne {
        * node 1 get * number
        node 3 get {( - busy )} {(- ready )} ifelse fax
      } if
      0 exch getinterval toconsole
    } if
  } ifelse
} bind def

/printnode ~printnode_ wrapdef

/setready_ {/knode name
  nodelist knode get /node name false node 3 put
  printnode
} bind def

/setready ~setready_ wrapdef

/setbusy_ {/knode name
  nodelist knode get /node name true node 3 put
  printnode
} bind def

/setbusy ~setbusy_ wrapdef

|------------------ machinery of 'nodes' window -----------------------
|
| If Xwindows is available, we maintain a window that indicates the state
| of the dvt and currently connected dnodes (the 'nodes') and allows some
| functions to be initiated by mouse clicks.
|
| The nodes window starts with a single entry, 'dvt', in the top row,
| and adds new connections in lower rows. Each dnode is shown as
| 'hostname:port#'. The current owner(s) of the keyboard is(are) highlighted
| (blue, bold text). The status each dnode is indicated by the color of its
| background (white for 'ready' and green for 'busy'). Busy dnodes will not
| be given a new keyboard phrase unless the phrase is tagged by '!'.
|
| A simple mouse click into the nodes window selects a new owner of the
| keyboard, regardless of the state of dnodes. Note that this chooses a
| single node as the owner of the keyboard. This node is highlighted in
| the nodes window.
|
| A 'control' mouse click over a dnode selects the entire group of dnodes
| to which this dnode belongs as owners of the keyboard. All dnodes of the
| group are highlighted in the nodes window.
|
| A dnode that is hung in the busy state is reset by a 'shift-click' into
| its cluster window field (this resets only the lock that makes the dvt
| reject normal keyboard phrases for this dnode; the dnode itself is not
| altered).
|
| The services of the cluster window are duplicated through dvt procedures
| that you can invoke from the keyboard (obviously this is your resort when
| Xwindows is not available).

/wW 200 2 add def
/wH nodelist length 13 mul 2 add def
/woutline [ 0 0 wW 1 sub 0 wW 1 sub wH 1 sub 0 wH 1 sub 0 0 ] def
/noderects [
    /x 1 def
    1 13 nodelist length 1 sub 13 mul 1 add { /y name
       /r 4 /w array def
       x r 0 put  y r 1 put wW 2 sub r 2 put 13 r 3 put
       r
       } for
  ] def
/nodelocs [
    /x 5 def
    12 13 nodelist length 1 sub 13 mul 12 add { /y name
       [ x y 6 sub ] mkact
       } for
  ] def
/NORMALFONT 
    (-b&h-lucida-medium-r-normal-sans-0-0-75-75-p-0-iso8859-10) def
/BOLDFONT 
    (-b&h-lucida-bold-r-normal-sans-0-0-75-75-p-0-iso8859-10) def
/BLACK <d 0 0 0 > mapcolor def
/BLUE  <d 0 0 1 > mapcolor def
/RED   <d 1 0 0 > mapcolor def
/BG <d 235 243 248 > 255 div mapcolor def
/HBG <d 166 219 160 > 255 div mapcolor def
/NORMALTEXT [ NORMALFONT BLACK -1 0 ] def
/HIGHTEXT  [ BOLDFONT BLUE -1 0 ] def

|--------------------------------------------- draw cluster window

/drawnode {
  wid noderects knode get 
  node 3 get { HBG } { BG } ifelse fillrectangle
  node 0 get class /nullclass ne {
    wid nodelocs knode get exec
    pbuf 0 node 0 get fax
    knode 0 ne {
      (:) fax * node 1 get * number
    } if 
    0 exch getinterval
    NORMALTEXT
    drawtext node 5 get exec pop pop pop
  } if
} bind def

|------------------------------------------------- mouseclick
| - a simple mouse click selects a node

|-- mouse key combos
/plain1 {
  mB 1 eq mM 0 eq and
} bind def

/shift1_plain2 {
  mB 1 eq mM 1 eq and
  mB 2 eq mM 0 eq and or
} bind def

/ctrl1_plain3 {
  mB 1 eq mM 4 eq and
  mB 3 eq mM 0 eq and or
} bind def

/c__ {
  0 1 nodelist length 1 sub { /knode name
    nodelist knode get /node name
    printnode
  } for
} bind def

/c_ ~c__ wrapdef

|--------------------------------------------- resist resizing

/windowsize { pop pop
  { [
    wid wW wH resizewindow
  } stopped end cleartomark
} bind def
  
/drawwindow {
  { [ busy { stop } if     | stop capsule + reentry prevention
    /busy true def
    wid woutline BLACK drawline
    0 1 nodelist length 1 sub { /knode name
      nodelist knode get /node name
      drawnode
    } for
  } stopped cleartomark /busy false def
  end
} bind def

/mouseclick { 
  { [ 4 1 roll busy { stop } if  | stopped and reentry lock
    /busy true def
    dup -16 bitshift /mB name 255 and /mM name
    /mY name /mX name 
    /knode mY 1 sub 13 div def
    nodelist knode get /node name
    node 0 get class /nullclass eq {stop} if
    plain1 {false nodelist knode get 3 put c_ stop} if
  } stopped cleartomark /busy false def
  end
} bind def
  
otherXwindows {
  screensize /scrH name /scrW name
  [ [ scrW wW sub 10 sub scrH wH sub 10 sub wW wH ]
    (TheMules) (Mules) makewindow /wid name pop
  MASTER userdict debug_dict /line get 0 (/w) fax * wid * number
      0 exch getinterval mkact exec put
  /busy false def
  wid true mapwindow  
} if


end _module
