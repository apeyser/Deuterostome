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
|===================== a scheduler for cluster operation ========================
|
| - schedules from a dvtsup the solving of a model in a cluster of dnodes
| - each dnode has the same code that solves the model
| - there is no limit on the number of dnodes
| - a job involves a two-dimensional variation in model parameters
| - a 'joblet' solves one variation and is the unit task of a dnode
| - joblets are solved using continuation from the results of another
|   joblet (or a primer); this creates dependencies for the order of
|   solution of joblets
| - the dnodes to be used are defined in the list 'peers' of group numbers
| - the 'job' is defined by a list of lists of joblets (rows, columns)
| - each joblet includes:
|    - dependency test (procedure or active string)
|    - status (numeral: 0 - to do, 1 - spawned, 2 - done, 3 - failed)
|    - command to execute the joblet in a dnode (procedure or active string)
| - the console interface of the scheduler includes commands to start, halt,
|   continue, and close a job.
| - the graphical interface of the scheduler involves the 'nodes' window
|   (where the status of dnodes is indicated) and a 'job' window in which
|   each joblet is represented by a checkerboard field whose color
|   indicates the status of the joblet (white - to do; light green - spawned;
|   green - done; red - failed)
| - the scheduler is activated once when the job is started and once each
|   time when a joblet is completed (or has failed)
| - the dnode code that solves a joblet returns on the operand stack a
|   status (2 - done; 3 - failed), which is automatically reported back to
|   the job scheduler in the dvtsup
| - the scheduler attempts to spawn as many joblets as possible, given the
|   joblets left to do, their dependencies, and the availability of dnodes
| - joblets whose dependencies involve failed joblets will not be solved
| - the job scheduler can attend to several concurrent jobs (its commands
|   require a job specification)

100 dict dup begin

|---------------------------------------- startjob
| /jobdict | --
|
| Expects to find in 'jobdict':
|
| /job [ [ joblet..]..] def
| /peers [ group# ...] def
| /jobname (window title) def
|
| A joblet is described by the list"
|
| [ {dependency test} status {command} ] (procedures can be replaced by
|                                         active strings)
|
| The dependency test is executed in the dvtsup and usually inspects the job
| list to check dependencies; the test returns a boolean indicating resolution
| the joblet's dependencies.
|
| The command is given to the dnode and directs the dnode to insert model
| parameters, solve the model, and return a numeral indicating success (2) or
| failure(3) 

/startjob { dup find begin /jobdictname name
  screensize /scrH name /scrW name
  /BLACK <d 0 0 0 > mapcolor def
  /STATECOLORS [
    <d 1 1 1 > mapcolor                 | TODO (white)
    <d 0.5 1 0.5 > mapcolor             | SPAWNED (light green)
    <d 0.25 0.75 0.25 >  mapcolor       | DONE (green)
    <d 1 0.25 0.25 > mapcolor           | FAILED (red)
  ] def 
  /nrows job length def /ncols job 0 get length def
  /wW 2 ncols 13 mul add def
  /wH 2 nrows 13 mul add def
  /squares [
      0 1 nrows 1 sub { /krow name
          [
          0 1 ncols 1 sub { /kcol name
              [ kcol 13 mul 1 add krow 13 mul 1 add 13 13 ]
            } for
          ]
        } for
    ] def
  /woutline [ 0 0 wW 1 sub 0 wW 1 sub wH 1 sub 0 wH 1 sub 0 0 ] def
  [ [ scrW wW sub 10 sub 10 wW wH ] jobname dup makewindow /wid name
  currentdict userdict debug_dict /line get 0 (/w) fax * wid * number
    0 exch getinterval mkact exec put
  jobscheduler /windowsize get /windowsize name
  jobscheduler /drawwindow get /drawwindow name
  jobscheduler /mouseclick get /mouseclick name
  jobscheduler /finddnode get /finddnode name
  jobscheduler /showjoblet get /showjoblet name
  jobscheduler /buildresponse get  /buildresponse name
  wid true mapwindow
  /runflag true def
  pop jobdictname end
  schedule 
} bind def

|---------------------------------------- haltjob
| /jobdict | --

/haltjob { find begin /runflag false def end } bind def

|---------------------------------------- contjob
| /jobdict | --

/contjob { dup find begin
  /runflag true def
  schedule
  end
} bind def

|---------------------------------------- closejob
| /jobdict | --

/closejob { find begin
  wid dup false mapwindow deletewindow
  end
} bind def

|----------------------------- schedule -------------------------------
| /jobdict | --
|
| Called once when the job is started and then each time a joblet is
| completed.

/schedule { dup find begin /jobdictname name
  runflag {
      /krow 0 def
      job { /kcol 0 def
            { /joblet name
              joblet 1 get 0 eq {              | to do
                joblet 0 get exec {            | dependencies are resolved
                    finddnode { /knode name    | a node is ready
                        knode dvtsup begin setbusy end  | node goes busy
                        dvtsup /nodelist get knode get /node name
                        buildresponse
                        node 2 get 
                        [ ~[ dnoderesponse /response ~name
                             joblet 2 get {} forall       | do it
                             ~response completionidx ~put
                             ~console           | report back to dvtsup
                               2 ~list ~dup
                               ~response ~mkact ~exch 0 ~put
                               ~dup (exec) ~exch 1 ~put 
                               ~send
                          ]
                          (exec)
                        ] send                 | spawn it
                        1 joblet 1 put         | mark and show as spawned
                        showjoblet
                      } if
                  } if
                } if
              /kcol kcol 1 add def
            } forall
            /krow krow 1 add def
          } forall
    } if
  end
} bind def

|---------------------------------------- show joblet
|  (joblet, krow, kcol) | --

/showjoblet { 
  wid squares krow get kcol get STATECOLORS joblet 1 get get
  fillrectangle
} bind def

|---------------------------------------- build response: dnode -> dvtsup
| This is built originally in the dvtsup, to include job and dnode info, and
| then handed to the dnode. The dnode inserts its completion status and sends
| this as completion message to the dvt.

/buildresponse {
   [ jobdictname ~find ~begin
     krow /krow ~name kcol /kcol ~name
     ~job krow ~get kcol ~get /joblet ~name
     null ~joblet 1 ~put              | completion comes from dnode!
     ~showjoblet
     ~end
     ~dvtsup ~begin knode ~setready ~end
     jobdictname ~schedule
   ] /dnoderesponse name
   /completionidx 16 def
} bind def 

|---------------------------------------- find a ready dnode
| -- | knode true
|    | false

/finddnode {
  dvtsup begin
  false
  0 1 nodelist length 1 sub { /knode name
      nodelist knode get /node name
      node 0 get class /nullclass ne {
          node 3 get not {
              false peers { node 4 get eq { pop true exit } if } forall
                { pop knode true exit } if
            } if
        } if 
    } for
  end
} bind def

|-------------------------------- checkerboard window -------------------------

|------------------ resist resizing of window

/windowsize { pop pop
  { [
    wid wW wH resizewindow
  } stopped cleartomark
  end
} bind def

|------------------ draw window

/drawwindow {
  { [ 
    wid woutline BLACK drawline
    /krow 0 def
    job { 
        /kcol 0 def
        { /joblet name showjoblet
          /kcol kcol 1 add def
        } forall
        /krow krow 1 add def
      } forall
  } stopped cleartomark
  end
} bind def

|------------------ ignore mouseclick

/mouseclick { 
  pop pop pop
  end 
} bind def


/startjob find userdict /startjob put
/haltjob find userdict /haltjob put
/contjob find userdict /contjob put
/closejob find userdict /closejob put
/schedule find userdict /schedule put

end userdict /jobscheduler put

|-------------------------------- test ------------------------------------
| /job [ [ joblet..]..] def
| /peers [ group# ...] def
| /jobname (window title) def
|
| A joblet is described by the list"
|
| [ {dependency test} status {command} ]

/clearjob { find begin 
  job { { 0 exch 1 put } forall } forall
  end
} bind def

/testdict 50 dict dup begin

/job [ 3 { [ 5 { 3 list } repeat ] } repeat ] def
/peers [ -1 ] def
/jobname (testjob) def

job { { { 0.0 0.0 1.0 1e6 { add } for _ pop 2 } exch 2 put } forall } forall
/krow 0 def
job { /row name
      /kcol 0 def
      2 { ~[ ~job krow ~get kcol 1 ~add ~get 1 ~get 2 ~eq ] row kcol get 0 put
          /kcol kcol 1 add def
        } repeat
      krow 0 eq
        { ~[ ~true ] }
        { ~[ ~job krow 1 ~sub ~get kcol ~get 1 ~get 2 ~eq ] }
        ifelse row kcol get 0 put
        /kcol kcol 1 add def
      2 { ~[ ~job krow ~get kcol 1 ~sub ~get 1 ~get 2 ~eq ] row kcol get 0 put
           /kcol kcol 1 add def
        } repeat 
      /krow krow 1 add def
    } forall

end def
