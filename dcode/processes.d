| -*- mode: d; -*-
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

/PROCESSES 200 {
  |================== process ======================
  | ~active | ...
  |
  | executes active in PROCESSES -- equivalent to
  | PROCESSES indict, but predeclared in userdict.
  |
  /inprocess ~exec userdef

  | These need to be in the same order as
  |  flags struct array in dm-proc.c.
  /FFLAGS {
    /READ_ONLY
    /WRITE_TRUNCATE
    /WRITE_APPEND
  } makeenum def

  /_STD_FDS {
    /STDIN /STDOUT /STDERR /NULLR /NULLW /STDIN_ /STDOUT_ /STDERR_
  } makeenum def
  
  /_STD_READ [
    {/STDIN /NULLR /STDIN_} ~true forall
    {/STDOUT /STDERR /NULLW /STDOUT_ /STDERR_} ~false forall
  ] makestruct mkread def

  _STD_FDS {_STD_READ 2 index get makefd def} forall

  /fds ~[STDIN STDOUT STDERR] bind def
  
  /nfds ~[NULLR NULLW NULLW] bind def

  /fds_ ~[STDIN_ STDOUT_ STDERR_] bind def

  | (string) | --
  /tosystemconsole {
    PROCESSES /STDERR_ get exch writefd pop
  } bind def

  | These modes must match file enum modes in dm-proc.c
  /FILE_MODE [
    /USER_SET    4 8 3 pwr mul
    /GROUP_SET   2 8 3 pwr mul
    /STICKY      1 8 3 pwr mul
    /USER_READ   4 8 2 pwr mul
    /USER_WRITE  2 8 2 pwr mul
    /USER_EXEC   1 8 2 pwr mul
    /USER_ALL    7 8 2 pwr mul
    /GROUP_READ  4 8 1 pwr mul
    /GROUP_WRITE 2 8 1 pwr mul
    /GROUP_EXEC  1 8 1 pwr mul
    /GROUP_ALL   7 8 1 pwr mul
    /OTHER_READ  4 8 0 pwr mul
    /OTHER_WRITE 2 8 0 pwr mul
    /OTHER_EXEC  1 8 0 pwr mul
    /OTHER_ALL   7 8 0 pwr mul
    /ALL_RWX     7 8 mul 7 add 8 mul 7 add
    /ALL_RW      6 8 mul 6 add 8 mul 6 add
    /ALL         7 8 mul 7 add 8 mul 7 add 8 mul 7 add
  ] makestruct mkread def

  /FILE_MODE_BITS [
    /USER_SET
    /GROUP_SET
    /STICKY
    /USER_READ
    /USER_WRITE
    /USER_EXEC
    /GROUP_READ
    /GROUP_WRITE
    /GROUP_EXEC
    /OTHER_READ
    /OTHER_WRITE
    /OTHER_EXEC
  ] mkread def
  
  | mode# | [/mode...]
  /mkfile_mode {
    [
      exch FILE_MODE_BITS {    | /MODE... mode# /MODE
        FILE_MODE 1 index get  | /MODE... mode# /MODE mode-bits
        2 index 1 index and ne ~pop ~exch ifelse | /MODE... mode#
      } forall pop             | /MODE...
    ]
  } def

  | [/mode..] | mode
  /unmkfile_mode {
    0 exch {FILE_MODE exch get or} forall
  } def

  | (dir) (file) | fd
  /appopen {FFLAGS /WRITE_APPEND   get FILE_MODE /ALL_RW get openfd} bind def
  /wropen  {FFLAGS /WRITE_TRUNCATE get FILE_MODE /ALL_RW get openfd} bind def
  /rdopen  {FFLAGS /READ_ONLY      get FILE_MODE /ALL_RW get openfd} bind def

  | fd | --
  /close       {dup unmakefd 4 le ~pop ~closefd ifelse} bind def
  /closeifopen {dup closedfd      ~pop ~close   ifelse} bind def

  | (file)/fd | file-system-dict
  /statfs {
    statvfs [
      /BLOCK_SIZE
      /BLOCKS
      /BLOCKS_FREE
      /BLOCKS_AVAIL
      /FILES
      /FILES_FREE
      /FILES_AVAIL
      /FSID
      /RDONLY
      /NOSUID
      /NAME_MAX
      makestruct_stack |]
  } bind def

  | fdin fdout | --
  /cpstream {
    cp close
  } bind def

  | (dir-in) (file-in) (dir-out) (file-out)
  /cpfile {
    4 2 roll
    rdopen 3 1 roll
    wropen
    copystream
  } bind def

  | (dir-in) (file-in) (dir-out) (file-out)
  /mvfile {
    4 copy rename {4 ~pop repeat} {
      4 copy copyfile
      pop pop rmfile
    } ifelse
  } bind def

  | [\/mode..] / null | mode
  /_mkdir_mode {
    dup null eq {pop FILE_MODE /ALL_RWX get} ~unmkfile_mode ifelse
  } bind def

  | (dir) \[/mode..\] / null | --
  /mkdir_p {
    _mkdir_mode 
    4 {null} repeat openlist 
    /dir /mode 
    /ndir /ldir /fdir /n
  } {
    /ldir dir 0 0 getinterval def
    /n 0 def

    dir {                               | left
      (^/*[^/]+/*) regex not ~exit if   | left next-dir pre []
      pop pop /ndir name                | left

      /n n ndir length add def
      /fdir dir 0 n getinterval def
      ldir ndir existsfile not {
        fdir mode makedir
      } if
      /ldir fdir def
    } loop pop
  } localfunc bind def

  | (dir) \[/mode..\] / null | --
  /mkdir {openlist} {_mkdir_mode makedir} localfunc bind def

  | These types must match file enum modes in dm-proc.c
  /FILE_TYPE {
    /UNKNOWN
    /BLOCK
    /CHARACTER
    /REGULAR
    /DIRECTORY
    /LINK
    /SOCKET
  } makeenum mkread def

  | filetype# | /filetype
  /mkfile_type {
    FILE_TYPE {2 index eq ~exit if pop} forall
    exch pop
  } def

  | sec nsec | dict
  /mktime {
    [
      /SECONDS
      /NANOSECONDS
      makestruct_stack |]
  } def

  | time-dict1 time-dict2 | time-1 < time-2
  /lttime {
    1 index /SECONDS get 1 index /SECONDS get lt ~true {
      1 index /SECONDS get 1 index /SECONDS get gt ~false {
        1 index /NANOSECONDS get 1 index /NANOSECONDS get lt
      } ifelse
    } ifelse
    3 1 roll pop pop
  } def

  | time-dict1 time-dict2 | time-1 > time-2
  /gttime {
    1 index /SECONDS get 1 index /SECONDS get gt ~true {
      1 index /SECONDS get 1 index /SECONDS get lt ~false {
        1 index /NANOSECONDS get 1 index /NANOSECONDS get gt
      } ifelse
    } ifelse
    3 1 roll pop pop
  } def

  | time-dict1 time-dict2 | time-1-equals-time-2
  /eqtime {
    1 index /SECONDS get 1 index /SECONDS get eq ~true {
      1 index /NANOSECONDS get 1 index /NANOSECONDS get eq
    } ifelse
    3 1 roll pop pop
  } def

  | (dir) (file)
  /existsfile {
    stat not ~false {17 ~pop repeat true} ifelse
  } bind def

  | (dir) (file) / fd | dict true / false
  /statfile {
    stat not ~false {
      mkfile_type 8 1 roll
      mkfile_mode 8 1 roll
      mktime 7 1 roll
      mktime 6 1 roll
      mktime 5 1 roll

      [
        /DEV
        /INODE
        /NLINK
        /UID
        /GID
        /RDEV
        /SIZE
        /BLOCKSIZE
        /BLOCKS
        /ACCESS
        /MOD
        /STATUS
        /MODE
        /TYPE
        makestruct_stack |]
      true
    } ifelse
  } bind def

  | statfile-dict (dir) (file) | --
  /utimesput {
    3 -1 roll                   | (dir) (file) fileinfo
    {/ACCESS /MOD} {            | .. fileinfo /time
      1 index exch get          | .. fileinfo time-dict
      {/SECONDS /NANOSECONDS} { | .. fileinfo subtime.. time-dict /subtime
        1 index exch get exch   | .. fileinfo subtime.. time-dict
      } forall pop              | .. fileinfo subtime1 subtime2
      3 -1 roll                 | .. fileinfo
    } forall pop                | (dir) (file) access-sec access-ns mod-s mod-ns
    6 -2 roll utimes            | --
  } bind def

  | (dir) (file) | --
  /utimesset {
    4 {*} repeat 6 -2 roll utimes
  } bind def

  | ~active | ...
  /inpidsockets {
    {{{pidsockets indict} PROCESSES indict} enddicts} lock
  } bind def
  
  /pidsockets [
    /ll 400 {[null null false]} ~static makelist
    {/s /p /pd /t} ~null forall

    | [next data <<pid socket>>] | <<pid socket>>
    /_data {~data ll inlist} bind

    | list | --
    /_remove {~remove ll inlist} bind

    | ~active | bool
    /_iter {~iter ll inlist} bind

    | n | dict
    /_nth {~nth ll inlist} bind

    | [pid socket state] | pid
    /_getpid {0 get} bind

    | pid [null socket state] | --
    /_setpid {0 put} bind

    | [pid socket state] | --
    /_getsocket {1 get} bind

    | socket [pid null state] | --
    /_setsocket {1 put} bind

    | [pid socket state] | state
    /_getstate {2 get} bind

    | state [pid socket ?] | --
    /_setstate {2 put} bind

    |===================== add_process ==================
    | pid socket | --
    |
    | Add a process, socket pair for tracking.
    |
    /add_process {
      ~prepend ll inlist _data /pd name
      pd _setsocket
      pd _setpid
      true pd _setstate
    } bind

    |====================== _socketdead ===================
    | socket | bool-was-found
    |
    | Handle a socket disconnect event.
    | Pid may continue alive.
    |
    /_socketdead {/s name
      /t false def
      {dup /p name _data /pd name
        false
        pd _getsocket dup s ne ~pop {
          t ~pop ~disconnect ifelse
          null pd _setsocket 
          pd _getpid null eq {p _remove} {
            pd _getstate not {p _remove} if
          } ifelse
          pop false /t true def
        } ifelse
      } _iter pop t
    } bind

    |======================= piddead ========================
    | pid# | --
    |
    | Handle a process end event.
    | Socket is disconnected if active.
    |
    /piddead {/s name
      {dup /p name _data /pd name
        false
        pd _getpid dup null eq ~pop {
          unpid s eq {
            false pd _setstate
            pop true
          } if
        } ifelse
      } _iter pop
    } bind

    |======================= pidwaited ========================
    | pid | --
    |
    | Handle a process wait.
    | Socket is disconnected if active.
    | Removed from processes being tracked.
    |
    /pidwaited {/s name
      {dup /p name _data /pd name
        false
        pd _getpid dup null eq ~pop {
          s eq {
            false pd _setstate
            null  pd _setpid
            pd _getsocket null eq {p _remove} if
            pop true
          } if
        } ifelse
      } _iter pop
    } bind

    |==================== piddisconnect =====================
    | pid# | --
    |
    | Handle a disconnect request from a process.
    | Pid will continue alive.
    |
    /piddisconnect {/s name
      {dup /p name _data /pd name
        false
        pd _getpid dup null eq ~pop {
          unpid s eq {
            pd _getsocket dup null eq ~pop {
              disconnect
              null pd _setsocket
            } ifelse
            pop true
          } if
        } ifelse
      } _iter pop
    } bind

    | signal | --
    /killall {/s name
      {dup /p name _data /pd name
        pd _getpid dup null eq ~pop {
          pd _getstate {s killpid} ~pop ifelse
        } ifelse
        false
      } _iter pop
    } bind

    | -- | --
    /terminateall {
      {dup /p name _data /pd name
        pd _getpid dup null eq ~pop {
          pd _getstate {SIGNALS /TERM get killpid} ~pop ifelse
        } ifelse
        false
      } _iter pop

      {_data _getpid dup null ne ~wait if pop false} _iter pop
    } bind

    | -- | --
    /cleanall {
      {_remove false} _iter pop
    } bind

    |======================== job ============================
    | n | pid
    |
    | Get the pid for process #n (counting from 0), as listed
    |  by jobs.
    | Not stable after a wait.
    |
    /job {_nth _data _getpid} bind

    |======================= jobconsole =======================
    | n | socket
    |
    | Get the socket for process #n (counting from 0), as listed
    |  by jobs.
    | Not stable after a wait.
    |
    /jobconsole {_nth _data _getsocket} bind

    |========================= jobs ============================
    | -- | --
    |
    | List jobs: job#: pid: alive or dead
    | Not stable after a wait.
    |
    /jobs {
      0 {_data /pd name
        pd _getpid null ne {
          jobstr 0 * 3 index          * number                (: ) fax
                   * pd _getpid unpid * number                (: ) fax
                   pd _getstate {(alive)} {(dead)} ifelse fax (\n) fax
          0 exch getinterval toconsole
        } if
        1 add false
      } _iter pop 0 eq {(No jobs\n) toconsole} if
    } bind
    /jobstr 256 /b array

    /jobconsoles {
      0 {_data /pd name
        pd _getsocket null ne {
          jobstr 0 * 3 index                 * number         (: ) fax
                   * pd _getsocket socketval * number         (: ) fax
                   * pd _getpid dup null eq 
                     {pop -1} ~unpid ifelse  * number         (: ) fax
                   pd _getstate {(alive)} {(dead)} ifelse fax (\n) fax
        } if
        1 add false
      } _iter pop 0 eq {(No sockets\n) toconsole} if
    } bind
    
  ] makestruct def

  {{/die /die_base} {/quit /quit_base}} {
    exec /func_base name /func_name name
    systemdict func_name known {
      ~[
        ~[
          func_name {
            countdictstack 2 sub ~end repeat
            ~set_bgerror_ PROCESSES indict
            systemdict 1 index get def
            {terminateall _quitting} PROCESSES indict 
          } ~exec func_base ~mkact ~exec
        ] {~stopped aborted -1 die_base} ~lock
      ] bind userdict func_name put
    } if
  } forall

  /set_bgerror_ {
    {/error /error_base} {/bgerror_ find userdict 3 -1 roll put} forall
  } bind def

  | This must be in a ~stopped ~aborted -1 ~die_base lock capsule.
  /bgerror_ {
    countdictstack 2 sub ~end repeat
    /error_base ~abort bind def
    /error ~abort bind def
    /toconsole_base PROCESSES /tosystemconsole get def
    error_ops_length copy showerror

    console null ne {
      console ~[
        2 rollerror getpid unpid {
          {
            (From child ) fax * 4 -1 roll * number (: ) fax
            2 rollerror 1024 /b array errormessage fax 1 sub
          } warning restore
        } ~lock
      ] send
    } if
    abort
  } bind def

  /childproc false def

  | -- | --
  /_quitting {
    childproc {
      console dup null eq ~pop {
        ~[
          ~[
            getpid unpid {~piddead inpidsockets} ~PROCESSES ~indict
            ~restore
          ] ~lock
        ] send
      } ifelse
    } if
  } bind def

  /ENABLE_SEM {
    userdict /dm_type get /dnode eq {
      /get_threads ~threads bind def
      /set_threads ~makethreads bind def
    } {
      /inter_lock_set ~pop bind def
      /get_threads 0 def
      /set_threads ~pop bind def
    } ifelse
  } {
    /inter_lock_set ~pop bind def
    /get_threads 0 def
    /set_threads ~pop bind def
  } ifelse_compile

  | ~active fd-in fd-out fd-err \[fd-chained... | --
  | Never returns.
  /_bg_ {
    {
      ~cleanall inpidsockets
      true PROCESSES /childproc put
      set_bgerror_
      
      {dup class /markclass eq ~exit if closeifopen} loop pop
      {STDERR STDOUT STDIN} {exec
        1 index exch dupfd 3 1 roll
      } forall
      3 ~closeifopen repeat
      true inter_lock_set
      unlock {0} {1} ifelse die
    } ~stopped aborted -1 die_base
  } bind def

  |========================== bg_ ============================
  | ~active fd-in fd-out fd-err \[fd-chained... | pid
  |
  | Background a job. ~active is executed with fd's redirected
  |  to fd-in, fd-out, fd-err.
  | Returns process id.
  |
  | fd-chained operands are closed before executing ~active
  |  on child process. Useful for piping.
  |
  | ~active should return a boolean. If it returns false,
  |  the child exits with an error code of 1.
  | If the child throws an error, it returns -1.
  | The child calls ~_quitting when exiting to disconnect console.
  |
  /bg_ {
    {
      get_threads fork {setconsole set_threads _bg_} if | threads pid socket
      3 -1 roll pop                                     | pid socket

      1 index 3 1 roll ~add_process inpidsockets
      {exch dup class /markclass eq ~exit if pop} loop pop

      5 1 roll 4 ~pop repeat
    } lock
  } def

  |========================== bg ============================
  | ~active fd-in fd-out fd-err | pid
  |
  | Background a job. ~active is executed with fd's redirected
  |  to fd-in, fd-out, fd-err.
  | Returns process id.
  | See bg_.
  |
  /bg {openlist bg_} bind def

  |=============================== kill ===========================
  | pid /SIGNAME | --
  |
  | Sends signal SIGNAME to process pid.
  | SIGNALS are defined in startup_common.d.in
  |
  /kill {
    SIGNALS exch get killpid
  } bind def

  | /SIGNAME | --
  /killall {
    SIGNALS exch get ~killall inpidsockets
  } bind def

  | -- | --
  /terminateall {
    ~terminateall inpidsockets
  } bind def

  |=============================== job ============================
  | n | pid 
  |
  | Get pid for the nth proces, as listed by jobs, starting from 0.
  | Not stable after a ~wait.
  |
  /job {~job inpidsockets} bind def

  |=============================== jobconsole ============================
  | n | pid 
  |
  | Get the socket for the nth proces, as listed by jobs, starting from 0.
  | Not stable after a ~wait.
  |
  /jobconsole {~jobconsole inpidsockets} bind def

  |============================ jobs ==============================
  | -- | --
  | 
  | List running jobs as:
  | #: pid: alive or dead.
  | Not stable after a ~wait.
  |
  /jobs {~jobs inpidsockets} bind def

  /jobconsoles {~jobconsoles inpidsockets} bind def
  
  |============================ isdead ==============================
  | pid | bool true / -- false
  |
  | Checks for process end.
  | Returns true if process is dead,
  |   on top of true iff process returns status 0
  | Otherwise, just false.
  |
  | NB: if isdead returns true, the pid has been waited on.
  |
  /isdead {
    dup checkpid {pop wait true} {pop false} ifelse
  } bind def

  |============================ wait ==============================
  | pid | bool
  |
  | Block until process pid ends.
  | Returns true iff process returns status 0.
  |
  /wait {waitdie 0 eq} bind def

  |============================ waitdie ==============================
  | pid | status
  |
  | Block until process pid ends.
  | Returns status in format for die: (cooked-signal | 128 << 8)
  |                                   raw-signal << 16
  |                                   exit
  |
  /waitdie {
    dup waitpid
    exch ~pidwaited inpidsockets
  } bind def

  |================= exitstatus ====================
  | status | exit-status true
  |        | false
  /exitstatus {
    dup 255 and dup 3 1 roll eq ~true {pop false} ifelse
  } bind def

  |================ signal status ====================
  | status | /signalname  true
  |        | signalnumber true
  |        | false
  /signalstatus {
    dup 255 and 1 index eq {pop false} {
      -8 bitshift
      dup 255 and 0 eq {-8 bitshift} {
        127 and SIGNALS {
          2 index eq ~exit if
          pop
        } forall exch pop
      } ifelse
      true
    } ifelse
  } bind def

  |====================== signaldie =============================
  | /signalname | --
  | signalnum   | --
  /signaldie {
    dup class mkact signaldie_ indict
    or 8 bitshift die
  } bind def

  /signaldie_ {
    /nameclass {0l SIGNALS 3 -1 roll get 128 or}
    /numclass  {8 bitshift 0l}
  } bind makestruct def

  |============================ wait_ =============================
  | pid / pid (source) | --
  |
  | Blocks untils process pid ends.
  | If process does not return status 0, throws a NOSYSTEM error
  |   coming from (source).
  |
  /wait_done_buf 128 /b array def
  /wait_ {
    dup class /arrayclass ne {(wait)} if
    exch dup wait {
      wait_done_buf 0 (Done: ) fax * 5 -1 roll unpid * number (\n) fax
      0 exch getinterval toconsole
    } lock
    ~pop {/NOSYSTEM makeerror} ifelse
  } bind def

  |============================ wait_quiet =============================
  | pid / pid (source) | --
  |
  | Just like wait_, 'cept it doesn't message on completion
  /wait_quiet {
    dup class /arrayclass ne {(wait)} if
    exch wait ~pop {/NOSYSTEM makeerror} ifelse
  } bind def

  |======================= socketdead ============================
  | ... error-pending socket | -- <<error thrown>>
  |
  | Lives in userdict.
  |
  | Handler for console disconnect by child process.
  | Uses pidsockets to handle disconnect.
  | Throws a pending error if error-pending.
  | If the socket is unknown here, call previous socketdead
  |   handler in userdict.
  |
  /socketdead ~[
    ~[
      /socketdead destruct_execn {             | ... bool socket {}
        1 index ~_socketdead inpidsockets {    | ... bool socket {}
          pop pop ~error if
        } ~exec ifelse                         | --
      } currentdict ~indict
    ] ~lock
  ] bind userdict 3 -1 roll put

  |======================== fg_ ==================================
  | ~active fd-in fd-out fd-err | true/false
  |
  | Creates a child executing ~active, and blocks until done.
  | Returns true iff return status of process is 0
  |   aka, active returns true without throwing an error.
  |
  /fg_ {
    bg wait
  } bind def

  |======================== fg ==================================
  | ~active | --
  |
  | Creates a child executing ~active, and blocks until done.
  | Throws a NOSYSTEM error iff return status of process is not 0
  |   aka, active returns false or throws an error.
  |
  /fg {
    fds bg (fg) wait_
  } bind def

  | \[(exec) ... | --
  | Process image replaced by exec with string operands,
  |   after signalling parent to disconnect console.
  | Used only in child process.
  /_spawnsh {
    {
      console dup null eq ~pop {
        ~[
          ~[
            getpid unpid {~piddisconnect inpidsockets} ~PROCESSES ~indict
            ~restore
          ] ~lock
        ] send
      } ifelse

      closelist spawn
    } lock
  } bind def

  |======================== _sh_ ================================
  | \[ (exec) ... fd-in fd-out fd-err | \[ (exec) ... pid
  |
  | Execute (exec) with following operands in child process,
  |  using fd-in fd-out and fd-err as redirects.
  | Returns pid of child process running in bg.
  | -- without clear to mark
  |
  /_sh_ {
    {~_spawnsh fds bg waitdie die} 4 1 roll bg
  } bind def

  |======================== sh_bg ================================
  | \[ (exec) ... fd-in fd-out fd-err | pid
  |
  | Execute (exec) with following operands in child process,
  |  using fd-in fd-out and fd-err as redirects.
  | Returns pid of child process running in bg.
  |
  /sh_bg {
    _sh_ counttomark 1 add 1 roll cleartomark
  } bind def

  |======================== sh_bg ================================
  | \[ (exec) ... fd-in fd-out fd-err | pid (exec)
  |
  | Execute (exec) with following operands in child process,
  |  using fd-in fd-out and fd-err as redirects.
  | Returns pid of child process running in bg and name of executable,
  |  useful for wait variation.
  |
  /sh_bg_ {
    counttomark 1 sub index counttomark 1 add 1 roll | (exec) \[ (exec) ...
    sh_bg exch
  } bind def

  |======================== sh_ ================================
  | \[ (exec) ... fd-in fd-out fd-err | bool
  |
  | Execute (exec) with following operands in child process,
  |  using fd-in fd-out and fd-err as redirects.
  | Returns true if (exec) returns zero.
  |
  /sh_ {sh_bg wait} bind def


  |======================== sh_io ================================
  | \[ (exec) ... fd-in fd-out fd-err | --
  |
  | Execute (exec) with following operands in child process,
  |  using fd-in fd-out and fd-err as redirects.
  | Waits until process is done, and errors out if process fails.
  |
  /sh_io {sh_bg_ wait_} bind def

  |======================= sh ===================================
  | \[ (exec) ... | --
  | 
  | Execute (exec) with following operands as child process in
  |   the foreground (using the same in, out, and err fds as
  |   parent).
  | Blocks until child execution finished; if (exec) returns
  |  non-zero exec status, throws NOSYSTEM error.
  |
  /sh {fds sh_io} bind def

  |======================= sh_quiet ===================================
  | \[ (exec) ... | --
  |
  | Execute (exec) with following operands as child process in
  |   the foreground (using the same in, out, and err fds as
  |   parent).
  | Blocks until child execution finished; if (exec) returns
  |  non-zero exec status, throws NOSYSTEM error.
  |
  /sh_quiet {fds sh_io_quiet} bind def

  |======================= sh_io_quiet =================================
  | \[ (exec) ... fd-in fd-out fd-err | --
  |
  | Execute (exec) with following operands as child process in
  |   the foreground (redirecting to fd-in fd-out fd-err).
  | Blocks until child execution finished; if (exec) returns
  |  non-zero exec status, throws NOSYSTEM error.
  |
  /sh_io_quiet {sh_bg_ wait_quiet} bind def

  |====================== pipe ==================================
  | \[ ~active ... fd-in fd-out fd-err | bool
  |
  | Runs a pipe of child processes, with ~active1's output feeding
  |  into ~active2's input, and so forth.
  | active1's input is fd-in, activen's output is fd-out,
  |  and all active's error is fd-err.
  | Returns true if every sub-process returns a zero (which happens
  |  if active returns true or zero if its an exec)
  |
  /pipe {
    counttomark 1 add 3 roll closelist
    dup length list 0
    2 {null} repeat openlist
    /in /out_ /err /actives
    /pids /i
    /out /nin
  } {
    true
    actives length 0 ne {
      /in in copyfd def
      actives last {
        pipefd /out name /nin name
        actives i get in out err openlist nin ~bg_ enddict pids i put
        /i i 1 add def
        in close out close
        /in nin def
      } repeat

      /out out_ copyfd def
      actives i get in out err ~bg enddict pids i put
      in close out close

      pids {wait and} forall
    } if
  } localfunc bind def

  |============================== andp =============================
  | \[ ~active ... fd-in fd-out fd-err | pid
  |
  | Executes ~active1 to ~activen in sequence. Continues as
  |  long as ~activeX returns a zero status (short-circuits).
  | All children use fd-in as STDIN, fd-out as STDOUT and
  |  fd-err as STDERR.
  | The group returns a non-zero exit status if any child returns
  |  a non-zero exit status -- aka, fails on first child failure.
  |
  /andp {
    {
      {
        counttomark dup 1 eq ~pop {-1 roll} ifelse
        fds fg_ not {cleartomark false exit} if
        dup class /markclass eq {pop true exit} if
      } loop
    } 4 1 roll bg
    counttomark 1 add 1 roll cleartomark
  } bind def

  |============================== andp =============================
  | \[ ~active ... | --
  |
  | Executes ~active1 to ~activen in sequence. Continues as
  |  long as ~activeX returns a zero status (short-circuits).
  | All children use parent's STDIN, STDOUT and STDERR.
  | Blocks until children are done; if any child fails with
  |  non-zero exit status, processing stops and NOSYSTEM
  |  is thrown.
  |
  /andfg {
    fds andp (andfg) wait_
  } bind def

  |============================== andp =============================
  | \[ ~active ... fd-in fd-out fd-err | pid
  |
  | Executes ~active1 to ~activen in sequence. Continues as
  |  long as ~activeX returns a non-zero status (short-circuits).
  | All children use fd-in as STDIN, fd-out as STDOUT and
  |  fd-err as STDERR.
  | The group returns a zero exit status if any child returns
  |  a zero exit status -- aka, succeeds on first child success.
  |
  /orp {
    {
      {
        counttomark dup 1 eq ~pop {-1 roll} ifelse
        fds fg_ {cleartomark true exit} if
        dup class /markclass eq {pop false exit} if
      } loop
    } 4 1 roll bg
    counttomark 1 add 1 roll cleartomark
  } bind def

  |============================== andp =============================
  | \[ ~active ... | --
  |
  | Executes ~active1 to ~activen in sequence. Continues as
  |  long as ~activeX returns a non-zero status (short-circuits).
  | All children use parent's STDIN, STDOUT and STDERR.
  | Blocks until children are done; if all children fail
  |  with non-zero exit status, processing stops and NOSYSTEM
  |  is thrown. Returns with first successful child.
  |
  /orfg {
    fds orp (orfg) wait_
  } bind def

  |=============================== readtomark ===============================
  | (buffer) offset fd (char) | (buffer) offset fd true / (buffer) offset false
  |
  | Fills (buffer) from offset by reading fd until char is found.
  | Blocks until either found char, or reached end-of-file
  | Returns true if char is found; otherwise false if end-of-file found.
  | char is not inserted into buffer.
  | offset is updated to offset after characters inserted.
  |
  /readtomark {0 get openlist /buf /off /fd /char} {
    buf off buf length off sub getinterval | (sub-buf)
    fd char readtomarkfd {                 | (sub-sub-buf) fd
      pop                                  | (sub-sub-buf)
      length off add                       | new-off
      buf exch fd true                     | (buf) new-off fd true
    } {                                    | (sub-sub-buf)
      length off add                       | new-off
      buf exch false                       | (buf) new-off false
    } ifelse
  } localfunc bind def

  |=============================== readline ===============================
  | (buffer) offset fd | (buffer) offset fd true / (buffer) offset false
  |
  | same as readtomark, but char is set to newline.
  |
  /readline {
    (\n) readtomark
  } bind def

  |=============================== readlines ===============================
  | fd | [ (line)... ]
  |
  | read lines (not including newlines), and put in list
  |
  /readlines {
    [exch {(\n) 0 get readtomarkfd_nb not ~exit if} loop]
  } bind def

  |========================= get... =================================
  |======================= _getfromactive ==========================
  | ~active ~postfunc (name) | ...
  | active: -- | --
  | postfunc: pipe-rd | ...
  /_getfromactive {pipefd openlist /func /pfunc /nm /rd /wr} {
    ~[rd ~close /func find construct_exec true]
    STDIN wr STDERR ~bg ~enddict stopped

    wr close {rd close stop} if {
      nm ~wait_quiet enddict
      rd /pfunc find enddict 
    } stopped rd closeifopen ~stop if
  } caplocalfunc bind def

  |============================== getline ============================
  | ~active | (string)
  | active: -- | --
  | 
  | Executes active, and sets fd-out to a pipe. 
  | Reads first line (stripping out final newline) from pipe and closes pipe.
  | Blocks until active ends.
  |
  /getline {
    {(\n) 0 get readtomarkfd_nb ~close if} (getline) _getfromactive
  } bind def

  |============================== getlines ============================
  | ~active | [ (string)... ]
  | active: -- | --
  |
  | Executes active, and sets fd-out to a pipe. 
  | Reads all lines from pipe (stripping out final newlines).
  | Blocks until active ends.
  |
  /getlines {
    ~readlines (getlines) _getfromactive
  } bind def    

  |============================== getall ============================
  | ~active | (string)
  | active: -- | --
  |
  | Executes active, and sets fd-out to a pipe. 
  | Reads all lines from pipe and returns as a single string.
  | Blocks until active ends.
  |
  /getall {
    ~suckfd (getall) _getfromactive
  } bind def

  |============================= readstream ===========================
  | ~func | (stdout) (stderr) true/false
  | ~func: -- | --
  |
  | func is run as a seperate process, with its STDIN set to NULLR,
  |  its STDOUT and STDERR piped out. The output of the pipes are
  |   returned as strings.
  | Returns true iff ~func doesn't error out or abort.
  |
  /readstream {
    4 ~pipefd repeat
    3 {null} repeat
    openlist
    /func
    /rd /wr /erd /ewr /rdrd /rdwr /erdrd /erdwr
    /pid_eps /pid_out /pid_err
  } {
    {
      {/func find enddict true} NULLR wr ewr
      openlist rd erd rdrd rdwr erdrd erdwr
      bg_ /pid_eps name
      [wr ewr] ~close forall | rd erd rdrd rdwr erdrd erdwr

      {STDIN suckfd STDOUT exch writefd close true} erd erdwr STDERR
      openlist rd rdrd rdwr erdrd
      bg_ /pid_err name
      [erd erdwr] ~close forall | rd rdrd rdwr erdrd

      {STDIN suckfd STDOUT exch writefd close true} rd rdwr STDERR
      openlist erdrd rdrd bg_ /pid_out name
      [rd rdwr] ~close forall | rdrd erdrd
    } ~stopped aborted
    {erdrd suckfd toconsole abort} if
    {erdrd suckfd toconsole stop} if

    erdrd suckfd pid_err wait_quiet
    rdrd  suckfd pid_out wait_quiet
    exch         pid_eps wait       | (stdout) (stderr) bool
  } caplocalfunc bind def

|=============================== rmpath functions =====================

  | (dir) (file) norecur-bool | --
  /_rmpath {
    /norecur exch {{(rmfile) /DIR_NOTEMPTY makeerror}} {null mkact} ifelse def
    __rmpath
  } bind def

  | (dir) (file) | bool
  /isdir {
    stat not {(isdir) /FILE_NOSUCH makeerror} if
    openlist 18 2 roll cleartomark
    FILE_TYPE /DIRECTORY get eq
  } bind def

  | (dir) (file) <</norecur defined>> | --
  /__rmpath {
    2 copy isdir {
      2 copy finddir {
        norecur
        {1 index exch __rmpath} forall pop
      } if
    } if
    rmpath
  } bind def

  |========================== rmfile ========================
  | (dir) (file) | --
  |
  | Removes file (dir/file), or dir (dir/file) if empty.
  |
  /rmfile {
    true ~_rmpath /rmpath_ ~inlayer PROCESSES indict
  } bind def

  |=========================== rmdir ========================
  | (dir) (file) | --
  |
  | Removes (dir/file) directory recursively, removing all children
  |   first, then attempting to remove parent.
  |
  /rmdir {
    false ~_rmpath /rmpath_ ~inlayer PROCESSES indict
  } bind def

  |=========================== setwdirp ==========================
  | (dir) (subdir) | --
  |
  | sets current working directory to (dir/subdir).
  |
  /setwdirp {
    exch setwdir setwdir
  } bind def

  |======================== tosystem ===========================
  | (exec ...) | --
  |
  | Executes (exec ...) via bash. Throws NOSYSTEM if
  |  bash returns non-zero exit status.
  | Blocks until bash returns.
  | STDIN and STDOUT are redirected to /dev/null.
  | Lives in userdict.
  |
  /tosystem {
    {
      openlist PROGS /BASH get (-c) 4 -1 roll NULLR NULLW STDERR 
      sh_ not {(tosystem) /NOSYSTEM makeerror} if
    } PROCESSES indict
  } bind userdict 3 -1 roll put

  |====================== fromsystem ===========================
  | (exec ...) | (...)
  |
  | Executes (exec ...) via bash. Throws NOSYSTEM if
  |  bash returns non-zero exit status.
  | Blocks until bash returns.
  | STDIN is redirected to /dev/null, and STDOUT is
  |  read and the string returned.
  | Lives in userdict.
  |
  /fromsystem {
    ~pipefd {
      /wt name /rd name {
        openlist PROGS /BASH get (-c) 4 -1 roll NULLR wt STDERR 
        sh_ not {
          wt close rd close
          (fromsystem) /NOSYSTEM makeerror
        } if
        wt close rd suckfd
      } stopped {
        wt closeifopen rd closeifopen
        stop 
      } if
    } /fromsystem_ ~caplayer PROCESSES indict
  } bind userdict 3 -1 roll put

  |=========================== multiproc =======================
  |
  | ~active | ...
  |
  | call active in procthreads processes (default threads)
  | waits for all processes to end.
  | to send back info, use console ~[] send: NB, in main node in PROCESSES.
  |
  | procthreads determines the number of processes -- defaults to threads
  | procthreads_rank in PROCESSES is the rank number of the process
  |   0..procthreads-1
  |
  userdict /dm_type get /dvt ne {
    /procthreads ~threads def
    /multiproc {
      {
        /procthreads_live procthreads def
        /procthreads_pids procthreads list def
      } {
        {
          0 1 procthreads_live 1 sub {          | ~active rank
            {
              /procthreads_rank name end 
              ~stopped aborted
              console {
                {
                  PROCESSES /procthreads_live get 1 sub
                  PROCESSES /procthreads_live put
                  continue
                  restore
                } lock
              } send
              ~abort if ~stop if
              true
            } fds bg                            | ~active rank pid
            procthreads_pids 3 -1 roll put      | ~active
          } for pop                             |

          {
            ~halt enddict
            procthreads_live 0 eq ~exit if
          } loop
        
          procthreads_pids {(multiproc) ~wait_quiet unlock} forall
        } stopped {terminateall stop} if
      } ~incapsave lock
    } bind userdef

  |=========================== forallproc =======================
  |
  | start step last ~active | ...
  |
  | call active in n processes (default threads), procthreads at a time.
  | waits for all processes to end.
  | to send back info, use console ~[] send: NB, in main node in PROCESSES.
  |
  | procthreads determines the number of processes -- defaults to threads
  | procthreads_rank in PROCESSES is the rank number of the process
  |   0..procthreads-1
  |
    /forproc {
      /procthreads_func name
      /procthreads_last name
      /procthreads_step name
      /procthreads_first name

      /procthreads_live 0 def
      /procthreads_max procthreads def
      /procthreads_n 0 procthreads_first procthreads_step procthreads_last {
        pop 1 add
      } for def

      {/procthreads_pids procthreads_n list def} {
        {
          /procthreads_i 0 def
          procthreads_first procthreads_step procthreads_last {/procthreads_j name
            {
              procthreads_live procthreads_max lt ~exit if
              ~halt enddict
            } loop

            /procthreads_live procthreads_live 1 add def
            {
              procthreads_j /procthreads_func find ~enddict ~stopped aborted
              console {
                {
                  {/procthreads_live procthreads_live 1 sub def} PROCESSES indict
                  continue
                  restore
                } lock
              } send
              ~abort if ~stop if
              true
            } fds bg                            | pid
            procthreads_pids procthreads_i put  |
            /procthreads_i procthreads_i 1 add def
          } for

          {
            procthreads_live 0 eq ~exit if
            ~halt enddict
          } loop

          procthreads_pids {(forproc) ~wait_quiet unlock} forall
        } stopped {terminateall stop} if
      } ~incapsave lock
    } bind userdef
  } if | not in dvt
} moduledef
