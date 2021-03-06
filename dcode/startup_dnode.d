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

|======================== startup_dnode.d ================================

| Contains procedures for:
|  - inspection of objects
|  - object/text interconversion
|  - transcription of objects
|  - file <=> VM interchange
|  - module support
|  - emulators of dvt operators

/dm_type /dnode def

save /startup_common_save name 
/startup_common_buf vmstatus sub 10 div /b array def
startup_common_save capsave {
  getstartupdir (startup_common.d) startup_common_buf readfile mkact exec
  getstartupdir (startup_libs.d)   startup_common_buf readfile mkact exec
} stopped startup_common_save restore {
  1024 /b array 0 
  (Unable to load: ) fax
  getstartupdir fax (startup_common.d\n) fax
  0 exch getinterval toconsole
  stop
} if

| pid (name) port (source) code
/error_ops_length 5 def

| -- | pid (name) port true
/_makeerror {
  getpid unpid
  myname
  getmyport
  true
} bind def

|============================= userdict =================================

|----------------- myname
| -- | (hostname)
| just allocate it on startup
/myname getmyname def

|---------------- socketdead
| socket | --
| default handler just throws away the socket and signals an error

| error-pending socket | -- <<error thrown>>
/socketdead {{
  false exch
  {
    currentdict /socket known {
      dup socket eq {
        {
          (On ) fax myname fax ( port ) fax * getmyport * number (: ) fax
          (** rthreads socket dead) fax
        } warning
        /socket -1 def kill
        exch pop true exch
      } if
    } if
  } userdict /mpidata known {mpidata indict} ~pop ifelse

  exch ~pop {
    disconnect
    _makeerror pop (socketdead) ERRORS /DEAD_SOCKET get showerror
  } ifelse 
  ~error ~abort ifelse
} lock} bind def

|============================= pawns =========================
|
/ENABLE_RTHREADS {
  |--------------------------- mpidata
  | dictionary for mpi pawns
  |
  /mpidata 200 dict dup begin | [
    | cpawn: keyboard owning pawn by rank, -1 for node
    /cpawn -1 def
    | pawns: number of currently active pawns
    /pawns 0 def
    | memsize: array for vmresize on pawns
    /memsize <x 100000 200 200 1000 200> def

    | winmade: true iff ThePawns window has been built
    /winmade false def
    | pbuf: locale buffer for text writing into ThePawn
    /pbuf 80 /b array def
    | mpiinfo: list describing pawns
    |  [n1 dict1 n2 dict2...], where n is the number of pawns
    |  described by dictn, and dictn is a is a dictionary that maps
    |  onto the MPI_Info for those pawns. See MPI_Info 
    |  and MPI_Comm_spawn_multiple for a description of 
    |  pairs in MPI_Info (for example, /host=(host.domain))
    |  The values should always be strings.
    /mpiinfo [] def
    | precall: a procedure called before creating the threads,
    |  for example: (mpdboot) tosystem to start the mpd servers
    /precall {} def
    | postcall: a procedure called after destroying mpi threads,
    |  for example: (mpdallexit) tosystem to stop the mpd servers
    /postcall {} def

    | updatet: updates ThePawns and prints the current keyboard owner.
    /updatet {
      winmade {{currentdict begin drawwindow} stopped pop} {
        pbuf 0 (pawn ) fax * cpawn * number (:\n) fax 0 exch getinterval
        toconsole_base
      } ifelse
    } bind def

    | pawn#/* checkbusy --
    | if pawn number (or all(*)) are busy, print error message and
    |  stop
    /checkbusy {
      dup * eq {
        pop
        false pawnbusy {or} forall {(Pawn is busy!\n) toconsole_base stop} if
      } {
        pawnbusy exch get {(Pawn is busy!\n) toconsole_base stop} if
      } ifelse
    } bind def

    | pawn#/* makebusy --
    | sets pawn# or all(*) busy.
    /makebusy {
      dup checkbusy
      dup * eq {pop 0 1 pawns 1 sub {true pawnbusy 3 -1 roll put} for} {
        true pawnbusy 3 -1 roll put
      } ifelse
      updatet
    } bind def

    | pawn#/* makeready --
    | sets pawn# or all(*) not busy
    /makeready {
      dup * eq {pop 0 1 pawns 1 sub {false pawnbusy 3 -1 roll put} for} {
        false pawnbusy 3 -1 roll put
      } ifelse
      updatet
    } bind def

    | save rank (name) sethostname --
    | sets the hostname for ThePawns for a given rank
    | and restores the save. Intended for use by a call from pawn.
    /sethostname {
      2 index capsave 
      255 /b array 0 * 4 index * number (: ) fax 3 -1 roll fax
      0 exch getinterval
      nodenames 3 -1 roll 1 add put
      end
      restore
    } bind def

    | save pawn# makeready_dpawn --
    | call to makeready from pawn
    /makeready_dpawn {
      ~makeready stopped end {stop} if restore
    } bind def

    | save pawn# makebusy_dpawn --
    | call to makebusy from pawn
    /makebusy_dpawn {
      ~makebusy stopped end {stop} if restore
    } bind def

    | pawn#/-1 setcurrent --
    | sets the keyboard owner and updates ThePawns
    /setcurrent {/cpawn name
      updatet
    } bind def

    | pawn#/-1 emacs_next_talk --
    | sets the keyboard owner to the next pawn (or the dnode)
    | wrapping around. Then ends mpidata.
    /emacs_next_talk {
      cpawn dup * eq {pop pawns 1 sub} if
      1 add dup pawns eq {pop -1} if
      dup cpawn ne ~setcurrent ~pop ifelse
      end
    } bind def

    | pawn#/-1 emacs_last_talk --
    | sets the keyboard owner to the previous pawn (or the dnode)
    | wrapping around. Then ends mpidata.
    /emacs_last_talk {
      cpawn dup * eq {pop 0} if
      dup 0 lt {pop pawns} if 1 sub
      dup cpawn ne ~setcurrent ~pop ifelse
      end
    } bind def

    | -- kill --
    | kills all mpi threads, makes postcall,
    | destroys ThePawns window, resets keyboard owner
    | and restores all data used by mpidate procedures
    /kill {
      {null makerthreads postcall} stopped
      {winmade ~deletewin if} stopped pop
      currentdict /save_ known {save_ restore} if
      -1 setcurrent
      /pawns 0 def
      ~stop if
    } bind def 

    |=========== mpidata Xwindows ===================
    Xwindows_ {
      /NORMALFONT {fontdict /NORMALFONT get} bind def
      /BOLDFONT {fontdict /BOLDFONT get} bind def
      
      /windowsize {
        { [ 3 1 roll | ]
          winmade not {stop} if
          wH ne exch wW ne or {
            wid wW wH resizewindow
          } if 
        } stopped end {cleartomark stop} if pop
      } bind def

      /drawwindow {
        { [ | ]
          winmade not {stop} if
          wid woutline BLACK drawline
          0 1 mpidata /pawns get 1 add 1 sub {drawnode} for
        } stopped end {cleartomark stop} if pop
      } bind def

      /drawnode {/kpawn name
        wid noderects kpawn get
        kpawn 0 eq ~BG {
          pawnbusy kpawn 1 sub get ~HBG ~BG ifelse
        } ifelse fillrectangle
        
        wid 
        nodelocs kpawn get exec      
        nodenames kpawn get
        cpawn dup * eq kpawn 0 ne and exch kpawn 1 sub eq or 
        ~HIGHTEXT ~NORMALTEXT ifelse
        drawtext pop pop pop
      } bind def

      /actions 3 dict dup begin | [
        /click {kpawn 1 sub setcurrent} bind def
        /cancel {* makeready} bind def
        /group {* setcurrent} bind def |]
      end def

      /mouseclick {
        { [ 4 1 roll | ]
          winmade not {stop} if
          /mS name /mY name /mX name
          /kpawn mY 1 sub 13 div def
          mS /default actions commonmousedict mouseaction pop
        } stopped end {cleartomark stop} if pop
      } bind def

      /newwin {
        {[ | ]
          Xwindows not {stop} if

          /BLACK <d 0 0 0>       mapcolor def
          /GRAY  <d 0.2 0.2 0.2> mapcolor def
          /BLUE  <d 0 0 1>       mapcolor def
          /RED   <d 1 0 0>       mapcolor def
          /LBLUE <d 0.1 0.1 0.9> mapcolor def

          /BG <d 235 243 248>  dup length /d array copy 255 div mapcolor def
          /HBG <d 166 219 160> dup length /d array copy 255 div mapcolor def

          /NORMALTEXT ~NORMALFONT [null BLACK -1 0] makefont def
          /HIGHTEXT ~BOLDFONT [null BLUE -1 0] makefont def
    
          screensize /scrH name /scrW name

          /wW 200 2 add def
          /wH mpidata /pawns get 1 add 13 mul 2 add def

          /woutline [ 0 0 wW 1 sub 0 wW 1 sub wH 1 sub 0 wH 1 sub 0 0 ] def

          /noderects [
            /x 1 def
            1 13 mpidata /pawns get 1 add 1 sub 13 mul 1 add { /y name
              /r 4 /w array def
              x r 0 put  y r 1 put wW 2 sub r 2 put 13 r 3 put
              r
            } for
          ] def

          /nodelocs [
            /x 5 def
            12 13 mpidata /pawns get 1 add 1 sub 13 mul 12 add { /y name
              ~[x y 6 sub]
            } for
          ] def

          /nodenames [
            255 /b array 0 dvtnodeid fax (:) fax * getmyport * number 
            0 exch getinterval

            /i 0 def
            0 2 mpiinfo length 1 sub {/j name
              mpiinfo j get {
                255 /b array 0 * i * number (: ) fax
                mpiinfo j 1 add get dup /host known {/host get fax} {pop} ifelse
                0 exch getinterval
                /i i 1 add def
              } repeat
            } for
          ] def

          /wid [scrW wW sub 10 sub scrH wH sub 10 sub wW wH ]
            (ThePawns) (Pawns) makewindow 
          def
          /winnamebuf 30 /b array def
          /winname 
            winnamebuf 0 (/w) fax * wid * number 0 exch getinterval mkact exec 
          def
          currentdict userdict winname put
          wid true mapwindow
          /winmade true def
          BG pop
        } stopped {cleartomark stop} if pop
      } bind def

      /deletewin {
        {[ |]
          /winmade false def
          wid deletewindow
        } stopped {cleartomark stop} if pop
      } bind def

      | -- delete_window --
      | procedure call to deletewin.
      /delete_window {{deletewin} stopped end {stop} if} bind def
    } if

    | [[n /key val ...]...] rthreads_multi_ --
    | creates the mpiinfo from the nested lists,
    | where each n is the number of pawns, the key value pairs
    |  are put into the mpiinfo dictionary.
    /rthreads_multi_ {
      [exch {
        dup 0 get 1 index length 1 sub 2 div dict dup begin | [n /k v] n dict 
        3 -1 roll 1 1 index length 1 sub getinterval  | n dict [/k v..]
        0 2 2 index length 1 sub { | n dict [/k v..] i
          2 copy get 3 1 roll      | n dict /k [/k v..] i
          2 copy 1 add get         | n dict /k [/k v..] i v 
          3 1 roll pop 3 1 roll    | n dict [/k v..] /k v 
          def                      | n dict [/k v..]
        } for pop end
      } forall] /mpiinfo name
    } bind def

    | [n1 (host1) n2 (host2)... ] rthreads_mhosts_ --
    | creates an mpiinfo, where the dictionaries only consist
    | of the pair /host=(host)
    /rthreads_mhosts_ {
      [exch
        0 2 2 index length 1 sub {    |[.. [n1 (host1) ..] i             |]
          [                           |[.. [n1 (host1) ..] i [           |]]
            2 index 2 index get       |[.. [n1 (host1) ..] i [ni         |]]
            3 index 3 index 1 add get |[.. [n1 (host1) ..] i [ni (hosti) |]]
            /host exch                |[.. [n1 (host1) ..] i [ni /host (hosti) |]]
          ] 3 1 roll pop              |[.. [ni /host (hosti)] [n1 (host1)...] |]
        } for pop                     |[[n1 /host (host1)] ... ] 
      ] rthreads_multi_
    } bind def

    | [(host1) ...] rthreads_hosts --
    | creates an mpiinfo, where the dictionaries only consist 
    | of the pair /host=(host), and where the number for each host is 1
    /rthreads_hosts_ {
      [exch {
        [ 1 /host 4 -1 roll ]
      } forall] rthreads_multi_
    } bind def

    | n rthreads_single_ --
    | creates the mpiinfo dictionaries, where there is only
    | one empty dictionary
    /rthreads_single_ {
      [exch 0 dict] /mpiinfo name
    } bind def

    | ~info-dict-setup rthreads_start --
    | Starts up the pawns by setting up mpidata,
    | calling ~info-dict-setup to create the pawn-number, mpiinfo 
    | dictionary pairs, vmresize the pawns and create ThePawns window,
    | and read in startup file on the pawns.
    | All of this is put in a save to be restored by kill
    /rthreads_start {
      {
        pawns 0 gt {(** Rthreads already exist!\n) toconsole_base halt} if
        save /save_ name {
          exec
          0 0 2 mpiinfo length 1 sub {mpiinfo exch get add} for /pawns name
          /pawnbusy [pawns {true} repeat] def
          precall
          mpiinfo makerthreads /socket name
          -1 setcurrent
          currentdict /newwin known ~newwin if
          
          socket ~[memsize {exch pop vmresize} ~lock] send
          socket {
            {
              exch not {abort} if
              /fbuf vmstatus sub 10 div /b array def
              ~[~[
                mpirank getmyfqdn ~mpidata ~begin ~sethostname
              ] ~lock]
              1 index capsave
              getstartupdir (startup_dpawn.d) fbuf readfile mkact exec
              rsend dnoderespond
              restore
            } lock
          } send
          save_ capsave
        } stopped {kill stop} if
      } lock
    } bind def 

    | save pawn# notify --
    | intended to be called by dpawn to notify the node
    | that it has ended it's part of a job previously allocated
    | to it. nwait should be set to the total number of jobs.
    | sets the pawn ready for more jobs.
    /notify {
      makeready
      /nwait nwait 1 sub def
      continue
      end
      restore
    } bind def |]
  end def |========= end of mpi data ================

  |============================================= pawn functions
  |
  | Xint rthread_ops --
  | set the size of the operand stack for pawns
  /rthread_ops {
    mpidata /memsize get 0 put
  } bind def

  | Xint rthreads_dicts --
  | set the size of the dictionary stack for pawns
  /rthreads_dicts {
    mpidata /memsize get 1 put
  } bind def

  | Xint rthreads_execs --
  | set the size of the execution stack for pawns
  /rthreads_execs {
    mpidata /memsize get 2 put
  } bind def

  | Xint rthreads_memory --
  | set the memory size in MB for the pawns
  /rthreads_memory {
    mpidata /memsize get 3 put
  } bind def

  | Xint rthreads_user --
  | sets the size of the user dictionary for pawns
  /rthreads_user {
    mpidata /memsize get 4 put
  } bind def

  | [[n /key val ..]...] rthreads_multi --
  | creates pawns, with each group of n pawns defined by the 
  | associate key-value pairs.
  /rthreads_multi {
    mpidata begin {
      ~rthreads_multi_ rthreads_start
    } stopped end {stop} if
  } bind def

  | n rthreads_single --
  | Creates pawns (n of them) with an empty MPI_Info dictionary,
  | in most implementations on the local machine
  /rthreads_single {
    mpidata begin {
      ~rthreads_single_ rthreads_start
    } stopped end {stop} if
  } bind def

  | [n1 (host1) n2 (host2)...] rthreads_mhosts
  | Creates pawns on hosts, n on hostn.
  /rthreads_mhosts {
    mpidata begin {
      ~rthreads_mhosts_ rthreads_start
    } stopped end {stop} if
  } bind def

  | [(host1) (host2)...] rthreads_hosts
  | Creates pawns on hosts, 1 on hostn.
  /rthreads_hosts {
    mpidata begin {
      ~rthreads_hosts_ rthreads_start
    } stopped end {stop} if
  } bind def
} if_compile

| active rthreads_exec ??
| executes active on dnode if mpi hasn't been setup
|   or no rthread active (cpawn == -1)
| if all rthreads active (cpawn == *), executes on all rthreads,
|  otherwise executes on the chosen rthread (cpawn)
/rthreads_exec ~unlock bind def 
| Overridden immediately if ENABLE_RTHREADS

/ENABLE_RTHREADS {
  /rthreads_exec {
    mpidata /cpawn get dup -1 eq {pop unlock} {
      mpidata begin {
        dup makebusy
        save exch ~[~[5 -1 roll ~dnodereceive] ~lock] rsend restore
      } stopped end {stop} if
    } ifelse
  } bind def

  | -- rthreads_stop --
  | kill pawns, destroy ThePawns window, recover allocated
  | memory
  /rthreads_stop {
    mpidata begin {
      checkrthreads not {(** Rthreads not running!\n) toconsole_base} if
      pawns 0 eq {(** No rthreads to stop! Halting\n) toconsole_base halt} if
      * checkbusy
      kill
    } stopped end {stop} if
  } bind def

  | -- rthreads_r --
  | Set all pawns as not busy.
  /rthreads_r {
    * mpidata begin ~makeready stopped end ~stop if
  } bind def

  | -- rthread_kill --
  | destroy pawns (and windows, etc) without checking the pawns' status.
  /rthreads_kill {
    mpidata begin ~kill stopped pop end
  } bind def

  | pawn#/-1 | --
  | set keyboard owner pawn (or dnode if -1).
  /rthreads_t {
    mpidata begin {
      dup cpawn ne ~setcurrent ~pop ifelse
    } stopped end ~stop if
  } bind def

  | ~func-maker: <<i | {}/null>> execpawns --
  | execute a job on each pawn and wait until
  |   they've all been finished.
  | func-maker is executed for each pawn, with the intended
  |   recipients rank on the stack, and the job for that
  |   rank shall be returned.
  |   If func-maker returns a null, that pawn is not given
  |   a job.
  | Waits in a halt-loop until all pawns have signalled that
  |  their jobs have been completed.

  /execpawns {false _execpawns} bind def

  /execall {
    dup execpawns
    save * 3 -1 roll exec dup null eq ~pop {
      1 index capsave exec
    } ifelse
    restore
  } bind def

  /execkickpawns {false _execkickpawns} bind def

  /_execpawns {~wait_dnode execpawns_} bind def

  /_execkickpawns {~recv_kick_dnode execpawns_} bind def

  | ~func-maker: <<i | {}/null>> broadcast kicker-func execpawns --
  /execpawns_ {
    {
      mpidata begin {
        /kicker_func name
        save
        /nwait 0 def
        exch {
          * execpawns_rsend {/nwait pawns def} if
        } {
          0 1 pawns 1 sub {
            execpawns_rsend {/nwait nwait 1 add def} if
          } for
        } ifelse
        restore
        pop

        {
          nwait 0 eq ~exit if
          halt
        } loop
      } stopped end ~stop if
    } lock
  } bind def

  | ~func-maker save pawn# | ~func-maker save sent-bool
  /execpawns_rsend {
    dup 3 index exec | ~func-maker save pawn# ~active
    dup null eq {pop pop false} {
      1 index makebusy
      ~[exch destruct_exec /kicker_func find] rsend
      true
    } ifelse
  } bind def

  | ~job sexecpawns --
  | a Simplified version of execpawns. It executes ~job on
  | all pawns, and waits for them to be done.
  /sexecpawns {~_execpawns sexecpawns_} bind def

  /sexecall {dup exec sexecpawns} bind def

  | ~job ~execpawns-version sexecpawns_ --
  /sexecpawns_ {
    exch mpidata /cfunc put
    {pop /cfunc find} true 3 -1 roll exec
  } bind def

  | -- | --
  /kickpawns {
    ~null ~_execkickpawns sexecpawns_
  } bind def

  | /modulename module_send --
  | sends a module to all pawns. Pawns shall recieve the modules
  | and reset the save box for the module to a local value.
  | I.e., it will active as if the module had been created locally
  | as if by a module _module pair.
  /module_send {
    mpidata begin /module_send_ layer {
      ~[userdict 3 -1 roll get ~recv_module] /module_cmd name
      {pop /module_cmd find} execpawns
    } stopped /module_send_ _layer end ~stop if
  } bind def


  |=================== names_send =====================
  | [/name... ] | --
  |
  | copies each name to the pawns, transcribing composite objects
  |  at the receiving end.
  |
  /names_send {
    save openlist 3 -1 roll {
      {~[exch dup find {recv_transcribe def} ~exec] sexecpawns} forall
    } stopped cleartomark restore ~stop if
  } bind def

  |-------------------------- inlayerall
  | ~active /layer_name | ...
  |
  | Wraps up /layer_name layer ~active stopped /layer_name _layer ~stop if
  |  which is a common usage pattern for layers.
  | ... is whatever active leaves behind.
  |
  | This version also handles pawns -- it starts a layer on the pawns,
  |   halts the pawns, does an inlayer with the active object
  |   and then restarts the pawns to close the layer.
  |
  | {} /name | --
  /inlayerall {
    {
      ~[{kickdnode ~stop if} 3 -1 roll ~inlayer] sexecpawns
      exec kickpawns
    } 1 index ~inlayer stopped {~stop sexecall} if
  } bind def

  |-------------------------- restoreall
  | /save_name | ...
  |
  | restores the /savename on dnode & pawns
  |
  /restoreall {
    save ~[2 index mkact ~restore] sexecpawns restore
    find restore
  } bind def

  |==================================== end of pawn functions
} if_compile

|============================= command line interface for dvt =====

| dvt... expect to be locked when called

|  ~exec save knode true-if-send-to-active-pawn | ...
/dvtreceive_base {
  {~rthreads_exec} {~unlock} ifelse 
  openlist 4 -2 roll exch dup capsave null ~stopped push
  | ... stop null save knode / ... stop null knode
  exch dup class /nullclass eq ~pop {restore exch pop} ifelse
  | ... stop knode
  exch pop | ... knode
  dvtrespond
} bind def

| knode | ...
/dvtrespond {
  {console ~[3 -1 roll {dvtsup begin setready_dnode} ~exec] send} insave
} bind def

| (line) | {line}
/dvttokenize {
  ~[{exch token not {pop exit} if} loop]
} bind def

| save (line) knode | ...
/dvtreceive {
  exch mkact 3 1 roll true dvtreceive_base
|  exch dvttokenize 3 1 roll true dvtreceive_base
} bind def

| save (line) knode | ...
/dvtreceive_np {
  exch mkact 3 1 roll false dvtreceive_base
|  exch dvttokenize 3 1 roll false dvtreceive_base
} bind def

| save (line) knode | ...
/dvtsystem {
  ~tosystem 4 -1 roll 3 -1 roll true dvtreceive_base
} bind def

| save (line) knode | --
/linebuf 8192 /b array def
/dvtexecsystem {
  exch dvttokenize 3 1 roll {
    linebuf 0 3 -1 roll exec 0 exch getinterval tosystem
  } 3 1 roll true dvtreceive_base
} bind def

|----------------------- color loader and setup
| [/color_name ...] | -- <<redefines toconsole>>
/loadcolor {
  userdict /COLOR known not {getstartupdir (color.d) fromfiles} if
} bind def

/make_toconsole {
  loadcolor
  COLOR /make_toconsole get exec
  whoami
} bind def

| (base-dir) (file) | --
/convert_from_32 {
  save /cstate name {
    /filename name
    /dirname name
    
    /boxfile
      filename length 4 add /b array 0 filename fax (.box) fax pop
    def
    /olddirname
      dirname length 5 add /b array 0 dirname fax (/old/) fax pop
    def
    /newdirname
      dirname length 1 add /b array 0 dirname fax (/) fax pop
    def

    olddirname boxfile readf32 newdirname boxfile writeboxfile
  } stopped cstate restore {stop} if
} bind def

| (base-dir) | --
/convertdir_from_32 {
  save /cdstate name {
    dup length 1 add /b array 0 3 -1 roll fax (/) fax pop /basedirname name
    basedirname length 4 add /b array 0 basedirname fax (old/) fax pop
    /oldbasedirname name
    
    oldbasedirname findfiles {
      0 get (.box$) regex not {pop} {pop /basefilename name pop pop
        (Converting: ) toconsole
        basedirname toconsole basefilename toconsole (\n) toconsole
        basedirname basefilename convert_from_32
        (Done\n) toconsole
      } ifelse
    } forall
  } stopped cdstate restore {stop} if
} bind def

|------------------------------------------------ whoami
| prints name and port
| for use with make_toconsole to identify who looks like who
| -- | -- <<printout>>
|
/whoami ~[
  1024 /b array {
    0 
    myname fax
    (:) fax
    * getmyport * number
    (\n) fax
    0 exch getinterval toconsole_base
  } ~exec
] bind def

Xwindows_ {
  /take_input_focus {
    {[
      console dup null eq {pop} {
        {Xwindows {userdict begin take_input_focus} if restore} send
      } ifelse |]
    } stopped cleartomark
    end
  } bind def

  /delete_window {end} bind def | default delete action is nothing.
} if

(Starting...\n) toconsole
{
  /reqfiles [
    (matrix.d)
    (processes.d)
  ] def
  /optfiles [
    /ENABLE_PETSC {
      {getstartupdir (petsc.d)}
    } if_compile
    {getconfdir (dnode.d)} 
    {gethomedir (.dnode)}
  ] def
} {
  reqfiles ~loadstartup  forall
  optfiles {exec loadopt} forall
} incapsave
