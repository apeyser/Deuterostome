/ops module 1000 dict dup begin 

/mksmap {/errs name /cmds name /parents name
  2 dict dup begin |[
    /parents parents def
    /all 2 dict dup begin |[
      /errors errs def
      /commands cmds def |]
    end def |]
  end
} bind def


/buffer 100 1024 mul /b array def
/buffern 0 def

/__ {
  buffer buffern * 4 -1 roll text /buffern name pop
} def

/nl {(\n)__} def

/bifdef {/ifdef name ifdef /all ne {(#ifdef )__ ifdef __ nl} if} bind def
/eifdef {ifdef /all ne {(#endif //)__ ifdef __ nl} if} bind def
/recur {/rcmd name
  dup /parents known {
    dup /parents get {find rcmd} forall
  } if
} bind def

/doall {/doit name /ntype name
  mkact recur
  {1 index /parents eq {pop pop} {
    exch bifdef
    dup ntype known not {pop} {
      ntype get {doit} forall
    } ifelse
    eifdef
  } ifelse} forall
} bind def
    
/docommands {
  /docommands /commands 
    {/cmd name (  \(B*\) ")__ cmd __ (", \(B*\) op_)__ cmd __ (,)__ nl} 
  doall
} bind def

/doerrname {
  /doerrname /errors {pop (  )__ __ (,)__ nl} doall
} bind def

/doerrstr {
  /doerrstr /errors {exch pop (  \(B*\) ")__ __ (",)__ nl} doall
} bind def
        

/all {
  /group name /file name /path name
  ops group get /gdict name

  (// Automatically produced from src/codegen/ops.d)__ nl
  (// DO NOT EDIT HERE!)__ nl nl

  (B* _sysop[] = )__ nl
  ({)__ nl
  gdict docommands
  (  \(B*\) "]", \(B*\) op_closelist,)__ nl
  (  \(B*\) "", \(B*\) NULL)__ nl
  (};)__ nl nl

  (P _syserrc[] = {)__ nl
  gdict doerrname
  (  0L)__ nl
  (};)__ nl nl

  (B* _syserrm[] = {)__ nl
  gdict doerrstr
  (  \(B*\) NULL)__ nl
  (};)__ nl

  buffer 0 buffern getinterval path file writefile
} bind def

/common 2 dict dup begin |[
  /all 2 dict dup begin |[
    /commands [
      /error /errormessage /abort
      /toconsole 
      /pop /exch /dup /copy /index /roll /clear
      /count /cleartomark /counttomark
      /currentdict
      /dict /cleardict /array /list
      /used /length 
      /begin /end /def /name 
      /find /get /put /known
      /getinterval /countdictstack /dictstack
      /save /capsave /restore /vmstatus
      /bind /null
      /start /exec
      /if /ifelse /for /repeat /loop /forall /exit
      /stop /stopped
      /countexecstack /execstack
      /checkFPU /neg /abs /thearc /add /mod
      /sub /mul /div /sqrt /exp /ln /lg /pwr /cos
      /sin /tan /atan /floor /ceil /acos
      /eq /ne /ge /gt /le /lt /and /or /xor
      /bitshift /class /type /readonly
      /active /tilde /mkread /mkact /mkpass
      /ctype /parcel /text /number
      /token /search /anchorsearch 
      /gettime /localtime /getwdir /setwdir
      /readfile /writefile /findfiles /findfile
      /tosystem /fromsystem /transcribe
      /fax /merge /nextobject
      /interpolate /integrateOH /extrema 
      /solvetridiag /integrateOHv /tile /ramp
      /extract /dilute /ran1 /solve_bandmat
      /complexFFT /realFFT /sineFFT 
      /decompLU /backsubLU /integrateRS /bandLU
      /bandBS /invertLU /matmul /mattranspose
      /dilute_add /matvecmul
      /getstartupdir /gethomedir
    ] def
    /errors 100 dict dup begin |[
      /TIMER (** Timeout) def
      /CORR_OBJ (** Corrupted object) def
      /VM_OVF (** VM overflow) def
      /OPDS_OVF (** Operand stack overflow) def
      /EXECS_OVF (** Execution stack overflow) def
      /DICTS_OVF (** Dictionary stack overflow) def
      /OPDS_UNF (** Operand stack underflow) def
      /EXECS_UNF (** Execution stack undeflow) def
      /DICTS_UNF (** Dictionary stack undeflow) def
      /INV_EXT (** Invalid exit) def
      /INV_STOP (** Invalid stop) def
      /EXECS_COR (** Excution stack corrupted) def
      /INV_REST (** Stack holds discardable object) def
      /BAD_TOK (** Bad token) def
      /BAD_ASC (** Bad ASCII character) def
      /ARR_CLO (** Unmatched array closure) def
      /CLA_ARR (** Illegal class in array) def
      /PRO_CLO (** Unmatched procedure closure) def
      /OPD_CLA (** Operand class) def
      /OPD_TYP (** Operand type) def
      /OPD_ERR (** Operand class or type) def
      /RNG_CHK (** Range check) def
      /OPD_ATR (** Operand attribute) def
      /UNDF    (** Undefined name) def
      /DICT_ATR (** Dictionary attribute) def
      /DICT_OVF (** Dictionary overflow) def
      /DICT_USED (** Dictionary used) def
      /UNDF_VAL (** Undefined value) def
      /DIR_NOSUCH (** No such directory) def
      /BADBOX (** File does not contain a box object) def
      /BAD_MSG (** Bad message received on network) def
      /NOSYSTEM (** 'System' call to OS failed) def
      /INV_MSG (** Invalid message format) def
      /BAD_FMT (** Box not in native format) def
      /MEM_OVF (** Memory exhausted) def
      /BAD_ARR (** dmnuminc debug error) def
      /CLOCK_ERR (** Error accessing clock) def
      /LONG_OVF (** 64 bit integer overflow on load into 32 bit machine) def|]
    end def |]
  end def |]
end def

/regex 2 dict dup begin |[
  /DM_ENABLE_REGEX 2 dict dup begin |[
    /commands [/regex /regexi] def
    /errors 100 dict dup begin |[
      /REGEX_BADPAT (Regex Error: Invalid regular expression) def
      /REGEX_ECOLLATE (Regex Error: Invalid collating element) def
      /REGEX_ECTYPE (Regex Error: Invalid character class) def
      /REGEX_EESCAPE (Regex Error: `\' applied to unescapable character) def
      /REGEX_ESUBREG (Regex Error: invalid backreference number) def
      /REGEX_EBRACK (Regex Error: brackets `[]' not balanced) def
      /REGEX_EPAREN (Regex Error: paranthesis `\(\)' not balanced) def
      /REGEX_EBRACE (Regex Error: braces `{}' not balanced) def
      /REGEX_BADBR  (Regex Error: invalid repetition count\(s\) in `{}') def
      /REGEX_ERANGE (Regex Error: invalid character range in `[]') def
      /REGEX_ESPACE (Regex Error: ran out of memory) def
      /REGEX_BADRPT (Regex Error: `?', `*', or `+' operand invalid) def
      /REGEX_UNKNOWN (Regex Error: Unknown error) def |]
    end def |]
  end def |]
end def

/quitable 1 dict dup begin |[
  /all 1 dict dup begin |[
    /commands [/quit] def |]
  end def |]
end def

/net 1 dict dup begin |[
  /all 2 dict dup begin |[
    /commands [
      /connect /disconnect /send /getsocket
      /getmyname /getmyfqdn 
    ] def
    /error 2 dict dup begin |[
      /DEAD_SOCKET (Dead source connection) def
      /LOST_CONN (** Lost connection) def |]
    end def |]
  end def |]
end def

/x 1 dict dup begin |[
  /all 2 dict dup begin |[
    /commands [
      /Xwindows /Xdisplayname
      /screensize /makewindow
      /deletewindow /mapwindow /resizewindow
      /Xsync /mapcolor /drawline
      /drawsymbols /drawtext
      /makewindowtop 
      /Xauth /Xauthrev /Xauthgen /Xauthset
    ] def
    /errors 8 dict dup begin |[
      /NO_XWINDOWS(** X windows unavailable) def
      /X_ERR (** Error in X windows) def
      /X_BADFONT (** Bad X windows font) def
      /X_BADHOST (** Cannot connect to X server) def
      /X_SEC_MISS (** X Security Extension missing) def
      /X_SEC_GEN (** X Security Extension unable to generate) def
      /X_SEC_REV (** X Security Extension unable to revoke) def
      /X_SEC_LIB (** X Security library missing) def |]
    end def |]
  end def |]
end def

/node 2 dict dup begin |[
  /all 2 dict dup begin |[
    /commands [
      /lock /serialize /threads /makethreads 
      /tostderr 
      /halt /continue 
      /vmresize 
      /getplugindir
    ] def
    /errors 100 dict dup begin |[
      /LIB_LOAD (** Unable to load dynamically linked library) def
      /LIB_EXPORT (** Unable to find object in shared library) def
      /LIB_LINK (** Library has not been loaded) def
      /LIB_ADD (** Unable to add operator to library dictionary) def
      /LIB_LOADED (** Library has already been loaded) def
      /LIB_OVF (** Overflow in malloc while loading library) def
      /LIB_MERGE (** Unable to merge library into sysdict) def
      /LIB_INIT (** Unable to initialize loaded library) def
	    /VMR_ERR (** Cannot allocate D memory) def
      /VMR_STATE (** Memory alread minimized) def
      /ILL_OPAQUE (** Opaque dict type mismatch) def
      /FOLD_OPAQUE (** Illegal attempt to fold opaque object) def
      /NO_PLUGINS (** Compiled without plugin support) def |]
    end def |]
  end def
  /BUILD_ATLAS 2 dict dup begin |[
    /commands [
      /matmul_blas
      /decompLU_lp
      /backsubLU_lp
      /invertLU_lp
      /norm2
      /matvecmul_blas
      /triangular_solve
      /givens_blas
      /rotate_blas
      /xerbla_test
    ] def 
    /errors 100 dict dup begin |[
      /MATRIX_UNDEF_CUT (Matrix Error: undefined value in cut) def
      /MATRIX_ILLEGAL_CUT (Matrix Error: cut dimension less than 1) def
      /MATRIX_UNDER_CUT (Matrix Error: number of cut dimensions too small) def
      /MATRIX_NONMATCH_CUT (Matrix Error: array too small for cut) def
      /MATRIX_NONMATCH_SHAPE (Matrix Error: matrix dimensions don't match) def
      /MATRIX_PARAM_ERROR (Matrix Error: parameters to clapack illegal) def
      /MATRIX_INT_ERR (Matrix Error: Internal Error - message on stack) def
      /MATRIX_SINGULAR (Matrix Error: lu matrix is singular) def |]
    end def |]
  end def |]
end def

/xnode 1 dict dup begin |[
  /all 1 dict dup begin |[
    /commands [/Xwindows_ /Xconnect /Xdisconnect] def |]
  end def |]
end def

/master 2 dict dup begin |[
  /all 1 dict dup begin |[
    /commands [/getmyport /setconsole /console /killsockets] def |]
  end def
  /DM_HOST_IS_32_BIT 1 dict dup begin |[
    /commands [/readf32] def |]
  end def |]
end def

/plugins 1 dict dup begin /all 1 dict dup begin |[
  /commands [/loadlib /nextlib] def |]
end def end def

/terminal 1 dict dup begin /all 1 dict dup begin |[
  /commands [/nextevent /aborted /regex] def |]
end def end def

/pawn 1 dict dup begin /all 1 dict dup begin |[
  /commands [
    /send 
    /mpiprobe /mpiiprobe /mpisend /mpirecv 
    /mpibarrier /mpibroadcast
    /mpirank /mpisize
  ] def |]
end def end def

/dgen 1 dict dup begin /all 1 dict dup begin |[
  /parents [/common /quitable] def |]
end def end def

/dvt 1 dict dup begin |[
  /parents [/common /net /x /terminal /quitable] def
end def

/dnode 1 dict dup begin |[
  /parents [/common /net /node /x /xnode /master /regex /plugins] def
end def

/dpawn 1 dict dup begin |[
  /parents [/common /node /pawn /regex /plugins] def |]
end def

end _module
