/ops 1000 dict dup begin 

/mkmap {/mapper name
  /ldict 1 dict def
  mapper /parents known {mapper /parents get {
    find /pdict name
    pdict used ldict used add dict ldict merge /ldict name
    pdict /commands known {
      pdict /commands get /cpdict name
      ldict /commands known not {1 dict ldict /commands put} if
      ldict /commands get /cldict name
      cldict used cpdict used add dict cldict merge /cldict name
      cldict ldict /commands put

      cpdict {/cmds name /ifdef name
        [
          cmds {} forall
          cldict ifdef known {cldict ifdef get {} forall} if
        ] cldict ifdef put
      } forall
    } if
  } forall} if

  mapper /commands known {
    mapper /commands get /cmdict name
    ldict /commands known not {
      ldict used 1 add dict ldict merge /ldict name
      1 dict ldict /commands put
    } if
    ldict /commands get /cldict name

    cldict used cmdict used add dict cldict merge /cldict name
    cldict ldict /commands put

    cmdict {/icmdict name /ifdef name
      [
        cldict ifdef known {cldict ifdef get {} forall} if
        icmdict /commands known {
          icmdict /commands get {[
            40 /b array 0 * 4 index text 0 exch getinterval
            40 /b array 0 (op_) fax * 6 -1 roll text 0 exch getinterval
          ]} forall
        } if
        icmdict /specials known {
          icmdict /specials get {} forall
        } if
      ] cldict ifdef put
    } forall
  } if

  mapper /parents known {mapper /parents get {
    find /pdict name
    pdict /errors known {
      pdict /errors get /epdict name
      ldict /errors known not {
        ldict used 1 add dict ldict merge /ldict name
        1 dict ldict /errors put
      } if
      ldict /errors get /eldict name
      eldict used epdict used add dict eldict merge /eldict name
      eldict ldict /errors put

      epdict {/errs name /ifdef name
        [
          errs {} forall
          eldict ifdef known {eldict ifdef get {} forall} if
        ] eldict ifdef put
      } forall
    } if
  } forall} if
        
  mapper /errors known {
    mapper /errors get /emdict name
    ldict /errors known not {
      ldict used 1 add dict ldict merge /ldict name
      1 dict ldict /errors put
    } if
    ldict /errors get /eldict name

    eldict used emdict used add dict eldict merge /eldict name
    eldict ldict /errors put

    emdict {/iemdict name /ifdef name
      [
        eldict ifdef known {eldict ifdef get {} forall} if
        iemdict /errors known {
          iemdict /errors get {[
            40 /b array 0 * 6 -1 roll text 0 exch getinterval
            3 -1 roll
          ]} forall
        } if
      ] eldict ifdef put
    } forall
  } if

  ldict
} bind def

/mksmap {/errs name /cmds name /parents name
  3 dict dup begin |[
    parents length 0 gt {/parents parents def} if
    cmds length 0 gt {
      /commands 1 dict dup begin |[
        /all 1 dict dup begin |[
          /commands cmds def |]
        end def |]
      end def
    } if
    errs used 0 gt {
      /errors 1 dict dup begin |[
        /all 1 dict dup begin |[
          /errors errs def |]
        end def |]
      end def
    } if |]
  end mkmap
} bind def


/buffer 100 1024 mul /b array def
/buffern 0 def

/__ {
  buffer buffern * 4 -1 roll text /buffern name pop
} def

/nl {(\n)__} def

/bifdef {/ifdef name ifdef /all ne {(#ifdef )__ ifdef __ nl} if} bind def
/eifdef {ifdef /all ne {(#endif //)__ ifdef __ nl} if} bind def

/all {
  /group name /file name /path name
  ops group get /gdict name

  (// Automatically produced from src/codegen/ops.d)__ nl
  (// DO NOT EDIT HERE!)__ nl nl

  (B* sysop[] = )__ nl
  ({)__ nl
  gdict /commands get {exch
    bifdef {
      (  \(B*\) ")__ dup 0 get __ (", \(B*\) )__ 1 get __ (,)__ nl
    } forall eifdef
  } forall
  (  \(B*\) "", \(B*\) NULL)__ nl
  (};)__ nl nl

  (P syserrc[] = {)__ nl
  gdict /errors get {exch
    bifdef {
      (  )__ 0 get __ (,)__ nl
    } forall eifdef
  } forall
  (  0L)__ nl
  (};)__ nl nl

  (B* syserrm[] = {)__ nl
  gdict /errors get {exch
    bifdef {
      (  \(B*\) ")__ 1 get __ (",)__ nl
    } forall eifdef
  } forall
  (  \(B*\) NULL)__ nl
  (};)__ nl

  buffer 0 buffern getinterval path file writefile
} bind def

/common 2 dict dup begin |[
  /commands 1 dict dup begin |[
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
      /specials [[(]) (op_closelist)]] def |]
    end def |]
  end def 
  /errors 1 dict dup begin |[
    /all 1 dict dup begin |[
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
  end def |]
end mkmap def

/regex 2 dict dup begin |[
  /commands 1 dict dup begin |[
    /DM_ENABLE_REGEX 1 dict dup begin |[
      /commands [/regex /regexi] def |]
    end def |]
  end def
  /errors 1 dict dup begin |[
    /DM_ENABLE_REGEX 1 dict dup begin |[
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
  end def |]
end mkmap def

/term [] [/quit] 1 dict mksmap def

/net [] [
  /connect /disconnect /send /getsocket
  /getmyname /getmyfqdn 
] 1 dict dup begin /LOST_CONN (** Lost connection) def end
mksmap def

/x [] [
  /Xwindows /Xdisplayname
  /screensize /makewindow
  /deletewindow /mapwindow /resizewindow
  /Xsync /mapcolor /drawline
  /drawsymbols /drawtext
  /makewindowtop 
  /Xauth /Xauthrev /Xauthgen /Xauthset
] 8 dict dup begin |[
  /NO_XWINDOWS(** X windows unavailable) def
  /X_ERR (** Error in X windows) def
  /X_BADFONT (** Bad X windows font) def
  /X_BADHOST (** Cannot connect to X server) def
  /X_SEC_MISS (** X Security Extension missing) def
  /X_SEC_GEN (** X Security Extension unable to generate) def
  /X_SEC_REV (** X Security Extension unable to revoke) def
  /X_SEC_LIB (** X Security library missing) def
end mksmap def

/net_term [/net /term] [] 1 dict mksmap def

/noterm 2 dict dup begin |[
  /commands 2 dict dup begin |[
    /all 1 dict dup begin |[
      /commands [
        /lock /serialize /threads /makethreads 
        /loadlib /nextlib 
        /tostderr /setconsole /console
        /halt /continue 
        /vmresize /killsockets
        /getplugindir
      ] def |]
    end def
    /BUILD_ATLAS 1 dict dup begin |[
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
      ] def |]
    end def |]
  end def 
  /errors 3 dict dup begin  |[
    /all 2 dict dup begin |[
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
    /BUILD_ATLAS 1 dict dup begin |[
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
  end def |]
end mkmap def

/net_noterm [/net /noterm] [/socketdead]
  1 dict dup begin /DEAD_SOCK (Dead socket connection) def end
mksmap def

/x_target [/x] [/Xwindows_ /Xconnect /Xdisconnect] 1 dict mksmap def

/dnodish [/common /net_noterm /x_target /regex] [] 1 dict mksmap def

/dnode 2 dict dup begin |[
  /parents [/dnodish] def
  /commands 2 dict dup begin |[
    /all 1 dict dup begin |[
      /commands [/getmyport] def |]
    end def
    /DM_HOST_IS_32_BIT 1 dict dup begin |[
      /commands [/readf32] def |]
    end def |]
  end def |]
end mkmap def

/dgen [/common /term] [] 1 dict mksmap def

/dvt [/common /net_term /x] [/nextevent /aborted /regex] 1 dict mksmap def

/dpawn [/dnodish] [] 1 dict mksmap def

end def
