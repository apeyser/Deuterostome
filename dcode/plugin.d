/PLUGINS module 1000 dict dup begin

/buf 1000 1024 mul /b array def

/header_handle {
  (OPAQUE_MEM\(frame, ) package (_) h (_N\))
} bind def

/header_handle_ind {textit /h__ name
  (OPAQUE_MEM\(frame, ) package (_) h__ (_N\))
} bind def

/body_handle {exch
  package (_) 3 -1 roll textit (\() 5 -1 roll textit (\))
} bind def

/body_name {
  package (_) 3 -1 roll textit (_N)
} bind def

/null_handle {/settee name /settyp name /frame name /hname name
  (if \(TAG\(OPAQUE_MEM\() frame textit (, ) package (_) hname textit (_N\)\)
       != NULLOBJ\)
  ) settee textit ( = \() settyp textit (\) ) hname textit frame textit
  body_handle textit
} bind def

/error_ {
  (RETURN_ERROR\() package (_) 4 -1 roll textit (\))
} bind def

/getbufferframe {(OPAQUE_MEM\(procframe, buffernameframe\))} def
/getbufferfrom  {(OPAQUE_MEM\() exch (, buffernameframe\))} def

/build_handle {/dest name /size name /x name
  /handle (\(initframe\)) def
(
   {
      B initframe[FRAMEBYTES];
      B* procframe = make_opaque_frame\() size textit
       (, opaquename, \n)
       handledict null ne {
         handledict {pop textit /h name
           package (_) h (_N,\n)
         } forall
       } if
       (NULL\);
     if \(! procframe\) return VM_OVF;
)
  x (
     moveframe\(procframe, ) dest textit (\);
   }
)
  /handle ~body_handle def
} bind def
     
/make_handle {textit /n name
  (OPAQUE_MEM_SET\(procframe, ) package (_) n (_N, initframe\))
} bind def

/make_handle_nr {textit /n name
  (OPAQUE_MEM_SET_NR\(procframe, ) package (_) n (_N, initframe\))
} bind def

/textname_ 100 /b array def

/textit_ 10 dict dup begin
  /nullclass {pop (NULL)} bind def
  /numclass {
    textname_ 0 * 4 -1 roll * number 0 exch getinterval
    dup length /b array copy
  } bind def
  /nameclass {
    textname_ 0 * 4 -1 roll mkact text 0 exch getinterval
    dup length /b array copy
  } bind def
  /opclass {
    textname_ 0 * 4 -1 roll text 0 exch getinterval
  } bind def
  /arrayclass {
    dup type /B ne {
      textname_ 0 ({) fax
      3 -1 roll {
        * exch * number (,) fax
      } forall
      (}) fax 0 exch getinterval
      dup length /b array copy
    } if
  } bind def
  /listclass {
    textname_ 0 ({) fax
    3 -1 roll {
      dup class textit_ exch get exec fax (,) fax
    } forall
    0 exch getinterval
    dup length /b array copy
  } bind def
  /dictclass {
    textname_ 0 ({) fax
    3 -1 roll {/obj name /nm name
      nm /nameclass get exec fax (,) fax
      obj textit_ obj class get exec fax (,) fax
    } forall
    0 exch getinterval
    dup length /b array copy
  } bind def
  /markclass {pop ([)} bind def
  /boolclass {{(TRUE)} {(FALSE)} ifelse} bind def
end def
/textit {dup class textit_ exch get exec} bind def

/makehandles {[]} bind def
/makehandles_ind {[]} bind def
/nameslist [] bind def
/bodyheaders () bind def
/errsdict null def
/makebodycode null def
/headercode () def
/bodycode () def
/inicode () def
/finicode () def

/toupper {
  dup length /b array copy
  0 1 2 index length 1 sub {/i name
    dup i get dup 97 lt 1 index 122 gt or {pop} {
      32 sub 1 index i put
    } ifelse
  } for
} bind def

/all {
  end currentdict end PLUGINS begin begin
  /islocal name /isbody name /file name /path name

  file 0 file length 2 sub getinterval toupper /package name
  file 0 file length 2 sub getinterval /package_ name
  
  makeops /opslist name
  makehandles dup length dup 0 eq {pop pop null} {
    dict dup begin exch {
      {} forall def
    } forall end
  } ifelse /handledict name
  makehandles_ind dup length dup 0 eq {pop pop null} {
    dict dup begin exch {
      {} forall def
    } forall end
  } ifelse /handledict_ind name

  isbody {dobody} {doheader} ifelse
  
} bind def

/dobody {
  buf 0
  bodyheaders fax (
#include ") fax package_ fax (.h"

UL ll_type = 0;
L op_hi\(void\) {return wrap_hi\(") fax
  package_ fax ( V) fax * version * number ("\);}
L op_libnum\(void\) {return wrap_libnum\(ll_type\);}
L ll_errc[] = {
) fax

  errsdict null ne {
    errsdict {pop textit /e name
      package fax (_) fax e fax (,\n) fax
    } forall
  } if
(  0L
};

B* ll_errm[] = {
) fax
  errsdict null ne {
    errsdict {exch pop /e name
      ("** ) fax package_ fax (: ) fax e fax (",\n) fax
    } forall
  } if

(  NULL
};

B* ll_export[] = {
  "hi", \(B*\) op_hi,
  "libnum", \(B*\) op_libnum,
  "INIT_", \(B*\) op_INIT_,
  "FINI_", \(B*\) op_FINI_,
) fax

  opslist {0 get textit /op name
    (") fax op fax (", \(B*\) op_) fax op fax (,\n) fax
  } forall
(  "", NULL
};

B opaquename[FRAMEBYTES];
) fax

  handledict null ne {
    handledict {pop textit /h name
      (static B ) fax package fax (_) fax h fax (_N[FRAMEBYTES];\n) fax
    } forall
    (\n) fax
  } if

  nameslist {textit /n name
    (static B ) fax package fax (_) fax n fax (_N[FRAMEBYTES];\n) fax
  } forall
  (\n) fax
  
  [bodycode] ~fax forall
(
  L op_INIT_\(void\) {
) fax

  handledict null ne {
    (makename\(") fax package fax (_HANDLE", opaquename\);\n) fax
    handledict {pop textit /h name
      (makename\(") fax h fax (", ) fax
      package fax (_) fax h fax (_N\);\n) fax
    } forall
  } if
  
  nameslist {textit /n name
    (makename\(") fax n fax (", ) fax
    package fax (_) fax n fax (_N\);\n) fax
  } forall

  inicode fax (
  return OK;
}

 L op_FINI_\(void\) {
) fax
  finicode fax (
  return OK;
}

) fax

  /handle ~body_handle def
  /nameit ~body_name def
  opslist {dup 0 get textit /op name 1 get /opb name
(L op_) fax op fax (\(void\) {
) fax
    [opb] {fax} forall (
}

) fax
  } forall (
) fax 0 exch getinterval path file writefile
} bind def
    

/doheader {
  buf 0 (\
#ifndef ) fax package fax (_H
#define ) fax package fax (_H

#define PLUGIN_NAME ) fax package_ fax (
#include ) fax islocal {("../src/plugin.h")} {(<dm/plugin.h>)} ifelse fax (

) fax

  /handle ~header_handle def
  handledict null ne {
    handledict {/h_ name textit /h name
      /h_ [h_] def
      (#define ) fax package fax (_) fax h fax (\(frame\) \() fax
      h_ {fax} forall
      (\)\n) fax
    } forall
  } if

  /handle ~header_handle_ind def
  handledict_ind null ne {
    handledict_ind {/h_ name textit /h name
      /h_ [h_] def
      (#define ) fax package fax (_) fax h fax (\(frame\) \() fax
      h_ {fax} forall
      (\)\n) fax
    } forall
  } if

  (\n) fax headercode fax (\n) fax

  /errn 1L def
  errsdict null ne {
    errsdict {exch textit /e name /e_ name
      (#define ) fax package fax (_) fax e fax
      ( \() fax * errn * number (L\)) fax (\n) fax
      /errn errn 1 add def
    } forall
  } if

  opslist {0 get textit /op name
    (\n#define op_) fax op fax ( EXPORTNAME\(op_) fax op fax (\)\n) fax
    (L op_) fax op fax (\(void\);\n) fax
  } forall

(
#endif
) fax
  0 exch getinterval path file writefile
} bind def

end _module
