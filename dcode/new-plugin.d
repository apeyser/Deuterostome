/NEW_PLUGINS module 1000 dict dup begin

| lower | LOWER
/toupper {
  save 1024 /b array 1 index capsave    | (string)/name save (string)
  0 * 5 -1 roll text 0 exch getinterval | save (string)
  dup length /b array copy exch restore
  0 1 2 index length 1 sub {/i name
    dup i get dup 97 lt 1 index 122 gt or {pop} {
      32 sub 1 index i put
    } ifelse
  } for
} bind def

/make_buffer {
  /buf 1000 1024 mul /b array def
  /bufn 0 def
} bind def

/__ {buf bufn * 4 -1 roll text /bufn name pop} bind def

/n_ {buf bufn * 4 -1 roll * number /bufn name pop} bind def

/nl {(\n)__} bind def

/makename {
  plugin_name dup /plugin_name_l name
  toupper         /plugin_name_u name
} bind def

/makeheader {
  make_buffer
  (#ifndef DM_)__ plugin_name_u __ (_H)__ nl
  (#define DM_)__ plugin_name_u __ (_H)__ nl nl

  (#define PLUGIN_NAME )__ plugin_name_l __ nl nl
  (#include )__ islocal {("../src/plugin.h")} {(<dm/plugin.h>)} ifelse __ nl nl
  
  plugin_types {{} forall /mem_attr name /mem_type name /mem_tag name /mem_name name
    /mem_name_u 30 /b array 0 * mem_name text 0 exch getinterval toupper def
    (#define )__ plugin_name_u __ (_)__ mem_name_u __ (\(frame\))__
    ( \()__ mem_type __ (\(OPAQUE_MEM\(frame, )__ 
    plugin_name_u __ (_)__ mem_name_u __ (_N)__ (\)\)\))__ nl nl

    (#define )__ plugin_name_u __ (_)__ mem_name_u __ (_INIT\(dframe\))__
    ( do { \\)__ nl 
    (B frame[FRAMEBYTES]; \\)__ nl
    (TAG\(frame\) = \()__ mem_tag __ (\); \\)__ nl
    (ATTR\(FRAME\) = )__ mem_attr __ (; \\)__ nl
    (OPAQUE_MEM_SET\(dframe, )__ plugin_name_u __ (_)__ mem_name_u __ (N, frame\); \\)__ nl
    (} while \(0\))__ nl nl
  } forall

  plugin_types used 0 ne {
    (#define MAKE_)__ plugin_name_u __ (\(frame, size\) \\)__ nl 
    (do { \\)__ nl
    (if \(! \(frame = MAKE_OPAQUE_DICT\(size)__
    plugin_types {pop /mem_name name
      /mem_name_u 30 /b array 0 * mem_name text 0 exch getinterval toupper def
      (, )__ mem_name_u __ (_N)__
    } forall
    (\)\)\) return VM_OVF; \\)__ nl
  
    plugin_types {pop /mem_name name
      /mem_name_u 30 /b array 0 * mem_name text 0 exch getinterval toupper def
      plugin_name_u __ (_)__ mem_name_u __ (_INIT\(frame\); \\)__ nl
    } forall
    (} while \(0\);)__ nl nl
  } if

  /en 1 def
  plugin_errs {pop /err_name name
    (#define )__ plugin_name_u __ (_)__ err_name toupper __ ( \()__ en n_ (L\))__ nl
    /en en 1 add def
  } forall nl

  plugin_ops {pop /op_name name
    op_name /init_ eq op_name /fini_ eq or not {
      (#define )__ (op_)__ op_name __ ( EXPORTNAME\(op_)__ op_name __ (\))__ nl
      (P op_)__ op_name __ (\(void\);)__ nl nl
    } if
  } forall

  (#endif //DM_)__ plugin_name_u __ (_H)__ nl nl

  buf bufn 0 exch getinterval path file writefile
} bind def

/makebody {
  make_buffer
  (#include "dm_)__ plugin_name_l __ (-header.h")__ nl nl

  plugin_types {pop /mem_name name
    /mem_name_u 30 /b array 0 * mem_name text 0 exch getinterval toupper def
    (PLUGIN_DEC_NAME\()__ mem_name_u __ (;)__ nl
  } forall

  (PLUGIN_INTRO\()__ plugin_version n_ (\);)__ nl nl

  (P ll_errc[] = { )__ nl
  plugin_errs {pop /err_name name
    (  )__ plugin_name_u __ (_)__ err_name toupper __ (, )__ nl
  } forall
  (  0L)__ nl
  (};)__ nl nl

  (B* ll_errm[] = { )__ nl
  plugin_errs {/err_string name pop
    (  \(B*\)"** )__ plugin_name_l __ (: )__ err_string __ (", )__ nl
  } forall
  (  NULL)__ nl
  (};)__ nl nl
  
  (B* ll_export[] = { )__ nl
  (  PLUGIN_OPS,)__ nl
  plugin_ops {pop /op_name name
    op_name /init_ ne {
      op_name /fini_ eq {/FINI_ /op_name name} if
      (  PLUGIN_OP\()__ op_name __ (\),)__ nl
    } if
  } forall
  plugin_ops /init_ known plugin_types used 0 ne or {
    (  PLUGIN_OP\(INIT_\),)__ nl
  } if
  (  \(B*\)"", NULL)__ nl
  (};)__ nl nl

  plugin_ops /init_ known plugin_types used 0 ne or {
    (P op_INIT_\(void\) {)__ nl
    plugin_types used 0 ne {
      (  makename\(\(B*\))__ plugin_name_u __ (_HANDLE, opaquename\); )__ nl
      plugin_types {pop /mem_name name
        /mem_name_u 30 /b array 0 * mem_name text 0 exch getinterval toupper def
        (  PLUGIN_DEF_NAME\()__ plugin_name_u __ (\);)__ nl
      } forall
    } if
    plugin_ops /init_ known {
     (  return init_\(\);)
    } {
      (  return OK;)
    } ifelse
    __ nl (})__ nl nl
  } if

  plugin_ops /fini_ known {
    (P op_FINI_\(void\) {return fini_\(\);})__ nl nl
  } if

  plugin_ops {pop /op_name name
    op_name /init_ ne op_name /fini_ ne and {
      (P op_)__ op_name __ (\(void\) {return )__ op_name __ (\(\);})__ nl nl
    } if
  } forall

  buf bufn 0 exch getinterval path file writefile
} bind def

/all {
  /islocal name /isbody name /file name /path name
  makename
  isbody ~makebody ~makeheader ifelse
} bind def

end _module
