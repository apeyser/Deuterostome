/color module 100 dict dup begin

|======================== color module ============================
|
| Adds the ability to use ansi color codes to color shells
| or dvt interface output
|
| Specifically, defines an operator color_fax that insert control codes
| into a string, while faxing it onto a buffer
| and an operator color_text which does the same, but uses a builder
|
| (buffer) index [ /color_name .. ] (string-to-color) color_fax (buffer) index
|
| (buffer) index [ /color_name .. ] ~builder color_text (buffer) index
|
| builder is an active object (procedure) that takes a buffer
|  and an index, and inserts text into that buffer from that
|  index, leaving behind the same buffer, and an index to the next character
|  such as {[list] xtext}
|
|  (buffer) index builder (buffer) index+n
|
| color_fax & color_text have aliases defined in userdict
|
|----------------------------------------------------------------
|
| Additionally, a toconsole supercharger has been added:
| [/color_name...] make_toconsole --
|
| It supercharges toconsole to color all calls to toconsole,
| and aliases the supercharged toconsole to userdict
|
|-----------------------------------------------------------------
|
| the defined color names are:
|   foreground colors: black red green yellow blue magenta cyan white
|   background colors: on_black on_red ... on_white
|   special effects: bold faint italic underlined slow_blink rapid_blink
|                    negative
|   reset code: reset
|
| These are additive so: [/red /italic /bold /blue]
|   will create a blue italic bold string
| and: [/red /italic /bold /reset /underlined ]
|   will create an uncolored, underlined string
|
|-------------------------------------------------------------------
|
| All these function check for dnode_escape in the current context.
| 
| If /dnode they add a \ to allow proper transfer of
|     the resulting string back to the dvt.
| if /dvt, control characters are inserted
| if /escape, print none-control code marker (ie, ^[[0m, for ex)
| if /none, don't put in any colors at all
|
| In the emacs bash shell or dvt shell, ansi-color-for-comint-on
| must be set in order to display colors
|
|========================================================================

userdict /dvt known {/dvt} {/dnode} ifelse userdict /dnode_escape put

/escape_ { | /l-number (buffer) index -> string
  * 4 -1 roll * number (m) fax
  0 exch getinterval
} bind def

/dvt    {(\033[   )  2 escape_} def
/dnode  {(\033[   ) 2 escape_} def
/escape {(^[[   )    3 escape_} def
/none   {pop ()}                def

/make_colors { | [ [start [/name ...]] .. ] -->
  {
    dup 0 get exch 1 get {
      ~[2 index ~dnode_escape ~mkact ~exec] def
      1 add
    } forall pop
  } forall
} def

[
  [30 [/black /red /green /yellow /blue /magenta /cyan /white]]

  [40 [/on_black /on_red /on_green /on_yellow
       /on_blue /on_magenta /on_cyan /on_white]]

  [0 [/reset /bold /faint /italic /underlined
      /slow_blink /rapid_blink /negative]]
] make_colors

/nocolors [] def
/_color_fax {| (buffer) /l-index --> (buffer) /l-index
  colors null eq {/colors nocolors def} if
  {
    string (\n) search {
      /substring name pop /string name
      substring length 0 eq {(\n) fax} {
        colors {mkact exec fax} forall
        substring    fax
        reset        fax
        (\n)         fax
      } ifelse
    } {
      length 0 ne {
        colors {mkact exec fax} forall
        string       fax
        reset        fax
      } if
      exit
    } ifelse
  } loop
} bind def

/color_fax { | (buffer) /l-index [/color ...] (string) --> (buffer) /l-index  
  color begin {
    /string name
    /colors name
    _color_fax
  } stopped end ~stop if
} bind def

/color_text {| (buffer) /l-index [/color ...] {} --> (buffer) /l-index
  color begin save /color_text_ name {
    /proc   name
    /colors name
    /i      name
    /buffer name

    /tbuffer buffer length i sub /b array def
    color_text_ capsave

    /string tbuffer 0 proc 0 exch getinterval def
    buffer i _color_fax
  } stopped {color_text_ restore end stop} if
  color_text_ restore end
} bind def

/color_string { | [/color ...] (string) --> (string)
  color begin {
    /string name
    /colors name
    /i 0 def
    
    string (\n) {search
      exch length 0 ne {/i i 1 add def} if
      not ~exit if
    } loop

    10 dnode_escape mkact exec length
      colors length 1 add mul
      i mul
      string length add
    /b array 0 _color_fax
    0 exch getinterval
  } stopped end ~stop if
} def

/color_fax    find userdict /color_fax  put
/color_text   find userdict /color_text put
/color_string find userdict /color_string put

/node_buffer 1024 10 mul /b array def
| [/color ...] | -- (defines new /toconsole in userdict)
/make_toconsole {
  color begin {
    /node_color name
| (string) | -- <<output>>
    /node_toconsole ~[
      node_buffer 0 node_color 4 -1 ~roll
      ~color_fax 0 ~exch ~getinterval systemdict /toconsole get
    ] bind dup userdict /toconsole put def
  } stopped end ~stop if
} bind def

end _module
