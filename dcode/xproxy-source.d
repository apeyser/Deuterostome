/XPROXY_SOURCE 100 dict dup begin

/ddef {
  1 index exch def dup find userdict 3 -1 roll put
} bind def

/proxy_ret {
  XPROXY_SOURCE begin {
    userdict exch known not {pop pop} {
      exch dup capsave /tsave name
      save
      [exch {} forall] /params name
      dup capsave /proxy_save name
      tsave
    } ifelse
    continue
  } stopped end {stop} if
  restore
} bind ddef

/proxy_locked false def

| /name num-params-in num-params-out | ~name
/makeproxy {/params_in name /proxy name
  ~[
    proxy params_in {
      XPROXY_SOURCE begin {
        /params_in name /proxy name
        save
        /cmd params_in 0 eq {proxy} {
          [params_in 3 add 3 roll proxy mkact]
        } ifelse def
        
        console ~[
          pid cmd {
            getsocket ~[
              [4 -1 roll mkact exec] 5 3 roll {proxy_ret} ~lock
            ] send
          } ~exec ~restore
        ] send
        
        restore
        /proxy_locked true def
        halt 
        /proxy_locked false def
        XPROXY_SOURCE /params get {} forall
        XPROXY_SOURCE /proxy_save get restore
      } stopped end {stop} if
    } ~lock
  ]
} bind def
 
/screensize dup 0 makeproxy bind def
/makewindow dup 3 makeproxy bind def
/makewindowtop dup 2 makeproxy bind def
/deletewindow dup 1 makeproxy bind def
/mapwindow dup 2 makeproxy bind def
/resizewindow dup 3 makeproxy bind def
/Xsync dup 0 makeproxy bind def
/mapcolor 1 makeproxy bind def
/fillrectangle 3 makeproxy bind def
/drawline 3 makeproxy bind def
/drawsymbols 5 makeproxy bind def
/drawtext 4 makeproxy bind def

| /windowname
/register_proxy {
  console ~[windowname pid {getsocket
    4 -1 roll dup capsave 4 1 roll
    10 dict dup begin 
      exch /src name
      exch /pid name
      exch /windowname name
      /take_input_focus ~[
        src ~[windowname pid {take_input_focus_recv} ~locked] ~send
        ~end
      ] bind def
      /windowsize {
        save
        src ~[5 3 roll windowname pid {windowsize_recv} ~locked] send
        end
        restore
      } bind def
      /drawwindow ~[
        src ~[windowname pid {drawwindow} ~locked] ~send
        ~end
      ] bind def
      /mouseclick {
        save
        src ~[6 3 roll windowname pid {mouseclick_recv} ~locked] send
        end
        restore
      } bind def
      /delete_window ~[
        src ~[windowname pid {delete_window_recv} ~locked] ~send
        ~end
      ] bind def
    windowname end userdict exch put
    restore
  } ~locked] send
} def

/take_input_focus_recv {
  XPROXY_SOURCE /proxy_locked get {pop pop} {
    userdict exch known not {pop} {
      1 index capsave 
      userdict exch get begin {take_input_focus} stopped pop
    } ifelse
  } ifelse
  restore
} bind ddef

/drawwindow_recv {
  XPROXY_SOURCE /proxy_locked get {pop pop} {
    userdict exch known not {pop} {
      1 index capsave
      userdict exch get begin {drawwindow} stopped pop
    } ifelse
  } ifelse
  restore
} bind ddef

/delete_window_recv {
  XPROXY_SOURCE /proxy_locked get {pop pop} {
    userdict exch known not {pop} {
      1 index capsave
      userdict exch get begin {delete_window} stopped pop
    } ifelse
  } ifelse
  restore
} bind ddef

/windowsize_recv {
  XPROXY_SOURCE /proxy_locked get {pop pop pop pop} {
    userdict exch known not {pop pop pop} {
      3 index capsave
      userdict exch get begin {windowsize} stopped pop
    } ifelse
  } ifelse
  restore
} bind ddef

/mouseclick_recv {
  XPROXY_SOURCE /proxy_locked get {pop pop pop pop pop} {
    userdict exch known not {pop pop pop pop} {
      4 index capsave
      userdict exch begin {mouseclick} stopped pop
    } ifelse
  } ifelse
  restore
} bind ddef

end _module
