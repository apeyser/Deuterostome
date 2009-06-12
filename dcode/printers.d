/PRINTER module 100 dict dup begin



{ begin
  { Xwindows not ~stop if
    screensize /scrH name /scrW name
    /printersmem [ 100 {[100 /b array 100 /b array false]} repeat ] def
    printersmem getprinters /printernodes name
    printers /rowH get /rowH name
    /wW scrW 3 div def /wH printernodes length rowH mul 6 add def
    [ [ 2 scrH wH sub 2 sub wW wH ]
      myname myshortname makewindow /wid name pop
    /woutline [ 0 0 wW 1 sub 0 wW 1 sub wH 1 sub 0 wH 1 sub 0 0 ] def
    /wpane [ 1 1 wW 2 sub wH 2 sub ] def
    /wbox 4 list def  
    printers /windowsize get /windowsize name
    printers /drawwindow get /drawwindow name
    printers /mouseclick get /mouseclick name
    printers /checkcolor get /checkcolor name
    printers /outcolor   get /outcolor   name      
    printers /BLACK get /BLACK name
    printers /GRAY get /GRAY name
    printers /BG get /BG name
    printers /TEXT get /TEXT name
    printer /GREEN get /GREEN put  
    currentdict userdict debug_dict /line get 0 (/w) fax * wid * number
      0 exch getinterval mkact exec put
    wid true mapwindow
  } stopped pop end
} userdict /makeprinters put

100 dict dup begin

/NORMALFONT 
    (-b&h-lucida-medium-r-normal-sans-0-0-75-75-p-0-iso8859-10) def
/BOLDFONT 
    (-b&h-lucida-bold-r-normal-sans-0-0-75-75-p-0-iso8859-10) def
/BLACK <d 0 0 0 > mapcolor def
/GRAY  <d 0.5 0.5 0.5 > mapcolor def
/BLUE  <d 0 0 1 > mapcolor def
/RED   <d 1 0 0 > mapcolor def
/GREEN <d 0 0.5 0> mapcolor def  
/BG <d 235 243 248 > 255 div mapcolor def
/TEXT [ NORMALFONT BLACK -1 -1 ] def
/BTEXT [ BOLDFONT BLACK -1 -1 ] def  
/rowH 10 def

|------------------ resist resizing of a macro window

/windowsize {
  wH ne exch wW ne or {
    { [
      wid wW wH resizewindow
    } stopped cleartomark
  } if end
} bind def

|------------------ draw a macro window

/drawwindow {
  { [
    wid woutline BLACK drawline
    wid wpane BG fillrectangle
    /y 2 def
    printersmem getprinters /printernodes name
    printernodes {
      dup 0 get /s name
      2 get {
        2 wbox 0 put y wbox 1 put wW 4 sub wbox 2 put rowH wbox 3 put  
        wid wbox GREEN fillrectangle
        wid 5 y s BTEXT drawtext pop pop pop
      } {
        wid 5 y s TEXT drawtext pop pop pop
      } ifelse
      /y y rowH add def
    } foreach 
  } stopped cleartomark
  end
} bind def
  
/mouseclick { 
    { [ 4 1 roll
      /mM name /mY name /mX name
      /krow mY 1 sub rowH div def
      krow printernodes length lt {
        printernodes krow get 1 get toconsole
      } if
    } stopped cleartomark
    drawwindow   
} bind def

end userdict /macros put

  
end _module
  
