/PROCESSES module 100 dict dup begin 

/STDIN  0 makefd def
/STDOUT 1 makefd def
/STDERR 2 makefd def

| [ (exec) ... | bool
/execfg {
  fork dup null eq {pop closelist spawn} if
  counttomark 1 add 1 roll cleartomark
  waitpid {0 eq} {false} ifelse
} bind def

| [ (exec) ... | fd-in fd-out pid
/execbg {
  pipe pipe | in-r in-w out-r out-w
  fork dup null eq {
    exch closefd
    dup 1 false makefd dupfd
    dup 2 false makefd dupfd
    closefd
    closefd
    dup 0 true makefd dupfd
    pop closelist spawn
  } if

  5 1 roll
  close
  3 -1 roll closefd | [ (exec) ... pid in-w out-r \\ ]
  counttomark 3 add 3 roll cleartomark
} bind def

| [[ (exec) ... [ (exec) ... [ (exec) | bool
/execpipefg {
  fork dup null eq {
    pop
  } if
 
  {
    counttomark dup 1 eq {3 1 roll pop pop exit} if
    1 add 1 roll cleartomark
  } loop
  waitpid {0 eq} {false} ifelse
} bind def
 

{
  counttomark 0 eq {pop} {
    pipe fork dup null eq {
      pop closefd
      dup 0 true makefd dupfd closefd
      closelist spawn
    } if
    
  } {
  } ifelse
} bind def

end _module