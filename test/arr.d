/ARR module

200 dict dup begin

/len 1024 1024 mul 10 mul def
/reps 50 def

/timed {
    gettime neg [ 3 -1 roll exec cleartomark |]
    gettime add
} bind def

/addtest {
    DA1 0 len 0 1 ramp pop pop
    DA2 0 len 0 1 ramp pop pop
    {DA1 reps {DA2 add} repeat} timed _ pop
} bind def

/addtestd {
    /mem layer {
      /DA1 len /d array def
      /DA2 len /d array def
      addtest
    } stopped /mem _layer ~stop if
} bind def

/addl {
    /mem layer {
      /DA1 len /l array def
      /DA2 len /l array def
      addtest
    } stopped /mem _layer ~stop if
} bind def

/addb {
    /mem layer {
      /DA1 len /b array def
      /DA2 len /b array def
      addtest
    } stopped /mem _layer ~stop if
} bind def

/movetest {
  DA1 0 len 0 1 ramp pop pop
  DA2 0 len 0 1 ramp pop pop
  {DA1 reps {DA2 copy} repeat} timed _ pop
} bind def

/moveb  {
  /mem layer {
    /DA1 len /b array def
    /DA2 len /b array def
    movetest
  } stopped /mem _layer ~stop if
} bind def

end _module
