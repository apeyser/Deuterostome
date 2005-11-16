/ARR module

200 dict dup begin

/len 1024 1024 mul 5 mul def
/reps 50 def

/ops [/add /sub /mul /div /pwr /mod /thearc /copy] bind def
/types [/b /w /l /s /d] bind def

/err 1024 /b array def
/testops {
  /t threads def
  types {/typ name
    err 0 (Starting tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
    
    /testops_ layer {
      {/A1 /A2 /A3 /A4 /A5 /A6} {1023 typ array def} forall
      ops {/op name
        err 0 (Starting tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
        
        {
          {
            {A1 A2 A3 A4 A5 A6} {exec 0 1 index length 2 1 ramp pop pop} forall
            /style (AA) def
            err 0 (Starting AA for op ) fax * op text
            ( for type ) fax * typ text (\n) fax
            0 exch getinterval toconsole
            
            A1 A2 op mkact exec pop
            {A3 A4 op mkact exec pop} serialize
            1 makethreads A5 A6 op mkact exec pop t makethreads

            err 0 (Ending AA for op ) fax * op text
            ( for type ) fax * typ text (\n) fax
            0 exch getinterval toconsole
          } {
            {A1 A3 A5} {exec 0 1 index length 2 1 ramp pop pop} forall
            /style (AS) def
            /copy op ne {
              err 0 (Starting AS for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole
              
              A1 3 op mkact exec pop
              {A3 3 op mkact exec pop} serialize
              1 makethreads A5 3 op mkact exec pop t makethreads
            
              err 0 (Ending AS for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole
            } if
          }
        } {
          exec
          0 1 A1 length 1 sub {/i name
            A1 i get A3 i get ne A1 i get A5 i get ne or {
              err 0 (In ) fax style fax (-) fax * op text
              ([) fax * i * number (]:) fax
              ( t = ) fax * A1 i get * number
              ( p = ) fax * A3 i get * number
              ( s = ) fax * A5 i get * number
              (\n) fax 0 exch getinterval toconsole
            } if
          } for
        } forall

        err 0 (Starting SA for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
        
        {A1 A2 A3} {exec 0 1 index length 2 1 ramp pop pop} forall
        3 A1 op mkact exec /a1 name
        {3 A2 op mkact exec} serialize /a2 name
        1 makethreads 3 A3 op mkact exec /a3 name t makethreads

        err 0 (Ending SA for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole

        /copy op ne {
          a1 a2 ne a1 a3 ne or {
            err 0 (In SA-) fax * op text
            ( t = ) fax * a1 * number
            ( p = ) fax * a2 * number
            ( s = ) fax * a3 * number
          } if
        } {
          0 1 A1 length 1 sub {/i name
            a1 i get 3 ne a2 i get 3 ne a3 i get 3 ne or or {
              err 0 (In SA-copy[) fax * i * number (]:) fax
              ( t = ) fax * a1 i get * number
              ( p = ) fax * a2 i get * number
              ( s = ) fax * a3 i get * number
              (\n) fax 0 exch getinterval
            } if
          } for
        } ifelse
        
        err 0 (Ending tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
      } forall

      err 0 (Ending tests for type ) fax * typ text (\n) fax
      0 exch getinterval toconsole
    } stopped /testops_ _layer ~stop if
  } forall
} bind def

/timed {
  gettime neg [ 3 -1 roll exec cleartomark |]
  gettime add
} bind def

/testtime {
  /t threads def
  types {/typ name
    err 0 (Starting tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
    
    /testops_ layer {
      {/A1 /A2 /A3 /A4 /A5 /A6} {len typ array def} forall
      ops {/op name
        err 0 (Starting tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
        
        {A1 A2 A3 A4 A5 A6} {exec 0 1 index length 2 1 ramp pop pop} forall
        /style (AA) def
        err 0 (Starting AA for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole

        {
          {(threaded) {reps {A1 A2 op mkact exec pop} repeat}}
          {(parallel) {{reps {A3 A4 op mkact exec pop} repeat} serialize}}
          {(serial) {
            1 makethreads reps {A5 A6 op mkact exec pop} repeat t makethreads
          }}
        } {
          exec timed /time name /threading name
          err 0 (Time for ) fax style fax (, ) fax threading fax ( = ) fax
          * time * number (\n) fax
          0 exch getinterval toconsole
        } forall

        err 0 (Ending AA for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole

        op /copy ne {
          {A1 A3 A5} {exec 0 1 index length 2 1 ramp pop pop} forall
          /style (AS) def
          err 0 (Starting AS for op ) fax * op text
          ( for type ) fax * typ text (\n) fax
          0 exch getinterval toconsole
          
          {
            {(threaded) {reps {A1 3 op mkact exec} repeat}}
            {(parallel) {{reps {A3 3 op mkact exec} repeat} serialize}}
            {(serial) {
              1 makethreads reps {A5 3 op mkact exec} repeat t makethreads
            }}
          } {
            exec timed /time name /threading name
            err 0 (Time for ) fax style fax (, ) fax threading fax ( = ) fax
            * time * number (\n) fax
            0 exch getinterval toconsole
          } forall
        
          err 0 (Ending AS for op ) fax * op text
          ( for type ) fax * typ text (\n) fax
          0 exch getinterval toconsole
        } if

        err 0 (Starting SA for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
        
        {A1 A2 A3} {exec 0 1 index length 2 1 ramp pop pop} forall
        {
          {(threaded) {reps {3 A1 op mkact exec} repeat}}
          {(parallel) {{reps {3 A2 op mkact exec} repeat} serialize}}
          {(serial) {
            1 makethreads 3 reps {A3 op mkact exec} repeat t makethreads
          }}
        } {
          exec timed /time name /threading name
          err 0 (Time for SA, ) fax threading fax ( = ) fax
          * time * number (\n) fax
          0 exch getinterval toconsole
        } forall

        err 0 (Ending SA for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole

        err 0 (Ending tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
      } forall

      err 0 (Ending tests for type ) fax * typ text (\n) fax
      0 exch getinterval toconsole
    } stopped /testops_ _layer ~stop if
  } forall
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
