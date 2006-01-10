/ARR module

200 dict dup begin

/len 1024 1024 mul 5 mul def

/dy_ops [/add /sub /mul /div /pwr /mod /thearc] def
/dyc_ops [dy_ops {} forall /copy] def
/mat_ops [/matmul /matvecmul /mattranspose] def

/ops [
  dyc_ops {} forall
  mat_ops {} forall
] bind def

/types [/b /w /l /s /d] bind def

/styles [/AS /AA /SA] def

/matsize 1023 def

/dyadic_ts {
  style /SA eq {
    {/A1 /A3 /A5} {matsize typ array def} forall
    {A1 A3 A5} {exec 1.1d exch copy pop} forall
  } {
    {/A1 /A2 /A3 /A4 /A5 /A6} {matsize typ array def} forall
    {A1 A2 A3 A4 A5 A6} {exec 0 1 index length 2 1 ramp pop pop} forall
  } ifelse
} bind def

/type_op types length dict dup begin
  types {
    ops length dict dup begin
    dyc_ops {true def} forall
    mat_ops {2 index /d eq def} forall
    end def
  } forall
end def

/style_op styles length dict dup begin
  styles {
    ops length dict dup begin
      dy_ops {true def} forall
      /copy 2 index /AS ne def
      mat_ops {2 index /AA eq def} forall
    end def
  } forall
end def

/tests [/thread /parallel /serial] def

/AA_dy_test tests length dict dup begin
  /thread {A1 A2 op mkact exec pop} def
  /serial {A3 A4 op mkact exec pop} def
  /parallel {A5 A6 op mkact exec pop} def
end def

/n 3d def
/AS_dy_test tests length dict dup begin
  /thread {A1 n op mkact exec pop} def
  /serial {A3 n op mkact exec pop} def
  /parallel {A5 n op mkact exec pop} def
end def

/SA_dy_test tests length dict dup begin
  /thread {n A1 op mkact exec /a1 name} def
  /serial {n A3 op mkact exec /a2 name} def
  /parallel {n A5 op mkact exec /a3 name} def
end def

/style_op_test styles length dict dup begin
  /AA ops length dict dup begin
    dyc_ops {AA_dy_test def} forall
    /matmul 3 dict dup begin
      /thread {A1 A2 A3 matmul pop} def
      /serial {A4 A5 A6 matmul pop} def
      /parallel {A7 A8 A9 matmul pop} def
    end def
    /mattranspose 3 dict dup begin
      /thread {A1 A2 mattranspose pop} def
      /serial {A4 A5 mattranspose pop} def
      /parallel {A7 A8 mattranspose pop} def
    end def
    /matvecmul 3 dict dup begin
      /thread {A1 A2 A3 matvecmul pop} def
      /serial {A4 A5 A6 matvecmul pop} def
      /parallel {A7 A8 A9 matvecmul pop} def
   end def
  end def
  /SA ops length dict dup begin
    dyc_ops {SA_dy_test def} forall
  end def
  /AS ops length dict dup begin
    dy_ops {AS_dy_test def} forall
  end def
end def

/A_dy_check {
  0 1 A1 length 1 sub {/i name
    A1 i get A3 i get roundne A1 i get A5 i get roundne or {
      err 0 (In ) fax * style text (-) fax * op text
      ([) fax * i * number (]:) fax
      ( t = ) fax * A1 i get * number
      ( p = ) fax * A3 i get * number
      ( s = ) fax * A5 i get * number
      (\n) fax 0 exch getinterval toconsole
      stop
    } if
  } for
} def

/mat_check {
  0 1 A1 length 1 sub {/i name
    0 1 A1 i get length 1 sub {/j name
      A1 i get j get A4 i get j get roundne
      A1 i get j get A7 i get j get roundne or {
        err 0 (In ) fax * style text (-) fax * op text
        ([) fax * i * number (,) fax * j * number (]:) fax
        ( t = ) fax * A1 i get j get * number
        ( p = ) fax * A4 i get j get * number
        ( s = ) fax * A7 i get j get * number
        (\n) fax 0 exch getinterval toconsole
        stop
      } if
    } for
  } for
} bind def

/log {ln 10d ln div} def
/antilog {10d exch pwr} def

/setdig10 {
  10d exch 1d sub pwr /dig10 name
} def
6d setdig10

/round {
  /dig10 10d rounddig 1d sub pwr def
  /val name
  val dup 0 lt {abs} if
  log dup floor /expo name
  1d mod 10d exch pwr expo 0 lt {10d mul} if dig10 mul floor dig10 div
  log 10d exch expo add pwr
  val 0 lt {-1d mul} if
} bind def

/roundne {
  dup type dup /S ne exch /D ne and {ne} {
    2 copy eq {pop pop false} {
      /v1 name /v2 name

      v1 dup 0 lt {neg} if
      log dup floor /exp1 name
      1d mod 10d exch pwr exp1 0 lt {10d mul} if dig10 mul floor
      v1 0 lt {neg} if /man1 name
      
      v2 dup 0 lt {neg} if
      log dup floor /exp2 name
      1d mod 10d exch pwr exp2 0 lt {10d mul} if dig10 mul floor
      v2 0 lt {neg} if /man2 name
      
      exp1 exp2 ne man1 man2 ne or
    } ifelse
  } ifelse
} bind def

/vec_check {
  0 1 A1 length 1 sub {/i name
    A1 i get A4 i get roundne A1 i get A7 i get roundne or {
      err 0 (In ) fax * style text (-) fax * op text
      ([) fax * i * number (]:) fax
      ( t = ) fax * A1 i get * number
      ( p = ) fax * A4 i get * number
      ( s = ) fax * A7 i get * number
      (\n) fax 0 exch getinterval toconsole
      stop
    } if
  } for
} bind def

/S_dy_check {
  a1 a2 roundne a1 a3 roundne or {
    err 0 (In ) fax
    * style text (-) fax * op text
    ( t = ) fax * a1 * number
    ( p = ) fax * a2 * number
    ( s = ) fax * a3 * number
    (\n) fax 0 exch getinterval toconsole
    stop
  } if
} bind def

/style_op_check styles length dict dup begin
  /AA ops length dict dup begin
    dyc_ops {~A_dy_check def} forall
    /matmul ~mat_check def
    /mattranspose ~mat_check def
    /matvecmul ~vec_check def
  end def
  /AS ops length dict dup begin
    dy_ops {~A_dy_check def} forall
  end def
  /SA ops length dict dup begin
    dy_ops {~S_dy_check def} forall
    /copy ~A_dy_check def
  end def
end def

/test_wrap 3 dict dup begin
  /thread {exec} def
  /serial {serialize} def
  /parallel {1 makethreads exec t makethreads} def
end def

/rowsa 1023 def
/colsa 1027 def
/colsb 1025 def

/matmul_ts {
  {/A1 /A4 /A7} {/c name
    c rowsa list def
    0 1 rowsa 1 sub {/i name
      colsb typ array c mkact exec i put
    } for
  } forall

  {/A2 /A5 /A8} {/a name
    a rowsa list def
    /rm 0 def
    0 1 rowsa 1 sub {/i name
      colsa typ array
      dup a mkact exec i put
      0 colsa rm 1 ramp rm add /rm name pop
    } for
  } forall

  {/A3 /A6 /A9} {/b name
    b colsa list def
    /rm 0 def
    0 1 colsa 1 sub {/i name
      colsb typ array
      dup b mkact exec i put
      0 colsb rm 1 ramp rm add /rm name pop
    } for
  } forall
} bind def

/mattranspose_ts {
  {/A1 /A4 /A7} {/b name
    b colsa list def
    0 1 colsa 1 sub {/i name
      rowsa typ array b mkact exec i put
    } for
  } forall

  {/A2 /A5 /A8} {/a name
    a rowsa list def
    /rm 0 def
    0 1 rowsa 1 sub {/i name
      colsa typ array
      dup a mkact exec i put
      0 colsa rm 1 ramp rm add /rm name pop
    } for
  } forall
} bind def

/matvecmul_ts {
  {/A1 /A4 /A7} {
    rowsa typ array def
  } forall
  
  {/A2 /A5 /A8} {/a name
    a rowsa list def
    /rm 0 def
    0 1 rowsa 1 sub {/i name
      colsa typ array
      dup a mkact exec i put
      0 colsa rm 1 ramp rm add /rm name pop
    } for
  } forall

  {/A3 /A6 /A9} {/b name
    b colsa typ array def
    b mkact exec 0 colsa 0 1 ramp pop pop
  } forall
} bind def

/ts ops length dict dup begin
  dyc_ops {~dyadic_ts def} forall
  /matmul ~matmul_ts def
  /matvecmul ~matvecmul_ts def
  /mattranspose ~mattranspose_ts def
end def

/err 1024 /b array def
/testops {
  /t threads def
  types {/typ name
    err 0 (Starting tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
    
    ops {/op name
      type_op typ get op get {
        err 0 (Starting tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole

        styles {/style name
          style_op style get op get {
            /ts_ layer {
              err 0 (Starting ) fax * style text
              ( for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole

              ts op get exec
              {/thread /serial /parallel} {/test name
                style_op_test style get op get test get
                test_wrap test get exec
              } forall
            
              {style_op_check style get op get exec} stopped {
                (Halting\n) toconsole halt
              } if
              
              err 0 (Ending ) fax * style text
              ( for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole
            } stopped /ts_ _layer ~stop if
          } if
        } forall
        
        err 0 (Ending tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
      } if
    } forall

    err 0 (Ending tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
  } forall
} bind def

/base_reps 5 def
/reps_tp_st_op types length dict dup begin
  types {
    styles length dict dup begin
    styles {
      ops length dict dup begin
      dyc_ops {
        2 index dup /d eq exch /s eq or {{base_reps 100 mul}} {
          {base_reps 20 mul}
        } ifelse def
      } forall
      mat_ops {{base_reps 100 mul} def} forall
      /matmul {base_reps} def
      end def
    } forall
    end def
  } forall
end def

/timed {
  gettime neg [ 3 -1 roll exec cleartomark |]
  gettime add
} bind def

/testtime {
  /t threads def
  /matsize_ matsize def
  /matsize len def
  types {/typ name
    err 0 (Starting tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
    
    ops {/op name
      type_op typ get op get {
        err 0 (Starting tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole

        styles {/style name
          style_op style get op get {
            /ts_ layer {
              err 0 (Starting ) fax * style text
              ( for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole

              ts op get exec
              {/thread /serial /parallel} {/test name
                style_op_test style get op get test get /func name
                test_wrap test get /tw name
                /reps reps_tp_st_op typ get style get op get exec def
                {reps {{func} tw} repeat} timed /time name
                err 0 (Time for ) fax * style text (, ) fax
                * test text ([) fax * reps * number (] = ) fax
                * time * number (\n) fax
                0 exch getinterval toconsole
              } forall

              err 0 (Ending ) fax * style text
              ( for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole
            } stopped /ts_ _layer ~stop if
          } if
        } forall

        err 0 (Ending tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
      } if
    } forall

    err 0 (Ending tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
  } forall
  /matsize matsize_ def
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
