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
/mattiny 100 def

/dosmall true def

| array | array-first-element
/offset {
  dup length 1 sub 1 exch getinterval
} bind def

| length type | sub-array-of-length
/offarr {
  exch 1 add exch array offset
} bind def

/dyadic_ts {
  style /SA eq {
    {/A1 /A3 /A5 /A7} {matsize typ offarr def} forall
    {A1 A3 A5 A7} {exec 1.1d exch copy pop} forall
    {/A9} {mattiny typ array def} forall
  } {
    {/A1 /A2 /A3 /A4 /A5 /A6 /A7 /A8} {matsize typ offarr def} forall
    {/A9 /A10} {mattiny typ array def} forall
    dosmall {
      {A1 A2 A3 A4 A5 A6 A7 A8} {exec 1.01d exch copy pop} forall
      {A9 A10} {exec 1.01d exch copy pop} forall
    } {
      {A1 A2 A3 A4 A5 A6 A7 A8} {exec 0 1 index length 2 1 ramp pop pop} forall
      {A9 A10} {exec 0 1 index length 2 1 ramp pop pop} forall
    } ifelse
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

/tests [/thread /parallel /serial /byhand /tiny] def

/AA_dy_test tests length dict dup begin
  /thread {A1 A2 op mkact exec pop} bind def
  /serial {A3 A4 op mkact exec pop} bind def
  /parallel {A5 A6 op mkact exec pop} bind def
  /tiny {A9 A10 op mkact exec pop} bind def
  /byhand {
    op /copy eq {
      0 1 A7 length 1 sub {/i name
        A7 i get A8 i put
      } for
    } {
      0 1 A7 length 1 sub {/i name
        A7 i get A8 i get op mkact exec A7 i put
      } for
    } ifelse
  } bind def
end def

/n 1.02d def
/AS_dy_test tests length dict dup begin
  /thread {A1 n op mkact exec pop} bind def
  /serial {A3 n op mkact exec pop} bind def
  /parallel {A5 n op mkact exec pop} bind def
  /tiny {A9 n op mkact exec pop} bind def
  /byhand {
    0 1 A7 length 1 sub {/i name
      A7 i get n op mkact exec A7 i put
    } for
  } bind def
end def

/SA_dy_test tests length dict dup begin
  /thread {n A1 op mkact exec /a1 name} bind def
  /serial {n A3 op mkact exec /a2 name} bind def
  /parallel {n A5 op mkact exec /a3 name} bind def
  /tiny {n A9 op mkact exec pop} bind def
  /byhand {
    op /copy eq {
      0 1 A7 length 1 sub {n A7 3 -1 roll put} for
    } {
      n 0 1 A7 length 1 sub {A7 exch get op mkact exec} for /a4 name
    } ifelse
  } bind def
end def

/style_op_test styles length dict dup begin
  /AA ops length dict dup begin
    dyc_ops {AA_dy_test def} forall
    /matmul 4 dict dup begin
      /thread {A1 A2 A3 matmul pop} bind def
      /serial {A4 A5 A6 matmul pop} bind def
      /parallel {A7 A8 A9 matmul pop} bind def
      /tiny {A13 A14 A15 matmul pop} bind def
      /byhand {
        0 1 A10 length 1 sub {/i name
          0 1 A10 0 get length 1 sub {/j name
            0 typ ctype
            0 1 A11 0 get length 1 sub {/k name
              A11 i get k get A12 k get j get mul add
            } for
            A10 i get j put
          } for
        } for
      } bind def
    end def
    /mattranspose 4 dict dup begin
      /thread {A1 A2 mattranspose pop} bind def
      /serial {A4 A5 mattranspose pop} bind def
      /parallel {A7 A8 mattranspose pop} bind def
      /tiny {A13 A14 mattranspose pop} bind def
      /byhand {
        0 1 A11 length 1 sub {/i name
          0 1 A11 0 get length 1 sub {/j name
            A11 i get j get A10 j get i put
          } for
        } for
      } bind def
    end def
    /matvecmul 4 dict dup begin
      /thread {A1 A2 A3 matvecmul pop} bind def
      /serial {A4 A5 A6 matvecmul pop} bind def
      /parallel {A7 A8 A9 matvecmul pop} bind def
      /tiny {A13 A14 A15 matvecmul pop} bind def
      /byhand {
        0 1 A11 length 1 sub {/i name
          0 typ ctype
          0 1 A12 length 1 sub {/j name
            A11 i get j get A12 j get mul add
          } for
          A10 i put
        } for
      } bind def
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
  /starred true def
  
  0 1 A1 length 1 sub {/i name
    A1 i get A3 i get roundne A1 i get A5 i get roundne or {
      /starred false def
      err 0 (In ) fax * style text (-) fax * op text
      ([) fax * i * number (]:\n) fax
      ( t = ) fax * A1 i get * number (\n) fax
      ( p = ) fax * A3 i get * number (\n) fax
      ( s = ) fax * A5 i get * number (\n) fax
      0 exch getinterval toconsole
      stop
    } {
      starred {A1 i get * ne {/starred false def} if} if
    } ifelse
  } for

  starred {(All ************\n) toconsole} if
} bind def

/A_dy_check_math {
  /starred true def
  
  0 1 A1 length 1 sub {/i name
    A1 i get A3 i get roundne
    A1 i get A5 i get roundne or
    A1 i get A7 i get roundne or {
      /starred false def
      err 0 (In ) fax * style text (-) fax * op text
      ([) fax * i * number (]:\n) fax
      ( t = ) fax * A1 i get * number (\n) fax
      ( p = ) fax * A3 i get * number (\n) fax
      ( s = ) fax * A5 i get * number (\n) fax
      ( b = ) fax * A7 i get * number (\n) fax
      0 exch getinterval toconsole
      stop
    } {
      starred {A1 i get * ne {/starred false def} if} if
    } ifelse
  } for

  starred {(All ************\n) toconsole} if
} bind def

/mat_check {
  /starred true def
  
  0 1 A1 length 1 sub {/i name
    0 1 A1 i get length 1 sub {/j name
      A1 i get j get A4 i get j get roundne
      A1 i get j get A7 i get j get roundne or {
        /starred false def
        err 0 (In ) fax * style text (-) fax * op text
        ([) fax * i * number (,) fax * j * number (]:\n) fax
        ( t = ) fax * A1 i get j get * number (\n) fax
        ( p = ) fax * A4 i get j get * number (\n) fax
        ( s = ) fax * A7 i get j get * number (\n) fax
        0 exch getinterval toconsole
        stop
      } {
        starred {A1 i get j get * ne {/starred false def} if} if
      } ifelse
    } for
  } for

  starred {(All ************\n) toconsole} if
} bind def

/mat_check_math {
  /starred true def
  
  0 1 A1 length 1 sub {/i name
    0 1 A1 i get length 1 sub {/j name
      A1 i get j get A4 i get j get roundne
      A1 i get j get A7 i get j get roundne or
      A1 i get j get A10 i get j get roundne or {
        /starred false def
        err 0 (In ) fax * style text (-) fax * op text
        ([) fax * i * number (,) fax * j * number (]:\n) fax
        ( t = ) fax * A1 i get j get * number (\n) fax
        ( p = ) fax * A4 i get j get * number (\n) fax
        ( s = ) fax * A7 i get j get * number (\n) fax
        ( b = ) fax * A10 i get j get * number (\n) fax
        0 exch getinterval toconsole
        stop
      } {
        starred {A1 i get j get * ne {/starred false def} if} if
      } ifelse
    } for
  } for

  starred {(All ************\n) toconsole} if
} bind def


/log {ln 10d ln div} bind def
/antilog {10d exch pwr} bind def

/setdig10 {/d ctype
  10d exch 1d sub pwr /dig10 name
} bind def
14d setdig10
/eps 1e-100 def

/round {/val name
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
        eps v1 lt eps neg v1 gt and
        eps v2 lt eps neg v2 gt and 2 copy or {and not} {pop pop

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
  } ifelse
} bind def

/vec_check {
  /starred true def
  
  0 1 A1 length 1 sub {/i name
    A1 i get A4 i get roundne A1 i get A7 i get roundne or {
      /starred false def
      err 0 (In ) fax * style text (-) fax * op text
      ([) fax * i * number (]:\n) fax
      ( t = ) fax * A1 i get * number (\n) fax
      ( p = ) fax * A4 i get * number (\n) fax
      ( s = ) fax * A7 i get * number (\n) fax
      0 exch getinterval toconsole
      stop
    } {
      starred {A1 i get * ne {/starred false def} if} if
    } ifelse
  } for

  starred {(All ************\n) toconsole} if
} bind def

/vec_check_math {
  /starred true def
  
  0 1 A1 length 1 sub {/i name
    A1 i get A4 i get roundne
    A1 i get A7 i get roundne or
    A1 i get A10 i get roundne or {
      /starred false def
      err 0 (In ) fax * style text (-) fax * op text
      ([) fax * i * number (]:\n) fax
      ( t = ) fax * A1 i get * number (\n) fax
      ( p = ) fax * A4 i get * number (\n) fax
      ( s = ) fax * A7 i get * number (\n) fax
      ( b = ) fax * A10 i get * number (\n) fax
      0 exch getinterval toconsole
      stop
    } {
      starred {A1 i get * ne {/starred false def} if} if
    } ifelse
  } for

  starred {(All ************\n) toconsole} if
} bind def

/S_dy_check {
  /starred true def
  
  a1 a2 roundne a1 a3 roundne or {
    /starred false def
    err 0 (In ) fax
    * style text (-) fax * op text (\n) fax
    ( t = ) fax * a1 * number (\n) fax
    ( p = ) fax * a2 * number (\n) fax
    ( s = ) fax * a3 * number (\n) fax
    0 exch getinterval toconsole
    stop
  } {
    starred {a1 * ne {/starred false def} if} if
  } ifelse

  starred {(All ************\n) toconsole} if
} bind def

/S_dy_check_math {
  /starred true def
  
  a1 a2 roundne
  a1 a3 roundne or
  a1 a4 roundne or {
    /starred false def
    err 0 (In ) fax
    * style text (-) fax * op text  (\n) fax
    ( t = ) fax * a1 * number (\n) fax
    ( p = ) fax * a2 * number (\n) fax
    ( s = ) fax * a3 * number (\n) fax
    ( b = ) fax * a4 * number (\n) fax
    0 exch getinterval toconsole
    stop
  } {
    starred {a1 * ne {/starred false def} if} if
  } ifelse

  starred {(All ************\n) toconsole} if
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

/style_op_check_math styles length dict dup begin
  /AA ops length dict dup begin
    dyc_ops {~A_dy_check_math def} forall
    /matmul ~mat_check_math def
    /mattranspose ~mat_check_math def
    /matvecmul ~vec_check_math def
  end def
  /AS ops length dict dup begin
    dy_ops {~A_dy_check_math def} forall
  end def
  /SA ops length dict dup begin
    dy_ops {~S_dy_check_math def} forall
    /copy ~A_dy_check_math def
  end def
end def

/test_wrap 4 dict dup begin
  /thread {exec} bind def
  /serial {serialize} bind def
  /parallel {1 makethreads ~exec stopped t makethreads ~stop if} bind def
  /byhand {exec} bind def
  /tiny {100 {dup exec} repeat pop} bind def
end def

/rowsa 1023 def
/colsa 1027 def
/colsb 1025 def

/matmul_ts {
  {/A13 /A14 /A15} {/c name
    c mattiny list def
    0 1 mattiny 1 sub {/i name
      mattiny typ array c mkact exec i put
    } for
  } forall
  
  {/A1 /A4 /A7 /A10} {/c name
    c rowsa list def
    0 1 rowsa 1 sub {/i name
      colsb typ offarr c mkact exec i put
    } for
  } forall

  {/A2 /A5 /A8 /A11} {/a name
    a rowsa list def
    /rm 0 def
    0 1 rowsa 1 sub {/i name
      colsa typ offarr
      dup a mkact exec i put
      0 colsa rm 1 ramp rm add /rm name pop
    } for
  } forall

  {/A3 /A6 /A9 /A12} {/b name
    b colsa list def
    /rm 0 def
    0 1 colsa 1 sub {/i name
      colsb typ offarr
      dup b mkact exec i put
      0 colsb rm 1 ramp rm add /rm name pop
    } for
  } forall
} bind def

/mattranspose_ts {
  {/A13 /A14} {/c name
    c mattiny list def
    0 1 mattiny 1 sub {/i name
      mattiny type array c mkact exec i put
    } for
  } forall
  
  {/A1 /A4 /A7 /A10} {/b name
    b colsa list def
    0 1 colsa 1 sub {/i name
      rowsa typ offarr b mkact exec i put
    } for
  } forall

  {/A2 /A5 /A8 /A11} {/a name
    a rowsa list def
    /rm 0 def
    0 1 rowsa 1 sub {/i name
      colsa typ offarr
      dup a mkact exec i put
      0 colsa rm 1 ramp rm add /rm name pop
    } for
  } forall
} bind def

/matvecmul_ts {
  {/A13 /A15} {/c name
    c 1 mattiny typ array copy def
  } forall
  
  {/A1 /A4 /A7 /A10} {
    rowsa typ offarr def
  } forall

  {/A14} {/a name
    a mattiny list def
    0 1 mattiny 1 sub {/i name
      1 mattiny typ array copy a mkact exec i put
    } for
  } forall
  
  {/A2 /A5 /A8 /A11} {/a name
    a rowsa list def
    /rm 0 def
    0 1 rowsa 1 sub {/i name
      colsa typ offarr
      dup a mkact exec i put
      0 colsa rm 1 ramp rm add /rm name pop
    } for
  } forall

  {/A3 /A6 /A9 /A12} {/b name
    b colsa typ offarr def
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

/testmath {
  /t threads def
  types {/typ name
    err 0 (Starting math tests for type ) fax * typ text (\n) fax
    0 exch getinterval toconsole
    
    ops {/op name
      type_op typ get op get {
        err 0 (Starting math tests for op ) fax * op text
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
              {/thread /serial /parallel /byhand} {/test name
                style_op_test style get op get test get
                test_wrap test get exec
              } forall
              
              
              {style_op_check_math style get op get exec} stopped {
                (Halting\n) toconsole halt
              } if

              err 0 (Ending ) fax * style text
              ( for op ) fax * op text
              ( for type ) fax * typ text (\n) fax
              0 exch getinterval toconsole
            } stopped /ts_ _layer ~stop if
          } if
        } forall

        err 0 (Ending math tests for op ) fax * op text
        ( for type ) fax * typ text (\n) fax
        0 exch getinterval toconsole
      } if
    } forall

    err 0 (Ending math tests for type ) fax * typ text (\n) fax
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
              {/thread /serial /parallel /tiny} {/test name
                style_op_test style get op get test get /func name
                test_wrap test get /tw name
                /reps reps_tp_st_op typ get style get op get exec def
                {reps {{func} tw} repeat} timed /time name
                err 0 (Time for ) fax * style text (, ) fax
                * test text
                test /thread eq {([) fax * threads * number (]) fax} if
                ([) fax * reps * number (] = ) fax
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

/threadreps 1e6 def
/fulltest {
  (\nTesting makethreads...\n) toconsole
  (Number of threads: ) toconsole threads _ pop
  threadreps {threads 1 makethreads makethreads} repeat
  (\nTesting ops:\n) toconsole
  (Number of threads: ) toconsole threads _ pop
  testmath |testops
  (\nTesting time:\n) toconsole
  (Number of threads: ) toconsole threads _ pop
  testtime
} bind def
  
end _module
