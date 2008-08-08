/MATRIX_TEST module 200 dict dup begin

/m 1000 def
/n ~m def
/k ~m def

/done {(Finished test: ) toconsole toconsole (\n) toconsole} bind def
/test_eps 1e-12 def

/compare {/temp name /a name /b name
  0d a temp copy b sub dup mul add sqrt
  0d b dup mul add sqrt div dup test_eps lt {exch pop} {
    (Test error: ) toconsole exch toconsole _
    halt
  } ifelse
  pop
} bind def

/compare3 {/temp name /a name /b name /c name
  b a temp compare
  c a temp compare
} bind def

/matmul_test {/beta name /alpha name settrans 
  C1 C_cuts beta A1_ A_cuts_ transA B1_ B_cuts_ transB alpha
  matmul_blas pop pop
} bind def

/matmulold_test {/beta name /alpha name settrans 
  Cold_temp A2_old B2_old matmul {alpha mul pop} forall
  C2 beta mul C_temp add pop
} bind def

/matmul_cmp {
  C1 C2 C_temp compare
} bind def

/gmres_test {
  y1 A1 A_cuts x1 m GMRES begin gmres end pop
} bind def

/gmres_cmp {
  x3 x3_data get_vector x1 x2 x_temp compare3
} bind def

/lu_test {
  A1 A_cuts piv1 decompLU_lp {3 {pop} repeat} {
    (Singular!\n) toconsole halt
  } ifelse
  y1 x1 copy A1 A_cuts piv1 backsubLU_lp pop
} bind def

/luold_test {
  A2_old piv2 decompLU {pop} {
    (Singular!\n) toconsole halt
  } ifelse
  A2_old piv2 y2 x2 copy backsubLU pop
} bind def

/luinv_test {
  A1 A_cuts piv1 decompLU_lp {3 {pop} repeat} {
    (Singular!\n) toconsole halt
  } ifelse
  A1 A_cuts piv1 invertLU_lp pop pop
} bind def

/luinvold_test {
  A2_old piv2 B2_old invertLU {pop} {
    (Singular!\n) toconsole halt
  } ifelse
} bind def

/luinv_cmp {
  A1 B2 C_temp compare
} bind def

/ksp_test {
  ksp A3 y3 x3 ksp_solve
} bind def

/settrans {/transB name /transA name
  transA not {
    /A1_ A1 def
    /A2_ A2 def
    /A3_ A3 def
    /A_cuts_ A_cuts def
  } {
    /A1_ A1t def
    /A2_ A2t def
    /A3_ A3t def
    /A_cuts_ At_cuts def
  } ifelse
  
  transB not {
    /B1_ B1 def
    /B2_ B2 def
    /B_cuts_ B_cuts def
  } {
    /B1_ B1t def
    /B2_ B2t def
    /B_cuts_ Bt_cuts def
  } ifelse
} bind def

/matvecmul_test {/beta name /alpha name false settrans 
  y1 beta A1_ A_cuts_ transA x1 alpha matvecmul_blas pop
} bind def

/matvecmulold_test {/beta name /alpha name false settrans 
  y2 beta mul
  y_temp A2_old x2 matvecmul alpha mul
  add pop
} bind def

/matvecmulp_test {
  y3 beta A3_ transA x3 alpha pmatvecmul
} bind def

/matvecmul_cmp {
  y3 y3_data get_vector y1 y2 y_temp compare3
} bind def

/trisolve_test {false settrans
  x1 x_temp copy A1_ A_cuts_ transA true triagonal_u triangular_solve
  y1 copy pop
} bind def

/trisolveold_test {false settrans
  n 1 sub -1 0 {/i name
    x2 i get
    A2 A_cuts i cut pop x_temp copy
    i 1 add 1 index length 1 index sub getinterval
    y2 i 1 add 1 index length 1 index sub getinterval mul
    sub
    A2 A_cuts i cut pop i get
    div y2 i put
  } for
} bind def

/trisolve_cmp {
  y1 y2 y_temp compare
} bind def

| [ [[test1 test2 comparator] [(name) params...]]... ]
/base_tests [
  [2 [~matmul_test ~matmulold_test ~matmul_cmp ~do_full] [
    [(matmul01nn) false false 0 1]
    [(matmul10nn) false false 1 0]
    [(matmul02nn) false false 0 2]
    [(matmul11nn) false false 1 1]
    [(matmul12nn) false false 1 2]
    [(matmul01nt) false true 0 1]
    [(matmul10nt) false true 1 0]
    [(matmul02nt) false true 0 2]
    [(matmul11nt) false true 1 1]
    [(matmul12nt) false true 1 2]
    [(matmul01tn) true false 0 1]
    [(matmul10tn) true false 1 0]
    [(matmul02tn) true false 0 2]
    [(matmul11tn) true false 1 1]
    [(matmul12tn) true false 1 2]
    [(matmul01nt) true true 0 1]
    [(matmul10nt) true true 1 0]
    [(matmul02nt) true true 0 2]
    [(matmul11nt) true true 1 1]
    [(matmul12nt) true true 1 2]
  ]]
  [2 [~luinv_test ~luinvold_test ~luinv_cmp {symmetric do_full and}] [
    [(luinv)]
  ]]
  [3 [~matvecmul_test ~matvecmulold_test ~matvecmulp_test 
    ~matvecmul_cmp ~do_full] [
    [(matvecmul01n) false 0 1]
    [(matvecmul01t) true 0 1]
    [(matvecmul10n) false 1 0]
    [(matvecmul10t) true 1 0]
    [(matvecmul11n) false 1 1]
    [(matvecmul11t) true 1 1]
    [(matvecmul02n) false 0 2]
    [(matvecmul02t) true 0 2]
    [(matvecmul12n) false 1 2]
    [(matvecmul12t) true 1 2]
  ]]
  [2 [~trisolve_test ~trisolveold_test ~trisolve_cmp
    {triagonal do_full and symmetric and}][
    [(trisolven) false]
    [(trisolvet) true]
  ]]
  [3 [~lu_test ~luold_test ~ksp_test ~gmres_cmp ~symmetric] [
    [(lu)]
  ]]
  [3 [~gmres_test ~luold_test ~ksp_test 
    ~gmres_cmp {symmetric gmres_on and do_full and}] [
    [(gmres)]
  ]]
] def

/gmres_on false def

/run_tests {
  base_tests {/itest name
    /testn itest 0 get def
    /test1   itest 1 get 0 get def
    /test2   itest 1 get 1 get def
    /test3   testn 2 gt {itest 1 get testn 1 sub get} ~null ifelse def
    /testcmp itest 1 get testn get def
    /conds   itest 1 get testn 1 add get def
    itest 1 get {/ktest name
      dup propagate
      conds {
        (Starting: ) toconsole ktest 0 get toconsole (\n) toconsole
        {{1 test1} {2 test2} {3 test3}} {/ctest name
          /ctest find 1 get null ne {
            gettime
            (Starting test#) toconsole /ctest find 0 get _ pop
            ktest 1 ktest length 1 sub getinterval {} forall
            /ctest find 1 get exec
            gettime exch sub
            (Time: ) toconsole _ pop
          } if
        } forall
        ktest 0 get testcmp
        ktest 0 get done
      } if
    } forall
  } forall
  pop
} bind def

/base_setup {
  /triagonal false def
  /triagonal_u false def
  /full false def
  
  1 C1 copy pop

  0 B1 copy pop
  0 1 n 1 sub {/i name
    B1 B_cuts i cut pop
    0 1 k 1 sub {/j name
      j i k mul add 1 index j put
    } for
    pop
  } for

  0 1 n 1 sub {x1 1 index put} for
  0 1 m 1 sub {y1 1 index put} for

  /ksp dup ksp_create def
} bind def
 
/identity_tests {
  {
    base_setup

    0 A1 copy pop
    0 1 m 1 sub {/i name
      1 A1 A_cuts i cut pop i n mod put
    } for

    m ~[m n {/n name /m name /nl name /n0 name
      /matArows nl 1 add /l array 
        0 n1 1 add 0 1 ramp pop
      def
      /matAcols nl /l array 
        0 1 nl 1 sub {/i name
          i n0 add n mod 1 index i put
        } for
      def
      /matAdata 1 nl /d array copy def
    } ~exec] execrange

    /A3 dup ~matArows ~matAcols /sparse m n mat_create def
    A3 ~matAdata mat_fill_data
    /A3t dup A3 mat_dup dup mat_transpose def

    {}
 } run_tests
} bind def

/triagonal_nu_tests {
  {
    base_setup
    triagonal_setup
  } run_tests
} bind def

/triagonal_u_tests {
  {
    base_setup
    /triagonal_u true def
    triagonal_setup
  } run_tests
} bind def

/triagonal_setup {
  /triagonal true def

  0 A1 copy pop
  m 1 sub -1 0 {/i name
    A1 A_cuts i cut pop 0 exch copy
    i m 1 sub eq not {
      A1 A_cuts i 1 add cut pop exch copy 0.5 mul |i 1 add mul
      y1 i 1 add get 0.5 mul |i 1 add mul
      y1 i put
    } if
    triagonal_u {1} {i 1 add} ifelse exch i put
    triagonal_u {1} {i 1 add} ifelse y1 i get add y1 i put
  } for

  m ~[m n triagonal_u {/triagonal_u name /n name /m name /nl name /n0 name
    /matArows nl 1 add /l array def
    0        matArows 0 put
    m n0 sub matArows 1 put
    2 1 matArows last {/row name | x_i = x_{i-1} + (x_{i-1} - x_{i-2}) - 1
      matArows row 1 sub get
      dup matArows row 2 sub get sub add
      1 sub matArows row put
    } for

    /matAcols matArows dup last get /l array def
    matScols 0
    1 1 matArows last {
      matArows exch get 1 index sub
      n 1 index sub
      1 ramp
    } for pop pop

    /matAdata matAcols length /d array def
    0 1 matArows last 1 sub {/row name
      matAcols
      matArows row get matArows row 1 add get 1 index sub
      getinterval
      0 1 3 index last {/i name
        triagonal_u {1} {n0 row add i 1 add add} ifelse
        0.5 i pwr mul
        1 index i put 
      } for pop
    } for
  } ~exec] execrange

  /A3 dup ~matArows ~matAcols /sparse m n mat_create def
  A3 ~matAdata mat_fill_data
  /A3t dup A3 mat_dup dup mat_transpose def

  {}
} bind def

/full_model (s4split_shift_35_1) def

/full_on false def

/do_full {full_on full not or} def

/full_tests {
  {
    full_on {
      base_setup
      /full true def
      
      GAT begin
      full_model read
      end
      
      GAT /MODEL get /Matrix get
      0 1 m 1 sub {/i name
        dup i get 0 n getinterval
        A1 A_cuts i cut pop copy pop
      } for
      /gatData name

      /A3 dup /dense m n mat_create def
      {~[
        exch m getrange /nl name /n0 name
        gatData n0 n mul nl n mul getinterval {
          dup length /d array copy /matAdata name
        } ~exec
      ]} execpawns
      A3 ~matAdata mat_fill_data
      /A3t dup A3 mat_dup dup mat_transpose def
      
      GAT /MODEL get /myName get forgetmodule
    } if
    {}
  } run_tests
} bind def

/propagate {exec
  A1t_old A1_old mattranspose pop
  A1 A2 copy pop
  A1t A2t copy pop
  
  B1t_old B1_old mattranspose pop
  B1 B2 copy pop
  B1t B2t copy pop

  C1t_old C1_old mattranspose pop
  C1 C2 copy pop
  C1t C2t copy pop
  
  x1 x2 copy pop
  y1 y2 copy pop

  {~[exch x1 length range x1 3 1 roll getinterval {
    0 x3 petsc_vec_copyto pop
  } ~exec]} ~execpawns
  {~[exch y1 length range y1 3 1 roll getinterval {
    0 y3 petsc_vec_copyto pop
  } ~exec]} ~execpawns

  exec
} bind def

/tests [
  [(identity) ~identity_tests]
  [(triagonal_nu) ~triagonal_nu_tests]
  [(triagonal_u) ~triagonal_u_tests]
  [(full) ~full_tests]
] def


/lsymmetric {
  /symmetric true def
  /m 5 def
  /n ~m def
  /k ~m def
} bind def

/lassymmetric {
  /symmetric false def
  /m 5 def
  /n 7 def
  /k 11 def
} bind def

/bsymmetric {
  /symmetric true def
  /m 1000 def
  /n ~m def
  /k ~m def
} bind def

/bassymmetric {
  /symmetric false def
  /m 1000 def
  /n 1007 def
  /k 991 def
} bind def

/test {
  {
    {(little symm) lsymmetric}
    {(little asymm) lassymmetric}
    {(big symm) bsymmetric}
    {(big asymm) bassymmetric}
  } {
    (Starting mode: ) toconsole 
    exec toconsole (\n) toconsole
    
    currentdict /A3 known {A3 mat_destroy} if
    currentdict /x3 known {x3 vec_destroy} if
    currentdict /y3 known {y3 vec_destroy} if
    currentdict /ksp known {ksp ksp_destroy} if

    /matrix_test_ layer {
      {
        /matrix_test_ layer
        PETSC begin
        100 dict dup /matrix_test name begin
      } sexecpawns

      /A1 m n mul /d array def
      /A_cuts m n 1 3 cutsn def
      /A1_old [A1 m {n /d parcel exch} repeat pop] def
      
      /A1t n m mul /d array def
      /At_cuts n m 1 3 cutsn def
      /A1t_old [A1t n {m /d parcel exch} repeat pop] def
      
      /B1 n k mul /d array def
      /B_cuts n k 1 3 cutsn def
      /B1_old [B1 n {k /d parcel exch} repeat pop] def
      
      /B1t k n mul /d array def
      /Bt_cuts k n 1 3 cutsn def
      /B1t_old [B1t k {n /d parcel exch} repeat pop] def
      
      /C_temp m k mul /d array def
      /Cold_temp [C_temp m {k /d parcel exch} repeat pop] def
      
      /C1 m k mul /d array def
      /C_cuts m k 1 3 cutsn def
      /C1_old [C1 m {k /d parcel exch} repeat pop] def
      
      /C1t k m mul /d array def
      /Ct_cuts k m 1 3 cutsn def
      /C1t_old [C1t k {m /d parcel exch} repeat pop] def

      /piv1 n /l array def
      /x_temp n /d array def
      /x1 n /d array def
      /y_temp m /d array def
      /y1 m /d array def
    
      /A2 m n mul /d array def
      /A2_old [A2 m {n /d parcel exch} repeat pop] def
      
      /A2t n m mul /d array def
      /A2t_old [A2t n {m /d parcel exch} repeat pop] def
      
      /B2 n k mul /d array def
      /B2_old [B2 n {k /d parcel exch} repeat pop] def
      
      /B2t k n mul /d array def
      /B2t_old [B2t k {n /d parcel exch} repeat pop] def
      
      /C2 m k mul /d array def
      /C2_old [C2 m {k /d parcel exch} repeat pop] def
      
      /C2t k m mul /d array def
      /C2t_old [C2t k {m /d parcel exch} repeat pop] def

      /piv2 n /x array def
      /x2 n /d array def
      /y2 m /d array def

      /x3_data n /d array def
      /x3 dup n vec_create def

      /y3_data m /d array def
      /y3 dup m vec_create def

      tests {
        dup 0 get (Starting test type: ) toconsole toconsole (\n) toconsole
        dup 1 get exec 0 get done
      } forall

      {
        end
        end
        false /matrix_test_ _layer pop
      } sexecpawns
    } stopped /matrix_test_ _layer {stop} if
  } forall
} bind def

end _module