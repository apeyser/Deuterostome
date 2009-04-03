/MATRIX_TEST module 200 dict dup begin

/m 1000 def

/mlittle 5 def
/nlittle 7 def
/klittle 11 def

/mbig 1000 def
/nbig 1007 def
/kbig 991 def

/mrbig 4500 def

/n ~m def
/k ~m def

/ksptype /DEFAULT def
/sparse_ksptype {/DEFAULT} def
/dense_ksptype {/GMRES} def
|/dense_ksptype {/DEFAULT} def

/pctype /DEFAULT def
/sparse_pctype {/DEFAULT} def
/dense_pctype {/BJACOBI_LU} def

/mtypes [
  /default {
    /ksptype ksptype
    /pctype pctype
  } makestruct
  /sparse {
    /ksptype sparse_ksptype
    /pctype sparse_pctype
  } makestruct
  /dense {
    /ksptype dense_ksptype
    /pctype dense_pctype
  } makestruct
] makestruct def

/makeksp {/mtype_ name
  pctypes  mtypes mtype_ get /pctype  get exec get kspsettings /pctype  put
  ksptypes mtypes mtype_ get /ksptype get exec get kspsettings /ksptype put
  ksp_create
} bind def

/done {(Finished test: ) toconsole toconsole (\n) toconsole} bind def
/test_eps 1e-12 def
/report_iterations false def

/halt_on_error true def
/halt {halt_on_error ~halt if} bind def

/compare {/temp name /a name /b name /cs name
  0d a temp copy b sub dup mul add sqrt dup
  0d b temp copy dup mul add sqrt div dup * eq ~pop {exch pop} ifelse
  dup * eq {(Test error undefined: ) toconsole cs toconsole _ halt} {
    dup test_eps gt {(Test error: ) toconsole cs toconsole _ halt} if
  } ifelse
  pop
} bind def

/cmpstring 1024 /b array def
/compare3 {/temp name /oldop name /newop name /petop name /cmps name
  cmpstring 0 cmps fax (:old-new) fax 0 exch getinterval
    oldop newop temp compare
  cmpstring 0 cmps fax (:old-pet) fax 0 exch getinterval
    oldop petop temp compare
  cmpstring 0 cmps fax (:new-pet) fax 0 exch getinterval
    newop petop temp compare
} bind def

/cmpstring 1024 /b array def
/compare4 {/temp name /oldop name /newop name /petop name /petop2 name 
  /cmps name
  cmpstring 0 cmps fax (:old-new) fax 0 exch getinterval
    oldop newop temp compare
  cmpstring 0 cmps fax (:old-pet) fax 0 exch getinterval
    oldop petop temp compare
  cmpstring 0 cmps fax (:new-pet) fax 0 exch getinterval
    newop petop temp compare
  cmpstring 0 cmps fax (:old-pet2) fax 0 exch getinterval
    oldop petop2 temp compare
  cmpstring 0 cmps fax (:new-pet2) fax 0 exch getinterval
    newop petop2 temp compare
  cmpstring 0 cmps fax (:pet-pet2) fax 0 exch getinterval
    petop petop2 temp compare
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
  x4 x4_data get_vector x3 x3_data get_vector x1 x2 x_temp compare4
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
  ksp A3 x3 y3 ksp_solve
} bind def

/ksp2_test {
  ksp2 A4 x4 y4 ksp_solve
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

/matvecmulp_test {/beta name /alpha name false settrans
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

/base_tests_do [
  /matmul true
  /luinv true
  /matvecmul true
  /trisolve true
  /lu true
  /gmres false
] makestruct def

| [ [[test1 test2 comparator] [(name) params...]]... ]
/base_tests [
  /matmul
  [[~matmul_test ~matmulold_test ] ~matmul_cmp ~do_full [
    [(01nn) false false 0 1]
    [(10nn) false false 1 0]
    [(02nn) false false 0 2]
    [(11nn) false false 1 1]
    [(12nn) false false 1 2]
    [(01nt) false true 0 1]
    [(10nt) false true 1 0]
    [(02nt) false true 0 2]
    [(11nt) false true 1 1]
    [(12nt) false true 1 2]
    [(01tn) true false 0 1]
    [(10tn) true false 1 0]
    [(02tn) true false 0 2]
    [(11tn) true false 1 1]
    [(12tn) true false 1 2]
    [(01nt) true true 0 1]
    [(10nt) true true 1 0]
    [(02nt) true true 0 2]
    [(11nt) true true 1 1]
    [(12nt) true true 1 2]
  ]]

  /luinv
  [[~luinv_test ~luinvold_test] ~luinv_cmp 
    {symmetric do_full and} [
    [()]
  ]]

  /matvecmul
  [[~matvecmul_test ~matvecmulold_test ~matvecmulp_test] 
    ~matvecmul_cmp ~do_full [
    [(01n) false 0 1]
    [(01t) true 0 1]
    [(10n) false 1 0]
    [(10t) true 1 0]
    [(11n) false 1 1]
    [(11t) true 1 1]
    [(02n) false 0 2]
    [(02t) true 0 2]
    [(12n) false 1 2]
    [(12t) true 1 2]
  ]]

  /trisolve
  [[~trisolve_test ~trisolveold_test] ~trisolve_cmp
    {triagonal do_full and symmetric and} [
    [(n) false]
    [(t) true]
  ]]

  /lu
  [[~lu_test ~luold_test ~ksp_test ~ksp2_test] ~gmres_cmp 
    ~symmetric [
    [()]
  ]]

  /gmres
  [[~gmres_test ~luold_test ~ksp_test]
    ~gmres_cmp {symmetric do_full and} [
    [()]
  ]]
] makestruct def

| (name) ~active:--|-- | --
/timer {
  gettimeofday 3 -1 roll exec 
  gettimeofday timediff neg
  exch toconsole (: ) toconsole _ pop
} bind def

/timediff {
  4 1 roll exch 4 1 roll
  sub 3 1 roll sub 1e-6 exch mul exch add
} bind def

/run_tests_buf 80 /b array def
/run_tests {
  base_tests {/itest name /itestdo name
    base_tests_do itestdo get {
      /testsn itest 0 get def
      /testcmp itest 1 get def
      /conds itest 2 get def

      itest 3 get {/ktest name
        dup propagate
        conds {
          /itestnm run_tests_buf 0 
            * itestdo mkact text ktest 0 get fax 
            0 exch getinterval def
          (Starting: ) toconsole itestnm toconsole (\n) toconsole
          /ntestsn 1 def
          testsn {/ctest name
            (Starting test#) toconsole ntestsn _ pop          
            (Time) ~[
              ktest 1 ktest length 1 sub getinterval {} forall
              ~ctest
            ] timer
            /ntestsn ntestsn 1 add def
          } forall
          itestnm testcmp 
          itestnm done
        } if
      } forall
    } if
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

  /ksp  dup /default makeksp def
  /ksp2 dup mtype    makeksp def
} bind def
 
/identity_tests {
  {
    /mtype /sparse def
    base_setup

    0 A1 copy pop
    0 1 m 1 sub {/i name
      1 A1 A_cuts i cut pop i n mod put
    } for

    m ~[m n {/n name /m name /nl name /n0 name
      /matArows nl 1 add /l array 
        0 nl 1 add 0 1 ramp pop
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
    /A3t dup A3 mat_dup def
    ~matAtrows ~matAtcols A3t mat_transpose

    {}
 } run_tests
} bind def

/triagonal_nu_tests {
  {
    /mtype /sparse def
    base_setup
    triagonal_setup
  } run_tests
} bind def

/triagonal_u_tests {
  {
    /mtype /sparse def
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
    |{(matArows) toconsole mpirank _ pop} groupconsole
    /matArows nl 1 add /l array def
    0        matArows 0 put
    m n0 sub matArows 1 put
    2 1 matArows last {/row name | x_i = x_{i-1} + (x_{i-1} - x_{i-2}) - 1
      matArows row 1 sub get
      dup matArows row 2 sub get sub add
      1 sub matArows row put
    } for

    |{(matAcols) toconsole mpirank _ pop} groupconsole
    /matAcols matArows dup last get /l array def
    matAcols 0
    1 1 matArows last {
      matArows exch get 1 index sub
      m 1 index sub
      1 ramp
    } for pop pop

    |{(matAdata) toconsole mpirank _ pop} groupconsole
    /matAdata matAcols length /d array def
    0 1 matArows last 1 sub {/row name
      matAdata
      matArows row get matArows row 1 add get 1 index sub
      getinterval
      0 1 2 index last {/i name
        /c matAcols matArows row get i add get def
        triagonal_u {1} {c 1 add} ifelse
        0.5 c n0 row add sub pwr exch mul
        1 index i put 
      } for
      pop
    } for
  } ~exec] execrange

  |(A3 create\n) toconsole
  /A3 dup ~matArows ~matAcols /sparse m n mat_create def
  |(A3 fill\n) toconsole
  A3 ~matAdata mat_fill_data
  |(A3 dup\n) toconsole
  /A3t dup A3 mat_dup def
  |(A3t transpose\n) toconsole
  ~matAtrows ~matAtcols A3t mat_transpose
  |(A3t transpose end\n) toconsole

  {}
} bind def

/full_model (s4split_shift_35_1) def

/full_on false def

/do_full {full_on full not or} def

/full_tests {
  {
    /mtype /dense def
    base_setup

    full_on {
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
      
      GAT /MODEL get /myName get forgetmodule
    } if

    /A3 dup /dense m n mat_create def
    {~[
      exch m range /nl name /n0 name
      A1 n0 n mul nl n mul getinterval {
        dup length /d array copy /matAdata name
      } ~exec
    ]} execpawns
    A3 ~matAdata mat_fill_data
    /A3t dup A3 mat_dup def
    A3t mat_transpose

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
  } ~exec]} execpawns
  {~[exch y1 length range y1 3 1 roll getinterval {
    0 y3 petsc_vec_copyto pop
  } ~exec]} execpawns

  /x4 dup x3 vec_dup def
  /y4 dup y3 vec_dup def
  /A4 dup A3 mat_dup def
  /A4t dup A3t mat_dup def

  exec
} bind def

/tests_name {
  /id (identity)
  /trinu (triagonal_nu)
  /triu (triagonal_u)
  /full (full)
} makestruct def

/tests_func {
  /id identity_tests
  /trinu triagonal_nu_tests
  /triu triagonal_u_tests
  /full full_tests
} bind makestruct def

/tests_do [
  /id true 
  /trinu true
  /triu true
  /full true
] makestruct def

/modes_do [
  /lsymmetric true
  /lasymmetric true
  /bsymmetric true
  /basymmetric true
  /bbsymmetric true
] makestruct def

/modes_su {
  /lsymmetric {
    /symmetric true def
    /m ~mlittle def
    /n ~m def
    /k ~m def
  }

  /lasymmetric {
    /symmetric false def
    /m ~mlittle def
    /n ~nlittle def
    /k ~klittle def
  }

  /bsymmetric {
    /symmetric true def
    /m ~mbig def
    /n ~m def
    /k ~m def
  }

  /basymmetric {
    /symmetric false def
    /m ~mbig def
    /n ~nbig def
    /k ~kbig def
  }

  /bbsymmetric {
    /symmetric true def
    /m ~mrbig def
    /n ~m def
    /k ~m def
  }
} bind makestruct def

/modes_name {
  /lsymmetric (little symm)
  /lasymmetric (little asym)
  /bsymmetric (big symm)
  /basymmetric (big asymm)
  /bbsymmetric (really big symm)
} makestruct def

| /name ~destroy | --
/pdestroy {
  exch currentdict 1 index known not {pop pop} {find exch exec} ifelse
} bind def

/test {
  {
    /lsymmetric
    /lasymmetric
    /bsymmetric
    /basymmetric
    /bbsymmetric
  } {
    modes_do 1 index get not ~pop {
      modes_su   1 index   get
      modes_name 3 -1 roll get

      /matrix_test_ layer currentdict PETSC begin begin {
        (Starting mode: ) toconsole toconsole (\n) toconsole
        exec
      
        {/A3 /A4 /A3t /A4t} {~mat_destroy pdestroy} forall
        {/x3 /y3 /x4 /y4} {~vec_destroy pdestroy} forall
        {/ksp /ksp2} {~ksp_destroy pdestroy} forall
        
      
        ~[report_iterations {
          /matrix_test_ layer
          PETSC begin
          /report_ report def
          /report name
          100 dict dup /matrix_test name begin kickdnode end
          /report report_ def
          end
          /matrix_test_ _layer ~stop if
        } ~exec] sexecpawns

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
        /x4_data n /d array def

        /y3_data m /d array def
        /y3 dup m vec_create def
        
        tests_do {
          not ~pop {
            tests_name 1 index get /tests_name_n name
            (Starting test type: ) toconsole 
            tests_name_n toconsole 
            (\n) toconsole
            
            tests_func exch get exec 
            tests_name_n done
          } ifelse
        } forall
        
        kickpawns
      } stopped end end /matrix_test_ _layer {stop} if
    } ifelse
  } forall
} bind def

end _module