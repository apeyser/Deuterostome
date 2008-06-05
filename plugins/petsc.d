/PETSC module 200 dict dup begin

/ksptypes {
  /KSPRICHARDSON
  /KSPCHEBYCHEV
  /KSPCG
  /KSPCGNE
  /KSPSTCG
  /KSPGLTR
  /KSPGMRES
  /KSPFGMRES
  /KSPLGMRES
  /KSPTCQMR
  /KSPBCGS
  /KSPBCGSL
  /KSPCGS
  /KSPTFQMR
  /KSPCR
  /KSPLSQR
  /KSPPREONLY
  /KSPQCG
  /KSPBICG
  /KSPMINRES
  /KSPSYMMLQ
  /KSPLCD
} makeenum def

/pctypes {
  /PCNONE
  /PCJACOBI
  /PCSOR
  /PCLU
  /PCSHELL
  /PCBJACOBI
  /PCMG
  /PCEISENSTAT
  /PCILU
  /PCICC
  /PCASM
  /PCKSP
  /PCCOMPOSITE
  /PCREDUNDANT
  /PCSPAI
  /PCNN
  /PCCHOLESKY
  /PCSAMG
  /PCPBJACOBI
  /PCMAT
  /PCHYPRE
  /PCFIELDSPLIT
  /PCTFS
  /PCML
  /PCPROMETHEUS
  /PCGALERKIN
  /PCOPENMP
} makeenum def

| ~act boolean-in-layer | --
/in_petsc {
  PETSC begin {
    {
      /PETSC_ layer stopped /PETSC_ _layer {stop} if
    } {exec} ifelse
  } stopped end {stop} if
} bind def

dm_type /dpawn eq {
  getplugindir (dmpetsc.la) loadlib /petsc_oplib name
  
  | /x <d ...> | --
  /vec_create {
    {
      0 1 index length petsc_vec_create petsc_vec_copyto | /x x
    } false in_petsc def
  } bind def
  
  | /A <l irows> <l icols> <d data> nmax | --
  /mat_create {
    {
      /nmax name /data name 1 index /irows name
      petsc_mat_create dup /A name
      0 1 A /MATRIX_M get 1 sub {/i name
        data irows i get irows i 1 add get 1 index sub getinterval
        i 0 A petsc_mat_copyto pop
      } for
      nmax 1 index /MATRIX_M get sub {petsc_mat_syncto} repeat
    } false in_petsc def
  } bind def

  | A x | --
  /pmatvecmul {
    {petsc_mat_vecmul} true in_petsc
  } bind def
  
  | A x | -- (Ax on dnode)
  /get_matvecmul {
    dup 3 1 roll pmatvecmul get_vector
  } bind def
  
  | x | --
  /get_vector {
    {
      0 1 index /VECTOR_N get /d array petsc_vec_copyfrom
      ~[exch mpirank ~recv_vector_result] rsend
    } true in_petsc
  } bind def
  
  | A nmax | --
  /get_matrix {
    {
      exch
      dup /A name dup /MATRIX_N get /d array /t name
      0 1 A /MATRIX_M get {/i name
        A i A /MATRIX_N get t petsc_mat_copyfrom
        ~[exch i mpirank ~recv_matrix_result] rsend
      } for
      nmax 1 index /MATRIX_M get sub {petsc_mat_syncfrom} repeat pop
    } true in_petsc
  } bind def

  | /ksp kspsettings | --
  /ksp_create {
    {
      dup /kspsettings name
      begin {
        ksptype kspparam pctype pcparam petsc_ksp_create
        dup rtol atol dtol maxits petsc_ksp_tol
      } stopped end {stop} if
    } false in_petsc def
  } bind def

  | x | --
  /vec_destroy {
    {petsc_vec_destroy} true in_petsc
  } bind def
  
  | A | -- 
  /mat_destroy {
    {petsc_mat_destroy} true in_petsc
  } bind def
  
  | ksp | --
  /ksp_destroy {
    {petsc_ksp_destroy} true in_petsc
  } bind def
  
  /report true def
  /repbuf 255 /b array def
  
  | ksp A/null x b | --
  /ksp_solve {
    {3 index /ksp_ name
|       4 copy /b_ name /x_ name /A_ name /ksp_ name {
|         (A: ) toconsole A_ _ pop
|         A_ null ne {
|           0 1 A_ /MATRIX_M get 1 sub {/m_ name
|             (Row:       ) 5 * m_ A_ /MATRIX_GM get add * number 
|                             (:\n) fax 0 exch getinterval toconsole
|             A_ m_ 0 A_ /MATRIX_N get /d array petsc_mat_copyfrom v_ pop
|           } for
|         } if
|         (b: ) toconsole b_ _ pop
|         (Start: ) toconsole b_ /VECTOR_GN get _ pop
|         b_ 0 b_ /VECTOR_N get /d array petsc_vec_copyfrom v_ pop
|         (x: ) toconsole x_ _ pop
|         (Start: ) toconsole x_ /VECTOR_GN get _ pop
|         x_ 0 x_ /VECTOR_N get /d array petsc_vec_copyfrom v_ pop
|         (k: ) toconsole ksp_ _ pop
|       } groupconsole
      petsc_ksp_solve pop
|       {
|         (Solved\n) toconsole
|         (b: ) toconsole b_ _ pop
|         (Start: ) toconsole b_ /VECTOR_GN get _ pop
|         b_ 0 b_ /VECTOR_N get /d array petsc_vec_copyfrom v_ pop
|         (x: ) toconsole x_ _ pop
|         (Start: ) toconsole x_ /VECTOR_GN get _ pop
|         x_ 0 x_ /VECTOR_N get /d array petsc_vec_copyfrom v_ pop
|       } groupconsole
      mpirank 0 eq {
        report {
          repbuf 0 (Convergence iterations: ) fax 
                   * ksp_ petsc_ksp_iterations * number
                   (\n) fax
          0 exch getinterval toconsole
        } if
      } if
|      mpibarrier
    } true in_petsc
  } bind def

  | ksp A/null x b | --
  /get_ksp_solve {
    1 index 5 1 roll ksp_solve get_vector
  } bind def
} {
  | pawnnum elements | offset length
  /range {
    mpidata /pawns get      | p# es ps
    3 copy div mul 4 1 roll | off p# es ps
    
    3 copy div exch         | off p# es ps len p#
    2 index 1 sub eq {      | off p# es ps len
      3 copy pop mod add    | off p# es ps len
    } if
    4 1 roll pop pop pop    | off len
  } bind def

  | dict | ~id
  /getid {/id get mkact} bind def
  
  | dict | <d data>
  /getdata {/data get} bind def
  
  | /x <d ...> | xdict
  /vec_create {
    2 dict dup begin {
      exch /data name exch /id name
      {
        {~[id data 4 -1 roll data length range getinterval ~vec_create]} 
        waitforpawns
      } true in_petsc
    } stopped end {stop} if
  } bind def

  | /A <l irows> <l icols> <d data> | Adict
  /mat_create {
    5 dict dup begin |[
      exch /data name
      exch /icols name
      exch /irows name
      exch /id name |]
    end dup {dup begin PETSC begin {
      /irows_ irows length /l array def
      mpidata /pawns get 1 sub irows length 1 sub range exch pop 
      exch /nmax put
      {/i name
        i irows length 1 sub range /nl name /n0 name
        ~[
          id
          irows  n0 nl 1 add getinterval  
          irows_ n0 nl 1 add getinterval copy irows n0 get sub
          icols irows n0 get irows n0 nl add get 1 index sub getinterval
          data  irows n0 get irows n0 nl add get 1 index sub getinterval
          nmax
          ~mat_create
        ]
      } waitforpawns
    } stopped end end {stop} if} true in_petsc
  } bind def

  | {} | --
  /waitsimple {
    /cfunc name {pop /cfunc find} waitforpawns
  } bind def

  | A x | --
  /pmatvecmul {
    {
      2 {getid exch} repeat
      ~[3 1 roll ~pmatvecmul] waitsimple
    } true in_petsc
  } bind def
  
  | x | --
  /vector_result {
    {/xval name} false in_petsc
  } bind def
  
  | A | --
  /matrix_result {
    {/Aval name} false in_petsc
  } bind def
  
  | <d sub-vec> mpi | --
  /recv_vector_result {
    {
      xval getdata exch 1 index length range getinterval copy pop 
      restore
    } lock
  } bind def
  
  | <d sub-vec> row mpi | --
  /recv_matrix_result {
    {
      Aval /irows get length 1 sub range pop add /nr name
      Aval getdata 
      Aval /irows get dup nr get exch nr 1 add get getinterval 
      copy pop
      restore
    } lock
  } bind def
  
  | x | x
  /get_vector {
    {
      dup vector_result
      ~[1 index getid ~get_vector] waitsimple
    } true in_petsc
  } bind def
  
  | A | A
  /get_matrix {
    {
      dup matrix_result
      ~[1 index getid 2 index /nmax get ~get_matrix] waitsimple
    } true in_petsc
  } bind def
  
  | A x | x
  /get_matvecmul {
    {
      dup vector_result
      ~[3 -1 roll getid 2 index getid ~get_matvecmul] waitsimple
    } true in_petsc
  } bind def
  
  /kspsettings {
    /rtol     1e-12
    /atol     *
    /dtol     {1d kspsettings /rtol get div}
    /maxits   *
    /pctype   *
    /ksptype  *
    /kspparam null
    /pcparam  null
  } makestruct def
  
  | /ksp | kspsettings
  /ksp_create {
    kspsettings dup used 1 add dict exch {merge
      2 copy /id put
      ~[3 -1 roll 2 index ~ksp_create] waitsimple
    } true in_petsc
  } bind def
  
  | x | --
  /vec_destroy {
    {
      ~[exch getid ~vec_destroy] waitsimple
    } true in_petsc
  } bind def
  
  | A | --
  /mat_destroy {
    {
      ~[exch getid ~mat_destroy] waitsimple
    } true in_petsc
  } bind def
  
  | ksp | --
  /ksp_destroy {
    {
      ~[exch getid ~ksp_destroy] waitsimple
    } true in_petsc
  } bind def

  | ksp A x b | --
  /ksp_solve {
    {
      ~[5 1 roll 4 {getid 4 1 roll} repeat ~ksp_solve] waitsimple
    } true in_petsc
  } bind def
  
  | ksp x b | --
  /ksp_resolve {
    {
      ~[4 1 roll 3 {getid 3 1 roll} repeat null 3 1 roll ~ksp_solve] waitsimple
    } true in_petsc
  } bind def

  | ksp A x b | x
  /get_ksp_solve {
    {
      1 index vector_result
      1 index 5 1 roll
      ~[5 1 roll 4 {getid 4 1 roll} repeat ~get_ksp_solve] waitsimple
    } true in_petsc
  } bind def

  | ksp x b | x
  /get_ksp_resolve {
    {
      1 index vector_result
      1 index 4 1 roll
      ~[4 1 roll 3 {getid 3 1 roll} repeat null 3 1 roll ~get_ksp_solve] waitsimple
    } true in_petsc
  } bind def
  
  | bool | --
  /report {
    0 ~[3 -1 roll {{/report name} true in_petsc restore} ~lock] rsend
  } bind def
  
  100 dict /petsc_tester name
  /petsc_test {
    /petsc_tester_ layer petsc_tester begin  {
      {
        /petsc_tester_ layer 
        100 dict dup /petsc_tester name begin
      } waitsimple
      
      /vecx dup <d 3 3 3 3 3> vec_create def
      /matA dup <l 0 5 10 15 20 25> 
        <l 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4>
        <d 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 0 0 0 1 1 0 0 0 0 1>
        mat_create def
      /k dup ksp_create def

      /vecb dup <d 5 4 3 2 1> vec_create def
      (Solution: \n) toconsole
      k matA vecx vecb get_ksp_solve getdata v_ pop
      
      /vecb2 dup <d 10 8 6 4 2> vec_create def
      (Solution x2: \n) toconsole
      k vecx vecb2 get_ksp_resolve getdata v_ pop
      
      vecx  vec_destroy
      vecb  vec_destroy
      vecb2 vec_destroy
      matA  mat_destroy
      k     ksp_destroy
      
      {
        end
        false /petsc_tester_ _layer pop
      } waitsimple
    } stopped end /petsc_tester_ _layer {stop} if
  } bind def

| -- | --
| /petsc_su {
|   /PETSC_common module_send
|   /PETSC_dpawn module_send
|   * {dup capsave PETSC_dpawn /init get exec PETSC begin restore} rsend
| } bind def

| /trace {
|   countexecstack list execstack dup length 1 sub 0 exch getinterval
|   (extend:\n) toconsole
|   reverse v_ {
|     dup class /listclass eq ~v_ ~_ ifelse pop (\n) toconsole
|   } forall
|   (\n) toconsole
| } bind def

| /layer {
|   (layer: ) toconsole _ trace
|   dup currentdict exch known { 
|       dup find dup class /boxclass eq {restore} {pop} ifelse
|   } if
|   save def
| } bind def

| /_layer {
|   (end layer: ) toconsole _ trace
|   find exch {restore true} {capsave false} ifelse 
| } bind def 
} ifelse

end _module

