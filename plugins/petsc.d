/PETSC module 200 dict dup begin

|============== Global definitions ==========================

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

| ~act | --
/in_petsc {
  PETSC begin stopped end {stop} if
} bind def

|================= dpawn definitions ===========================
/dpawn {
  getplugindir (dmpetsc.la) loadlib /petsc_oplib name
  
  | /x n | --
  /vec_create {
    {petsc_vec_create} in_petsc def
  } bind def

  | x <d > | --
  /vec_fill {
    0 3 -1 roll petsc_vec_copyto pop
  } bind def

  /mat_creators {
    | n <l irows> <l icols> | A
    /sparse {
      3 -1 roll petsc_mat_sparse_create
    }

    | m n | A
    /dense ~petsc_mat_dense_create
  } bind makestruct def

  | /A .... /type | --
  /mat_create {
    {mat_creators exch get exec} in_petsc def
  } bind def

  | ... | --
  /mat_fillers_set {
    | irows | --
    /sparse {/irows name}
    | N | --
    /dense {/N name}
  } bind makestruct def

  /mat_fillers_get [
    | <d data> | <d row>
    /sparse {
      irows row get 
      dup irows 0 get sub 
      irows row 1 add get 3 -1 roll sub 
      getinterval
    } bind

    | <d data> | <d row>
    /dense {
      row N mul N getinterval
    } bind
  ] def
  
  | A data mmax ... /type | --
  /mat_fill {
    {
      /mtype name
      mat_fillers_set mtype get exec
      /mmax name /data name /A name
      mat_fillers_get mtype get /filler name

      0 1 A /MATRIX_M get 1 sub {/row name
        data filler row 0 A petsc_mat_copyto pop
      } for
      mmax A /MATRIX_M get sub {petsc_mat_syncto} repeat
    } in_petsc
  } bind def

  | A x | --
  /pmatvecmul {
    {petsc_mat_vecmul} in_petsc
  } bind def
  
  | A x | -- (Ax on dnode)
  /get_matvecmul {
    dup 3 1 roll pmatvecmul get_vector
  } bind def
  
  | x | --
  /get_vector {
    {
      dup 0 1 index /VECTOR_N get /d array petsc_vec_copyfrom
      ~[3 1 roll exch /VECTOR_GN get ~recv_vector_result] rsend
    } in_petsc
  } bind def

  | ... | --
  /matrixers_set {
    | irows | --
    /sparse {/irows name}
    | -- | --
    /dense  {}
  } bind makestruct def

  | A | global_interval_start
  /matrixers_get {
    /sparse {
      pop irows row get
    }
    /dense {
      /MATRIX_GM get row add N mul
    }
  } bind makestruct def
  
  | A N mmax ... /type | --
  /get_matrix {
    {
      /mtype name
      matrixers_set mtype get exec
      /mmax name /N name
      dup /A name 
      N /d array /t name
      0 1 A /MATRIX_M get {/row name
        A row 0 t petsc_mat_copyfrom
        ~[
          exch 
          A matrixers_get mtype get exec 
          ~recv_matrix_result
        ] rsend
      } for
      mmax 1 index /MATRIX_M get sub {petsc_mat_syncfrom} repeat pop
    } in_petsc
  } bind def

  | /ksp kspsettings | --
  /ksp_create {
    {
      dup /kspsettings name
      begin {
        ksptype kspparam pctype pcparam petsc_ksp_create
        dup rtol atol dtol maxits petsc_ksp_tol
      } stopped end {stop} if
    } in_petsc def
  } bind def

  | x | --
  /vec_destroy {
    {petsc_vec_destroy} in_petsc
  } bind def
  
  | A | -- 
  /mat_destroy {
    {petsc_mat_destroy} in_petsc
  } bind def
  
  | ksp | --
  /ksp_destroy {
    {petsc_ksp_destroy} in_petsc
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
    } in_petsc
  } bind def

  | ksp A/null x b | --
  /get_ksp_solve {
    1 index 5 1 roll ksp_solve get_vector
  } bind def
} def

|=================== dnode definitions ======================
/dnode {
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

  | pawnnum elements | length
  /rangesize {range exch pop} bind def

  | pawnnum elements | offset
  /rangestart {range pop} bind def

  | obj-dict | ~id
  /getid {/id get mkact} bind def
  | funcdict matrix-dict | ~func
  /exectype {/mtype get get exec} bind def
  | matrix-dict | mtype
  /gettype {/mtype get} bind def
  
  | /x N | xdict
  /vec_create {
    {
      2 dict dup begin /N name /id name end dup /xval name
      {~[
        xval /id get
        3 -1 roll xval /N get rangesize 
        ~vec_create
      ]} execpawns
    } in_petsc
  } bind def

  | xval ~data | --
  /vec_fill {
    {
      ~[3 -1 roll getid 3 -1 roll construct_exec ~vec_fill] sexecpawns
    } in_petsc
  } bind def

  | ~irows ~icols | --
  /condense_sparse {
    {
      /icols name /irows name
      /icols_len mpi /pawns get list def
      ~[
        /icols construct_execn
        {~[exch length mpirank ~condense_recv] rsend} ~in_petsc
      ] sexecpawns

      1 1 icols_len last {
        icols_len 1 index 1 sub get icols_len 2 index get add
        icols_len 3 -1 roll put
      } for
      
      {
        dup 0 eq {pop null} {
          ~[
            icols_len 3 -1 roll 1 sub get 
            /irows construct_execn {
              exch add pop
            } ~exec
          ]
        } ifelse
      } execpawns
    } in_petsc
  } bind def

  | save icols_length pawn-num | --
  /condense_recv {
    {
      icols_len exch put
      restore
    } lock
  } bind def

  | pawn | sub-params...
  /mat_creators_get {
    | pawn | n ~sub-irows ~sub-icols
    /sparse {
      Aval /n get rangesize
      Aval /params get /icols get construct_exec
      Aval /params get /irows get construct_exec
    }

    | pawn | m n
    /dense {
      dup  m rangesize
      exch n rangesize
    }
  } bind makestruct def

  | ... | param-dict
  /mat_creators_set {
    | ~irows ~icols | param-dict
    /sparse {
      2 copy condense_sparse
      2 dict dup begin {
        /icols name
        /irows name
      } stopped end {stop} if
    }

    | -- | param-dictdict
    /dense {0 dict}
  } bind makestruct def

  | /A .... /type m n | Adict
  | on dpawn: <d data>
  /mat_create {
    {
      /Aval 6 dict def
      Aval /n       put
      Aval /m       put
      Aval /mtype   put mat_creators_set Aval exectype
      Aval /params  put mpidata /pawns get 1 sub exch range 
      Aval /mmax    put pop
      Aval /id      put
      {~[
        Aval /id get
        3 -1 roll mat_creators_get Aval exectype
        Aval gettype 
        ~mat_create
      ]} execpawns
      Aval
    } in_petsc
  } bind def

  | -- | ...
  /mat_fillers {
    | -- | irows
    /sparse {
      Aval /params get /irows get construct_exec
    }

    | -- | N
    /dense {Aval /n get}
  } bind makestruct def

  | A ~data | --
  /mat_fill {
    {
      /data name /Aval name
      ~[
        Aval getid
        /data construct_execn
        Aval /mmax get
        mat_fillers Aval exectype
        Aval gettype 
        ~mat_fill
      ] sexecpawns
    } in_petsc
  } bind def

  | A x | --
  /pmatvecmul {
    {
      2 {getid exch} repeat
      ~[3 1 roll ~pmatvecmul] sexecpawns
    } in_petsc
  } bind def
  
  | x <d data> | --
  /vector_result {
    {/data name /xval name} in_petsc
  } bind def
  
  | A <d data> | --
  /matrix_result {
    {/data name /Aval name} in_petsc
  } bind def

  | save <d sub-vec> interval_start | --
  /recv_vector_result {
    {
      data exch 2 index length getinterval copy pop
      restore
    } lock
  } bind def
    
  | save <d sub-vec> interval_start | --
  /recv_matrix_result {
    {
      data exch 2 index length getinterval copy pop
      restore
    } lock
  } bind def
  
  | x <d data> | <d data>
  /get_vector {
    {
      vector_result
      ~[xval getid ~get_vector] sexecpawns
      data
    } in_petsc
  } bind def

  | A | ...
  /matrixers {
    | A | ~irows
    /sparse {
      /params get /irows get construct_exec
    }
    | A | --
    /dense  {pop}
  } bind makestruct def
  
  | A <d data> | <d data>
  /get_matrix {
    {
      matrix_result
      ~[
        Aval getid 
        Aval /n get 
        Aval /mmax get 
        Aval matrixers Aval exectype
        Aval gettype
        ~get_matrix
      ] sexecpawns
      data
    } in_petsc
  } bind def
  
  | A x <d data> | <d data>
  /get_matvecmul {
    {
      vector_result
      ~[exch getid xval getid ~get_matvecmul] sexecpawns
      data
    } in_petsc
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
  } bind makestruct def
  
  | /ksp | kspsettings
  /ksp_create {
    kspsettings dup used 1 add dict exch {merge
      2 copy /id put
      ~[3 -1 roll 2 index ~ksp_create] sexecpawns
    } in_petsc
  } bind def
  
  | x | --
  /vec_destroy {
    {
      ~[exch getid ~vec_destroy] sexecpawns
    } in_petsc
  } bind def
  
  | A | --
  /mat_destroy {
    {
      ~[exch getid ~mat_destroy] sexecpawns
    } in_petsc
  } bind def
  
  | ksp | --
  /ksp_destroy {
    {
      ~[exch getid ~ksp_destroy] sexecpawns
    } in_petsc
  } bind def

  | ksp A x b | --
  /ksp_solve {
    {
      ~[5 1 roll 4 {getid 4 1 roll} repeat ~ksp_solve] sexecpawns
    } in_petsc
  } bind def
  
  | ksp x b | --
  /ksp_resolve {
    {
      ~[
        4 1 roll 3 {getid 3 1 roll} repeat 
        null 
        3 1 roll 
        ~ksp_solve
      ] sexecpawns
    } in_petsc
  } bind def

  | ksp A x b <d data> | <d data>
  /get_ksp_solve {
    {
      2 index exch vector_result
      ~[
        5 1 roll 4 {getid 4 1 roll} repeat 
        ~get_ksp_solve
      ] sexecpawns
      data
    } in_petsc
  } bind def

  | ksp x b <d data> | <d data>
  /get_ksp_resolve {
    {
      2 index exch vector_result
      ~[
        4 1 roll 
        3 {getid 3 1 roll} repeat 
        null 
        3 1 roll 
        ~get_ksp_solve
      ] sexecpawns
    } in_petsc
  } bind def
  
  | bool | --
  /report {
    0 ~[3 -1 roll {{/report name} in_petsc restore} ~lock] rsend
  } bind def

  | length {} | --
  /execrange {
    {/proc name /len name
      {
        ~[exch len range /proc construct_execn]
      } execpawns
    } in_petsc
  } bind def
  
  100 dict /petsc_tester name
  /petsc_test {
    /petsc_tester_ layer petsc_tester begin {
      {
        /petsc_tester_ layer 
        100 dict dup /petsc_tester name begin
      } sexecpawns

      /matD dup /dense 5 dup mat_create def
      {
        0 matD /MATRIX_M get dup mul /d array copy /matDdata name
        0 1 matD /MATRIX_M get 1 sub {/row name
          matDdata row 5 mul 5 getinterval 
          matD /MATRIX_GM row add 5 1 index sub getinterval
          1 exch copy pop
        } for
      } sexecpawns
      matD ~matDdata mat_fill
      
      /vecx dup 5 vec_create def
      {
        5 vecx /VECTOR_N get /d array copy /vecxdata name
      } sexecpawns
      vecx ~vecxdata vec_fill

      5 {/nl name /n0 name
        /matSrows 0 nl 1 add /d array copy def
        1 1 matSrows last {/row name
          5 n0 row add 1 sub sub 
          matSrows row 1 sub get add
          matSrows row put
        } for
        /matScols matSrows last get /d array def
        matScols 0
        1 1 matSrows last {
          matSrows exch get 1 index sub
          5 1 index sub
          1
          ramp 
        } for pop pop
      } execrange
      /matS dup ~matSrows ~matScols /sparse 5 dup mat_create def
      {
        /matSdata 1 matScols length /d array copy def
      } sexecpawns
      matS ~matSdata mat_fill
      
      /kS dup ksp_create def
      /kD dup ksp_create def

      /vecb dup 5 vec_create def
      {
        /vecbdata vecb /VECTOR_N get /d array
          0 vecb /VECTOR_N get 5 vecb /VECTOR_GN get sub -1 ramp pop
        def
      } sexecpawns
      vecb ~vecbdata vec_fill

      /vecb2 dup 5 vec_create def
      {
        /vecb2data vecbdata dup length /d array copy 2 mul def
      } sexecpawns
      vecb2 ~vecb2data vec_fill
      
      /res 5 /d array def
      {matS matD} {/mat name
        (Solution: \n) toconsole
        k mat vecx vecb  res get_ksp_solve   v_ pop
        (Solution x2: \n) toconsole
        k     vecx vecb2 res get_ksp_resolve v_ pop
      } forall
      
      {vecx vecb vecb2} ~vec_destroy forall
      {matS matD}       ~mat_destroy forall
      {kS kD}           ~ksp_destroy forall
      
      {
        end
        false /petsc_tester_ _layer pop
      } sexecpawns
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
} def

| define on basis of node/pawn type
dm_type mkact exec

end _module

