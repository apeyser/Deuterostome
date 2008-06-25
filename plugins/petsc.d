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

| ~act | --
/in_petsc {
  PETSC begin stopped end {stop} if
} bind def

| ~act | --
/out_petsc {
  end stopped PETSC begin {stop} if
} bind def

/dpawn {
  getplugindir (dmpetsc.la) loadlib /petsc_oplib name
  
  | <d ...>  save /x | save
  /vec_create {
    {
      3 -1 roll
      0 1 index length petsc_vec_create petsc_vec_copyto | /x x
    } in_petsc def
  } bind def

|       irows length 1 sub range /nl name /n0 name
|       irows n0 nl 1 add getinterval
|       irows_ n0 nl 1 add getinterval copy irows n0 get sub
|       icols irows n0 get irows n0 nl add get 1 index sub getinterval
|       n
|     {} bind


  /mat_creators [
    | n <l irows> <l icols> | A
    /sparse {
      3 -1 roll 3 copy /N name /icols name /irows name
      irows 0 get dup irows exch sub pop
      petsc_mat_sparse_create
      exch irows exch add pop
    } bind

    | m n | A
    /dense {
      dup /N name 
      petsc_mat_dense_create
    } bind
  ] makestruct def

  /mat_choppers [
    | <d data> | <d row>
    /sparse {
      irows row get dup irows row 1 add get 1 index sub getinterval
    } bind

    | <d data> | <d row>
    /dense {
      row N mul N getinterval
    } bind
  ] def
  
  | data save /A .... /type mmax | save
  /mat_create {
    {
      /mmax name /mtype name
      mat_creator mtype get exec
      dup /A name
      4 -1 roll /data name
      mat_choppers /mtype get /chopper name
      0 1 A /MATRIX_M get 1 sub {/row name
        data chopper row 0 A petsc_mat_copyto pop
      } for
      mmax 1 index /MATRIX_M get sub {petsc_mat_syncto} repeat
    } in_petsc def
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
  /set_matrixers [
    | irows | --
    /sparse {/irows name} bind
    | -- | --
    /dense  {}
  ] makestruct def

  | A | global_interval_start
  /get_matrixers [
    /sparse {
      pop irows row get
    } bind
    /dense {
      /MATRIX_GM get row add N mul
    } bind
  ] makestruct def
  
  | A N mmax ... /type | --
  /get_matrix {
    {
      /mtype name
      set_matrixers mtype get exec
      /mmax name /N name
      dup /A name 
      N /d array /t name
      0 1 A /MATRIX_M get {/row name
        A row 0 t petsc_mat_copyfrom
        ~[
          exch row mpirank A get_matrixers mtype get exec ~recv_matrix_result
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

  | obj-dict | ~id
  /getid {/id get mkact} bind def
  | funcdict matrix-dict | ~func
  /exectype {/mtype get get exec} bind def
  | matrix-dict | mtype
  /gettype {/mtype get} bind def
  
  | /x N | xdict
  | on dpawn: <d data>
  /vec_create {
    {
      2 dict dup begin /N name /id name end dup /xval name
      {~[xval /id get ~vec_create]} execpawns
    } in_petsc
  } bind def

  | pawn | sub-params...
  /mat_creators [
    | pawn | n ~sub-irows ~sub-icols
    /sparse {
      Aval /n get range exch pop
      Aval /params get /icols get dup class /listclass eq {~exec} if
      Aval /params get /irows get dup class /listclass eq {~exec} if
    } bind

    | pawn | m n
    /dense {
      dup  m range exch pop
      exch n range exch pop
    } bind
  ] makestruct def

  | ... | param-dict
  /mat_creators_params [
    | ~irows ~icols | param-dict
    /sparse {
      3 dict dup begin {
        /icols name
        /irows name
      } stopped end {stop} if
    } bind

    | -- | param-dictdict
    /dense {0 dict} bind
  ] makestruct def

  | /A .... /type m n | Adict
  | on dpawn: <d data>
  /mat_create {
    {
                                          /Aval 6 dict def
                                           Aval /n       put
                                           Aval /m       put
                                           Aval /mtype   put
      mat_creators_params Aval exectype    Aval /params  put
      mpidata /pawns get 1 sub exch range  Aval /mmax    put   pop
                                           Aval /id      put
      {~[
        Aval /id get
        3 -1 roll mat_creators Aval exectype
        Aval gettype mmax ~mat_create
      ]} execpawns
      Aval
    } in_petsc
  } bind def

  | A x | --
  /pmatvecmul {
    {
      2 {getid exch} repeat
      ~[3 1 roll ~pmatvecmul] sexecpawn
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

  | <d sub-vec> interval_start | --
  /recv_vector_result {
    {
      data exch 2 index length getinterval copy pop
      restore
    } lock
  } bind def
    
  | <d sub-vec> interval_start | --
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
      ~[xval getid ~get_vector] sexecpawn
      data
    } in_petsc
  } bind def

  | A | ...
  /get_matrixers [
    | A | ~irows
    /sparse {
      /params get /irows get 
        dup class /listclass eq {~exec} if
    } bind
    | A | --
    /dense  {pop}
  ] makestruct def
  
  | A <d data> | <d data>
  /get_matrix {
    {
      matrix_result
      ~[
        Aval getid 
        Aval /n get 
        Aval /mmax get 
        Aval get_matrixers Aval exectype
        Aval gettype
        ~get_matrix
      ] sexecpawn
      data
    } in_petsc
  } bind def
  
  | A x <d data> | <d data>
  /get_matvecmul {
    {
      vector_result
      ~[exch getid xval getid ~get_matvecmul] sexecpawn
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
  } makestruct def
  
  | /ksp | kspsettings
  /ksp_create {
    kspsettings dup used 1 add dict exch {merge
      2 copy /id put
      ~[3 -1 roll 2 index ~ksp_create] sexecpawn
    } in_petsc
  } bind def
  
  | x | --
  /vec_destroy {
    {
      ~[exch getid ~vec_destroy] sexecpawn
    } in_petsc
  } bind def
  
  | A | --
  /mat_destroy {
    {
      ~[exch getid ~mat_destroy] sexecpawn
    } in_petsc
  } bind def
  
  | ksp | --
  /ksp_destroy {
    {
      ~[exch getid ~ksp_destroy] sexecpawn
    } in_petsc
  } bind def

  | ksp A x b | --
  /ksp_solve {
    {
      ~[5 1 roll 4 {getid 4 1 roll} repeat ~ksp_solve] sexecpawn
    } in_petsc
  } bind def
  
  | ksp x b | --
  /ksp_resolve {
    {
      ~[4 1 roll 3 {getid 3 1 roll} repeat null 3 1 roll ~ksp_solve] sexecpawn
    } in_petsc
  } bind def

  | ksp A x b <d data> | <d data>
  /get_ksp_solve {
    {
      2 index exch vector_result
      ~[5 1 roll 4 {getid 4 1 roll} repeat ~get_ksp_solve] sexecpawn
      data
    } in_petsc
  } bind def

  | ksp x b <d data> | <d data>
  /get_ksp_resolve {
    {
      2 index exch vector_result
      ~[4 1 roll 
        3 {getid 3 1 roll} repeat null 3 1 roll ~get_ksp_solve
      ] sexecpawn
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
        ~[exch len range /proc find dup /listclass eq {/exec find} if]
      } execpawns
    } in_petsc
  } bind def

  | ~icols ~irows | --
  /condense_sparse {
    {
      /irows name /icols name
      /icols_len mpi /pawns get list def
      /irows_g mpi /pawns get list def
      ~[
        /icols find dup class /listclass eq {~exec} if
        {/icols name
          {
            ~[icols length mpirank {
              icols_len exch put
            }] rsend
          } in_petsc
        } ~exec
      ] sexecpawns

      1 1 icols_len length 1 sub {
        icols_len 1 index 1 sub get icols_len 2 index get add
        icols_len 3 -1 roll put
      } for
      
      {
        dup 0 eq {pop {}} {
          ~[icols_len 3 -1 roll 1 sub get 
            /irows find dup class /listclass eq {~exec} if {
              exch add pop
          } ~exec]
        } ifelse
      } execpawns
    } in_petsc
  } bind def
  
  100 dict /petsc_tester name
  /petsc_test {
    /petsc_tester_ layer petsc_tester begin  {
      {
        /petsc_tester_ layer 
        100 dict dup /petsc_tester name begin
      } sexecpawn

      5 {/nl name /n0 name
        0 nl 5 mul /d array copy /matDdata name
        0 1 nl 1 sub {/row name
          1 matDdata row 5 mul 5 getinterval 
          n0 row add dup 5 1 index sub getinterval copy pop
        } for
        matDdata
      } execrange
      /matD dup /dense 5 dup mat_create def
      
      5 {/d array 5 exch copy /vecxdata name pop vecxdata} execrange
      /vecx dup 5 vec_create def

      5 {/nl name /n0 name
        /matSrows 0 nl 1 add /d array copy def
        0 n0 1 add 1 n0 nl add {/i name
          5 i 1 sub sub add dup matSrows i put
        } for
        /matScols matSrows dup length 1 sub get /d array def
        0 1 matScols length 1 sub {/i name
          
        } for
        
        /matScols  <l 0 1 2 3 4 1 2 3 4 2 3 4 3 4>             def
        /matSrows  <l 0 5 9 12 14 15>                          def
        /matSrows  matSrows n0 nl 1 add getinterval            def
        /matSstart matSrows 0 get                              def
        /matSsize  matSrows dup length 1 sub get matSstart sub def

        /matScols matScols matSstart matSsize getinterval      def
        /matSdata 1 matSsize /d array copy                     def

        matSdata
      } execrange
      ~matScols ~matSrows condense_sparse
      /matS dup ~matSrows ~matScols /sparse 5 dup mat_create def
      
      /kS dup ksp_create def
      /kD dup ksp_create def
      5 {/nl name /n0 name
        /vecbdata nl /d array 0 nl 5 n0 sub -1 ramp pop def
        vecbdata
      } execrange
      /vecb dup 5 vec_create def
      
      {
        /vecb2data vecbdata dup length /d array copy 2 mul def
        vecb2data
      } sexecpawns
      /vecb2 dup 5 vec_create def
      
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
      } sexecpawn
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

dm_type mkact exec

end _module

