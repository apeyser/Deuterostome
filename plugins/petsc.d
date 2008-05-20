/PETSC module 100 dict dup begin

| [/name] | dict
/makeindexdict {
  dup length dict dup begin
  0 3 -1 roll {
    1 index def
    1 add
  } forall pop
  end
} bind def

| -- | save \[ 
/makestruct {
  [ | ]
} bind def

| \[ /name val ... | dict
/_makestruct {
  counttomark
  dup dict dup begin 1 index 3 add 1 roll
  dup 2 add -1 roll pop {
    dup 0 le {pop end exit} if
    3 1 roll def 
    2 sub
  } loop
} bind def
  
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
} makeindexdict def

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
} makeindexdict def

| -- | --
/caplayer {PETSC_ capsave} bind def

| ~act | --
/petsc_layer {
  PETSC begin /PETSC_ layer stopped end /PETSC_ _layer {stop} if
} bind def

end _module

PETSC begin

/PETSC_dpawn module 100 dict dup begin
/init {
  getplugindir (dmpetsc.la) loadlib /petsc name

  /in_petsc {
    {PETSC_dpawn begin stopped end {stop} if} petsc_layer
  } bind def
  
  | /x <d ...> | --
  /vec_create {
    {
      caplayer
      0 1 index length petsc_vec_create petsc_vec_copyto 
    } in_petsc def
  } bind def
  
  | /A n <l irows> <l icols> <d data> | --
  /mat_create {
    {
      caplayer
      /data name 1 index /irows name
      petsc_mat_create
      dup /A name
      0 1 A /m get 1 sub {/i name
        data irow i get irow i 1 add get 1 index sub getinterval
        i 0 A petsc_mat_copyto pop
      } for
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
      0 1 index /n get /d array petsc_vec_copyfrom
      ~[exch n ~recv_vector_result] rsend
    } in_petsc
  } bind def
  
  | A | --
  /get_matrix {
    {
      /A name
      /t A /n get /d array def
      0 1 A /m get {/i name
        A i A /n get t petsc_mat_copyfrom
        ~[exch i n ~recv_matrix_result] rsend
      } for
    } in_petsc
  } bind def

  | /ksp kspsettings | --
  /ksp_create {
    {
      caplayer
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
    {3 index /ksp name
      petsc_ksp_solve pop
      n 0 eq {
        report {
          repbuf 0 (Convergence iterations: ) fax 
                   * ksp petsc_ksp_iterations * number
                   (\n) fax
          0 exch getinterval toconsole
        } if
      } if
    } in_petsc
  } bind def

  | ksp A/null x b | --
  /get_ksp_solve {
    1 index 5 1 roll ksp_solve get_vector
  } bind def  
} bind def 

end _module

/PETSC_dnode module 100 dict dup begin
  
| ~act | act: <<??|??>>
/in_petsc {
  {PETSC_dnode begin stopped end {stop} if} petsc_layer
} bind def

| i n | n0 n1
/range {/n_ name /i_ name
  i_ n_ n div mul
  
  n_ n div
  i_ n 1 sub eq {n_ n mod add} if
} bind def

| dict | ~id
/getid {/id get mkact} bind def

| dict | <d data>
/getdata {/data get} bind def

| /x <d ...> | xdict
/vec_create {
  {/v name /x name
    {~[x v 4 -1 roll v length range getinterval ~vec_create]} waitforpawns
    2 dict dup begin v /data name x /id name end
  } in_petsc
} bind def

| /A n <l irows> <l icols> <d data> | Adict
/mat_create {
  5 dict dup begin |[
    exch /data name
    exch /icols name
    exch /irows name
    exch /cols name
    exch /id name |]
  end dup {begin PETSC begin {
    /irows_ irows length /l array def
    {/i name
      i irows length 1 sub range /nl name /n0 name
      ~[
        id cols
        irows  n0 nl 1 add getinterval  
        irows_ n0 nl 1 add getinterval copy irows i get sub
        icols irows i get irows i 1 add get 1 index sub getinterval
        data  irows i get irows i 1 add get 1 index sub getinterval
        ~mat_create
      ]
    } waitforpawns
  } stopped end end {stop} if} in_petsc
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
  } in_petsc
} bind def

| x | --
/vector_result {
  {/xval name} in_petsc
} bind def

| A | --
/matrix_result {
  {/Aval name} in_petsc
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
  } in_petsc
} bind def

| A | A
/get_matrix {
  {
    dup matrix_result
    ~[1 index getid ~get_matrix] waitsimple
  } in_petsc
} bind def

| A x | x
/get_matvecmul {
  {
    dup vector_result
    ~[3 -1 roll getid 2 index getid ~get_matvecmul] waitsimple
  } in_petsc
} bind def

/kspsettings makestruct |[
  /rtol     1e-12
  /atol     *
  /dtol     {1d kspsettings /rtol get div}
  /maxits   *
  /pctype   *
  /kpstype  *
  /kspparam null
  /pcparam  null |]
_makestruct def

| /ksp | kspsettings
/ksp_create {
  kspsettings dup used 1 add dict {merge
    2 copy /id put
    ~[3 -1 roll 2 index ~ksp_create] waitsimple
  } in_petsc
} bind def

| x | --
/vec_destroy {
  {
    ~[exch getid ~vec_destroy] waitsimple
  } in_petsc
} bind def

| A | --
/mat_destroy {
  {
    ~[exch getid ~mat_destroy] waitsimple
  } in_petsc
} bind def

| ksp | --
/ksp_destroy {
  {
    ~[exch getid ~ksp_destroy] waitsimple
  } in_petsc
} bind def

| ksp A x b | --
/ksp_solve {
  {
    ~[5 1 roll 4 {getid 4 1 roll} repeat ~ksp_solve] waitsimple
  } in_petsc
} bind def

| ksp x b | --
/ksp_resolve {
  {
    ~[4 1 roll 3 {getid 3 1 roll} repeat null 3 1 roll ~ksp_solve] waitsimple
  } in_petsc
} bind def

| ksp A x b | x
/get_ksp_solve {
  {
    1 index vector_result
    1 index 5 1 roll
    ~[5 1 roll 4 {getid 4 1 roll} repeat ~get_ksp_solve] waitsimple
  } in_petsc
} bind def

| ksp x b | x
/get_ksp_resolve {
  {
    1 index vector_result
    1 index 4 1 roll
    ~[4 1 roll 3 {getid 3 1 roll} repeat null 3 1 roll ~get_ksp_solve] waitsimple
  } in_petsc
} bind def

| bool | --
/report {
  0 ~[3 -1 roll {{/report name} in_petsc restore} ~lock] rsend
} bind def

100 dict /petc_tester name
/petc_test {
  /petsc_tester_ layer petsc_tester begin  {
    {
      /petsc_tester_ layer 
      100 dict dup /petsc_tester name begin
    } waitsimple

    /vecx dup <d 2 2 2 2 2> vec_create def
    /matA dup 5 <l 0 5 10 15 20 25> 
      <l 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4>
      <d 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 0 0 0 1 1 0 0 0 0 1>
      mat_create def
    /k dup ksp_create def

    /vecb dup <d 5 4 3 2 1> vec_create def
    (Solution: \n) toconsole
    k matA vecx vecb get_ksp_solve getdata v_ pop

    /vecb2 dup <d 5 4 3 2 1> 2 mul vec_create def
    (Solution x2: \n) toconsole
    k vecx vecb2 get_ksp_resolve getdata v_ pop

    vecx  vec_destroy
    vecb  vec_destroy
    vecb2 vec_destroy
    matA  mat_destroy
    ksp   ksp_destroy

    {
      end
      false /petsc_tester_ _layer
    } waitsimple
  } stopped end /petsc_tester_ _layer {stop} if
} bind def

| -- | --
/petsc_su {
  /PETSC module_send
  /PETSC_dpawn module_send
  * {PETSC begin PETSC_dpawn begin init} rsend
} bind def

end _module

end
