/PETSC module 100 dict dup begin

{
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
} /ksptypes 1 index length dict dup begin |[
  0 4 -1 roll {
    1 index def
    1 add
  } forall pop |]
end def

{
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
} /pctypes 1 index length dict dup begin |[
  0 4 -1 roll {
    1 index def
    1 add
  } forall pop |]
end def


dm_type /dpawn eq {
  getplugindir (dm-petsc.la) loadlib /petsc name

  /notify {
    {
      {
        save ~[n ~notify] rsend restore
        restore
      } in_petsc
    } lock
  } bind def

  /n mpirank def
  /in_petsc {
    PETSC begin /PETSC_ layer stopped /PETSC_ _layer {stop} if
  } bind def


  | /name <d ...> | --
  /vec_create {
    0 1 index length petsc_vec_create petsc_vec_copyto def
  } bind def

  | /A n <l irows> <l icols> <d data> | --
  /mat_create {
    {/data name 1 index /irows name
      petsc_mat_create /A name
      0 1 A /m get 1 sub {/i name
        data irow i get irow i 1 add get 1 index sub getinterval
        i 0 A petsc_mat_copyto pop
      } for

      A
    } in_petsc
    def
  } bind def

  | A x | -- (Ax on dnode)
  /get_matvecmul {
    {
      save
      petsc_mat_vecmul 
      0 1 index /n get /d array petsc_vec_copyfrom 
      ~[exch n ~get_matvecmul_res] rsend
      restore
    } in_petsc
  } bind def

  | ksp kspsettings | --
  /ksp_create {
    {
      begin {
        ksptype kspparam pctype pcparam petsc_ksp_create
        dup rtol atol dtol maxits petsc_ksp_tol
      } stopped end {stop} if
    } in_petsc
    def
  } bind def

  /vec_destroy {
    {petsc_vec_destroy} in_petsc
  } bind def

  /mat_destroy {
    {petsc_mat_destroy} in_petsc
  } bind def

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

} {

  /in_petsc {
    PETSC begin /PETSC_ layer {      
      /n pawns def
      /nwait 0 def
      exec
    } stopped /PETSC_ _layer {stop} if
  } bind def

  /makebusy {mpidata begin ~makebusy stopped end {stop} if} bind def
  /makeready {mpidata begin makeready_dpawn} bind def
  /pawns {mpidata /pawns get} def

  | i n | n0 n1
  /range {/n_ name /i_ name
    i_ n_ n div mul

    n_ n div
    i_ n 1 sub eq {n_ n mod add} if
  } bind def

  /wait {
    {{
      nwait 0 eq {exit} if
      halt
    } loop} lock
  } bind def

  /notify {
    {
      makeready
      /nwait nwait 1 sub def
      continue
    } lock
  } bind def

  | i {func} | --
  /call {
    {
      1 index makebusy
      ~[~dup ~capsave 4 -1 roll 
        ~exec ~notify
      ] rsend
      /nwait nwait 1 add def
    } lock
  } bind def

  | ~func-maker | --
  /callwait {/cfunc name
    {0 1 n 1 sub {dup cfunc call} for wait} in_petsc
  } bind def

  | /x <d ...> | --
  /vec_create {
    {2 copy /v name /x name
      {~[x v 4 -1 roll v length range getinterval ~vec_create]} callwait
    } in_petsc
    def
  } bind def

  | /A n <l irows> <l icols> <d data> | -- 
  /mat_create {
    {
      dup 5 1 roll /data name /icols name /irows name /cols name
      /irows_ irows length /l array def
      {/i name
        i irows length 1 sub range /nl name /n0 name
        ~[
          A cols
          irows  n0 nl 1 add getinterval  
          irows_ n0 nl 1 add getinterval copy irows i get sub
          icols irows i get irows i 1 add get 1 index sub getinterval
          data  irows i get irows i 1 add get 1 index sub getinterval
          ~mat_create
        ]
      } callwait
    } in_petsc
    def
  } bind def

  | /A /x | --
  /get_matvecmul {
    {
      /x name /A name
      /x_ x find def
      {pop ~[A mkact x mkact ~get_matvecmul]} callwait
    } in_petsc
  } bind def

  | <d sub-vec> mpi | --
  /get_matvecmul_res {
    x_ exch 1 index length ~range lock getinterval copy pop
  } bind def

  /kspsettings 8 dict dup begin |[
    /rtol     1e-12                          def
    /atol     *                              def
    /dtol     {1d kspsettings /rtol get div} def
    /maxits   *                              def
    /pctype   *                              def
    /kpstype  *                              def 
    /kspparam null                           def
    /pcparam  null                           def |]
  end def

  | /ksp | --
  /ksp_create {
    kspsettings {pop ~[3 index mkact 3 index ~ksp_create]} callwait pop pop
  } bind def

  | /x | --
  /vec_destroy {
    {pop ~[2 index mkact ~vec_destroy]} callwait pop
  } bind def

  | /A | --
  /mat_destroy {
    {pop ~[2 index mkact ~mat_destroy]} callwait pop
  } bind def

  | /ksp | --
  /ksp_destroy {
    {pop ~[2 index mkact ~ksp_destroy]} callwait pop
  } bind def

  | /ksp /A /x /b | --
  /ksp_solve {
    {pop 
      ~[5 index mkact 5 index mkact 5 index mkact 5 index mkact ~ksp_solve]
    } callwait pop pop pop pop
  } bind def

  | /ksp /x /b | --
  /ksp_resolve {
    {pop 
      ~[4 index mkact null 5 index mkact 5 index mkact ~ksp_solve]
    } callwait pop pop pop
  } bind def

  | bool | --
  /report {
    0 ~[3 -1 roll {{/report name} in_petsc} ~lock] rsend
  } bind def

} ifelse

end _module
