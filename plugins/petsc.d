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

  | /x <d ...> | --
  /vec_create {
    {
      /v name /x name
      0 1 n 1 sub {/i name
        i ~[x v i v length range getinterval ~vec_create] call
      } for wait

      x v
    } in_petsc
    def
  } bind def

  | /A n <l irows> <l icols> <d data> | -- 
  /mat_create {
    {
      /data name /icols name /irows name /cols name /A name
      /irows_ irows dup length /l array copy def
      0 1 n 1 sub {/i name
        i irows length 1 sub range /nl name /n0 name
        i ~[
          A cols
          irows_ n0 nl getinterval irows i get sub
          icols irows i get irows i 1 add get 1 index sub getinterval
          data  irows i get irows i 1 add get 1 index sub getinterval
          ~mat_create
        ] call
      } for wait

      A data
    } in_petsc
    def
  } bind def

  | /A /x | --
  /get_matvecmul {
    {
      /x name /A name
      /x_ x find def
      0 1 n 1 sub {/i name
        i ~[A mkact x mkact ~get_matvecmul] call
      } for
      wait
    } in_petsc
  } bind def

  | <d sub-vec> mpi | --
  /get_matvecmul_res {
    x_ exch 1 index length ~range lock getinterval copy pop
  } bind def
} ifelse

end _module
