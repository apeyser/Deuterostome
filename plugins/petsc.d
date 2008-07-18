/PETSC module 200 dict dup begin

|============== Global definitions ==========================

|----------------- ksptypes -------------------
| Enumeration for kspsettings for ksp_create.
| Defines the Krylov-type solver used in petsc.
| Each one may be associated with a data structure
|  (kspparam) defined by petsc_ksp_create in dmpetsc.c.in
|
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
  * /KSPDEFAULT
} makeenum def

|------------------ pctypes -----------------------
| Enumeration for kspsettings for ksp_create
| Defines the preconditioner for the solver defined by
|  ksptypes.
| Each one may be associated with a data structure
|  (pcparam) defined by petsc_ksp_create in dmpetsc.c.in
|
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
  /PCJACOBI_ILU
  /PCJACOBI_LU
  * /PCDEFAULT
} makeenum def

/monitortypes {
  /KSPMonitorOff
  /KSPMonitorTrueResidualNorm
  /KSPMonitorSingularValue
  * /KSPMonitorDefault
} makeenum def

|------------------------- in_petsc -----------------
| ~act in_petsc --
|
| executes ~act in PETSC dictionary.
|
/in_petsc {PETSC indict} bind def

|================= dpawn definitions ===========================
| Mapping from dnode functions to dpawn jobs associated with that
| function. In general, the mapping has the same name on both
| sides.
| All these dpawn procedures are collective -- they must be called
|  on all dpawns in the same order.
|
/dpawn {
  | load the operator dictionary on module load.
  getplugindir (dmpetsc.la) loadlib /petsc_oplib name
  
  |---------------------------------- vec_create
  | /x n | --
  |
  | Creates local vector with n local elements,
  | and names it /x.
  |
  /vec_create {
    {petsc_vec_create} in_petsc def
  } bind def

  |------------------------------------ vec_fill
  | x <d > | --
  |
  | Copies the elements from the d array into the vectors
  | local elements. Ie, <d > should have the same number
  | of elements as the vector has locally.
  |
  /vec_fill {
    0 3 -1 roll petsc_vec_copyto pop
  } bind def

  |------------------------------------- mat_creator

  |--------- mat_creators --------------
  | ... | A
  |
  | Calls the mat_creator for a specific matrix type,
  | eating the associated parameters.
  /mat_creators {
    | <l irows> <l icols> n | A
    | Yale compressed format sparse matrix. 
    | n is the local number of columns (the diagonal matrix)
    | irow is the offsets in to icol to start each row (local offset)
    | and is one longer than the number of rows (last is one past the
    |   the last row).
    /sparse petsc_mat_sparse_create

    | m n | A
    | Dense column oriented matrix. m is the number of rows,
    | n is the number of columns.
    /dense petsc_mat_dense_create

    | m n M N | A
    /blockdense petsc_mat_blockdense_create
  } bind makestruct def

  |-------------- mat_create -------------------
  | /A .... /type | --
  |
  | The generic creator. Creates a matrix of /type (dense, sparse, etc)
  | that will be named /A, with the parameters for it's matching
  | mat_creator in between.
  |
  /mat_create {
    {mat_creators exch get exec} in_petsc def
  } bind def

  |------------------------------------ mat_fill


  |---------------- mat_fill ---------------
  | A mmax ~row-maker | --
  | row-maker: A row | <d data> <l icols>
  |
  | Fills a matrix of any type row by row.
  | A is the matrix, mmax is the largest number of rows
  |  on any pawn, and ~row-maker creates/returns the data
  |  for each row.
  | ~row-maker gets the matrix and current local row index,
  |   and returns the data for that rows, and the column numbers
  |   matching each element of data. For dense matrices, that would
  |   be an l array of 0...N-1.
  |
  /mat_fill {
    currentdict {/lastdict name
      /filler name /mmax name 
      dup /A name                      | A

      0 1 A /MATRIX_M get 1 sub {      | A row
        2 copy ~filler lastdict indict | A row <d> <l>
        4 -2 roll exch petsc_mat_fill  | A
      } for
      mmax A /MATRIX_M get sub ~petsc_mat_syncfill repeat | A
      petsc_mat_endfill                                   | A

      pop
    } in_petsc
  } bind def

  |------------- mat_fillers_set --------------------
  | ... | --
  |
  | The type specific data initializaton function for use in
  |  mat_fill_data.
  | Gets type specific params in mat_fill_data, and store away
  |  for use by the ~row-maker procedure.
  |
  /mat_fillers_set {
    | irows icols | --
    | irows is the l-array of local offsets of each row in icol.
    | icols is the l-array of column numbers in the amtrix
    /sparse {/icols name /irows name}
    | N | --
    | N is the global number of columns in the matrix
    | creates an icol of 0...N-1 l-array.
    /dense {/N name
      /icols N /l array 0 1 index length 0 1 ramp pop def
    }
    | N | --
    /blockdense {/N name
      /icols N /l array 0 1 index length 0 1 ramp pop def
    }
  } bind makestruct def

  |-------------- mat_fillers_get ----------------------
  | <d data> | <d row> <l icols>
  |
  | the base of the ~row-maker procedure for each matrix type.
  | Takes the data (compressed) for the entire matrix, and chops
  | off the current row, and the current associated column numbers
  | Expects /row to be defined.
  |
  /mat_fillers_get {
    /sparse {
            irows row get irows row 1 add get 1 index sub getinterval
      icols irows row get irows row 1 add get 1 index sub getinterval
    }

    /dense {
      row N mul N getinterval 
      icols
    }

    /blockdense {
      row N mul N getinterval 
      icols
    }
  } bind makestruct def

  |-------------------- mat_fill_data ----------------------
  | A mmax data ... /type | --
  |
  | Fills matrix A with the data (compressed). 
  | A is the matrix.
  | mmax is the largest number of local rows on any
  |  pawn.
  | data is a <d > array, with the compressed data.
  | ... are the type specific parameters.
  | type is the matrix type: /dense, /sparse, ...
  |
  /mat_fill_data {
    {
      /mtype name
      mat_fillers_set mtype get exec
      /data name
      mat_fillers_get mtype get /data_filler name

      {/row name pop data data_filler} mat_fill
    } in_petsc
  } bind def

  |----------------------------------------------mat vec mul
  |
  |---------------------- pmatvecmul ----------------------
  | A x | --
  | 
  | Multiply Ax, store in x. A is square.
  |
  /pmatvecmul {
    {petsc_mat_vecmul} in_petsc
  } bind def
  
  |----------------------- get_matvecmul ----------------------
  | A x | -- (Ax on dnode)
  |
  | Multiply Ax, store in x, return x's elements to node.
  |
  /get_matvecmul {
    dup 3 1 roll pmatvecmul get_vector
  } bind def
  
  |--------------------------------------------------- get_vector 
  | x | --
  |
  | Return the local elements of x to node.
  |
  /get_vector {
    {
      dup 0 1 index /VECTOR_N get /d array petsc_vec_copyfrom
      ~[3 1 roll exch /VECTOR_GN get ~recv_vector_result] rsend
    } in_petsc
  } bind def

  |----------------------------------------------------- get_matrix
  |
  |--------------------- matrixers_set --------------------------
  | ... | --
  |
  | store the matrix type-specific parameters for matrix_get
  |
  /matrixers_set {
    | irows | --
    | irows: the array of local offsets for icols.
    /sparse {/irows name}
    | -- | --
    /dense  {}
    | -- | --
    /blockdense {}
  } bind makestruct def

  |---------------------- matrixers_get ---------------------------
  | A | local_interval_start
  |
  | return the type specific start of the local interval for the
  |  current row.
  | /row is defined before calling.
  /matrixers_get {
    /sparse {
      pop irows row get
    }
    /dense {
      row N mul
    }
    /blockdense {
      row N mul
    }
  } bind makestruct def
  
  |------------------------ get_matrix ------------------------------
  | A N mmax ... /type | --
  |
  | Returns the local data, row by row, to the node.
  | A is the matrix
  | N is the global number of columns.
  | mmax is the largest local number of rows on any pawn.
  | ... is the parameters specific to the type of A.
  | type is the the type of A: /dense, /sparse, ...
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

  |----------------------------------------------- ksp_create
  | /ksp kspsettings | --
  |
  | Creates ksp solver named /ksp, with the data in kspsettings.
  | See kspsettings on node for their definitions.
  |
  /ksp_create {
    {
      dup /kspsettings name
      {
        ksptype kspparam pctype pcparam monitortype petsc_ksp_create
        dup rtol atol dtol maxits petsc_ksp_tol
      } exch indict
    } in_petsc def
  } bind def

  |--------------------------------------------- vec_destroy
  | x | --
  |
  | Deallocate local portion of vector x, both in D and Petsc.
  |
  /vec_destroy {
    {petsc_vec_destroy} in_petsc
  } bind def
  
  |--------------------------------------------- mat_destroy
  | A | -- 
  |
  | Deallocate local portion of matrix m, both in D and Petsc.
  |
  /mat_destroy {
    {petsc_mat_destroy} in_petsc
  } bind def
  
  |---------------------------------------------- ksp_destroy
  | ksp | --
  |
  | Deallocate local portion of solver ksp, both in D and Petsc.
  /ksp_destroy {
    {petsc_ksp_destroy} in_petsc
  } bind def

  |------------------------------------------------- ksp_solve
  |
  |---------------- report --------------------------
  | If true, dump number of iterations to convergence
  /report true def
  |---------------- reportbuf -----------------------
  | Buffer for convergence reporting
  /repbuf 255 /b array def
  
  |----------------------- ksp_solve -------------------------
  | ksp A/null x b | --
  |
  | Use ksp to solve for x in Ax = b.
  | Reports iterations to convergence if /report=true
  | If A is null, re-uses the last last matrix A used with 
  | solver ksp. Should be more efficient in that case.
  |
  /ksp_solve {
    {3 index /ksp_ name
      petsc_ksp_solve pop
      mpirank 0 eq {
        report {
          repbuf 0 (Convergence iterations: ) fax 
                   * ksp_ petsc_ksp_iterations * number
                   (\n) fax
          0 exch getinterval toconsole
        } if
      } if
    } in_petsc
  } bind def

  |--------------- get_gsp_solve --------------------
  | ksp A/null x b | --
  |
  | Calls ksp_solve, and then returns the local portion
  | of x to the node.
  |
  /get_ksp_solve {
    1 index 5 1 roll ksp_solve get_vector
  } bind def
} def

|=================== dnode definitions ======================
|
| These functions are either internal functions, or the top
| layer that allocates the jobs to the pawns for the petsc
| functions.
/dnode {
  |------------------------------------------------- internal
  |
  |----------------- range --------------------------
  | pawnnum elements | offset length
  |
  | chops elements into ranges for a pawn.
  | pawnnum: rank of the pawn for which we want a range.
  | elements: the number of rows or such that we want to
  |  get the local range for pawnnum
  | offset: the global offset of the first element locally
  |  on pawnnum
  | length: the number of elements locally on pawnnum
  |
  /range {
    mpidata /pawns get      | p# es ps
    3 copy div mul 4 1 roll | off p# es ps
    
    3 copy div exch         | off p# es ps len p#
    2 index 1 sub eq {      | off p# es ps len
      3 copy pop mod add    | off p# es ps len
    } if
    4 1 roll pop pop pop    | off len
  } bind def

  |----------------- rangesize -----------------
  | pawnnum elements | length
  |
  | returns 'length' from 'range'
  |
  /rangesize {range exch pop} bind def

  |------------------ rangestart ----------------
  | pawnnum elements | offset
  |
  | returns 'offset' from 'range'
  |
  /rangestart {range pop} bind def

  |---------------- getid -----------------------
  | obj-dict | ~id
  |
  | For the object dict, return the name on the pawns
  |  as an active name to be executed on the pawn.
  |
  /getid {/id get mkact} bind def

  |------------------ exectype -----------------
  | funcdict matrix-dict | ...
  |
  | For a dictionary of procedures named by matrix type,
  |  execute the procedure associated with the type
  |  of the matrix.
  |
  /exectype {/mtype get get exec} bind def

  |------------------- gettype -----------------
  | matrix-dict | mtype
  |
  | return the type as a passive name associated with
  |  the matrix.
  |
  /gettype {/mtype get} bind def
  
  |---------------------------------------------- vec_create
  | /x N | xdict
  |
  | Call vec_create on the pawns.
  | Takes the name for the vector on the pawns, and the global
  |  length of the vector,
  |  and returns a dictionary describing that vector.
  |
  /vec_create {
    {
      2 dict {
        exch /N name 
        exch /id name
      } 1 index indict
      dup /xval name 
      {~[
        xval /id get
        3 -1 roll xval /N get rangesize 
        ~vec_create
      ]} execpawns
    } in_petsc
  } bind def

  |-------------------------------------------------- vec_fill
  | xval ~data | --
  |
  | Call vec_fill on the pawns.
  | xval: the node dictionary describing the vector.
  | ~data: an executable object on the pawns that will return
  |    a d-array with the local elements to fill the vector.
  |
  /vec_fill {
    {
      ~[3 -1 roll getid 3 -1 roll construct_exec ~vec_fill] sexecpawns
    } in_petsc
  } bind def

  |------------------------------------------------- mat_create
  |
  |----------------- condense_sparse ---------------------
  | ~irows ~icols | icols_off
  |
  | Calculates the global offsets of the first row for each
  |  pawn for a sparse matrix.
  |
  | ~irows: executable for the pawns that returns the local offsets
  |   of each row (in icols).
  | ~icols: executable for the pawns that returns the column number
  |   of each data element in the matrix.
  | icols_off: a list of the global offsets for the first row on
  |  each pawn. The last element is the offset of the row past the 
  |  last row of the matrix.
  |  In other words, icols_off[0]=0, icols_off[# of pawns]=global size
  |    of matrix -- just the non-zero elements.
  |
  | Called by /sparse of mat_creators_set.
  |
  /condense_sparse {
    {
      /icols name /irows name
      /icols_len mpidata /pawns get 1 add list def
      ~[
        /icols construct_execn
        {~[exch length mpirank ~condense_recv] rsend} ~in_petsc
      ] sexecpawns

      1 1 icols_len last 1 sub {
        icols_len 1 index 1 sub get icols_len 2 index get add
        icols_len 3 -1 roll put
      } for

      icols_len last -1 1 {
        icols_len 1 index 1 sub get
        icols_len 3 -1 roll put
      } for
      0 icols_len 0 put
      icols_len 
    } in_petsc
  } bind def

  |--------------------- condense_recv ---------------------
  | save icols_length pawn-num | --
  |
  | Called by pawns to return the number of non-zero elements
  |   for the current matrix that are locally stored by that pawn.
  | save: save box from the send.
  | icols_length: the total number of columns stored locally on that pawn
  | pawn_num: the rank of the pawn reporting.
  |
  | Called by pawns while the node calls condense_sparse.
  |
  /condense_recv {
    {
      icols_len exch put
      restore
    } lock
  } bind def

  |------------------------- mat_creators_get -------------------
  | pawn | ...
  |
  | Returns the matrix type-specific parameters for mat_create.
  |
  | pawn: rank number of the pawn for which we want the parameters.
  | ...: type specific parameters for the current matrix.
  |
  /mat_creators_get {
    | pawn | ~sub-irows ~sub-icols n
    | ~sub-irows: executable for the pawn that returns
    |   the local offsets for each row in the column array.
    |   If it's a procedure, append with ~exec
    | ~sub-icols: executable for the pawn that returns
    |   the column number for each data element of the matrix that
    |   is non-zero. If it's a procedure, append with ~exec.
    | n: the local number of columns. Not the width, but the diagonal
    |   matrix for the pawn. Usually is the same as the number of rows
    |   for the pawn.
    /sparse {
      Aval /n get rangesize openlist
      Aval /params get /irows get construct_exec
      Aval /params get /icols get construct_exec
      counttomark 2 add -2 roll pop
    }

    | pawn | m n
    | m: the number of local rows for the pawn.
    | n: the number of local columns for the pawn. For a square matrix,
    |   the same as m. It's the diagonal matrix columns, 
    |   not the full number of columns.
    /dense {
      dup  Aval /m get rangesize
      exch Aval /n get rangesize
    }

    | pawn | m n M N
    /blockdense {
      dup Aval /m get rangesize
      exch Aval /n get rangesize
      Aval /m get
      Aval /n get 
    }
  } bind makestruct def
  
  |------------------- mat_creators_set -------------------------
  | ... | param-dict
  |
  | Swallow up the matrix type-specific parameters for mat_create
  |    on the node.
  | ...: type-specific parameters.
  | param-dict: a dictionary storing the parameters for this type
  |   matrix.
  |
  /mat_creators_set {
    | ~irows ~icols | param-dict
    | ~irows: an executable for the pawns that returns the local offsets
    |   of the rows in icols.
    | ~icols: an executable for the pawns that returns the column number
    |   for each non-zero data element in the sparse matrix.
    |
    | Calls condense_sparse and requests from each pawn the size of ~icols
    |  in order to calculate the global offsets for each pawn.
    /sparse {
      2 copy condense_sparse
      3 dict {
        exch /icols_off name
        exch /icols name
        exch /irows name
      } 1 index indict
    }

    | -- | param-dict
    /dense {0 dict}

    | -- | param-dict
    /blockdense {0 dict}
  } bind makestruct def

  |-------------------------- mat_create --------------------
  | /A .... /type m n | Adict
  | 
  | Calls mat_create on pawns.
  |
  | /A: name of the matrix on the pawns.
  | ...: the type-specific paramters for a matrix of type /type
  |      (see mat_creators_set).
  | /type: type of /A: /sparse, /dense, ...
  | m: the global number of rows.
  | n: the global number of columns.
  | Adict: dictionary describing the matrix.
  |  elements:
  |    /m: global number of rows.
  |    /n: global number of columns.
  |    /mtype: matrix type.
  |    /params: dictionary of type specific parameters for matrix.
  |    /mmax: the maximum local number of rows on any pawn.
  |    /id: the name of the matrix on the pawns.
  |
  /mat_create {
    {
      /Aval 6 dict def
      Aval /n       put
      Aval /m       put
      Aval /mtype   put mat_creators_set Aval exectype
      Aval /params  put mpidata /pawns get 1 sub Aval /m get range 
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

  |------------------------------------------------- mat_fill
  |
  |---------------------- mat_fillers ----------------------
  | -- | ...
  |
  | Return the type-specific paramters for mat_fill_data
  |  on the pawns.
  | Called from mat_fill_data.
  |
  /mat_fillers {
    | -- | ~irows ~icols
    | ~irows: executable (appended with ~exec if procedure) on the
    |   pawn that returns the local offsets for each row in icols.
    | ~icols: executbale (appended with ~exec if procedure) on the pawn
    |   that returns the column number for the local non-zero data of the
    |   current matrix.
    /sparse {
      Aval /params get /irows get construct_exec
      Aval /params get /icols get construct_exec
    }

    | -- | N
    | N: the global number of columns for the current matrix.
    /dense {Aval /n get}

    | -- | N
    /blockdense {Aval /n get}
  } bind makestruct def

  |--------------------- mat_fill_data ------------------------
  | A ~data | --
  |
  | Fills a matrix on pawns with data from a d-array.
  | 
  | A: the dictionary describing a matrix from mat_create on node.
  | ~data: an executable that returns on each pawn a d-array of data
  |   (non-zero for sparse matrix) local to that pawn, in row and column
  |   sorted order.
  |
  /mat_fill_data {
    {
      /data name /Aval name
      ~[
        Aval getid
        Aval /mmax get
        /data construct_execn
        mat_fillers Aval exectype
        Aval gettype 
        ~mat_fill_data
      ] sexecpawns
    } in_petsc
  } bind def

  |------------------------ mat_fill ----------------------------
  | A ~row-maker | --
  | row-maker for petsc: A row | <d data> <l icols> 
  |
  | Fills a matrix on the pawns rows by row as directed by row-maker.
  | A: the dictionary from mat_create that describes the matrix.
  | ~row-maker: an executable for the pawns with the parameters:
  |    A: the matrix to be filled.
  |    row: the pawn local row number
  |    data: the (non-zero iff sparse) data for 'row'
  |    icols: the column number for each element of row.
  |
  /mat_fill {
    {
      /filler name /Aval name
      ~[
        Aval getid
        Aval /mmax get
        /filler destruct_execn
        ~mat_fill
      ] sexecpawns
    } in_petsc
  } bind def

  |------------------------------------------------------ pmatvecmul
  | A x | --
  |
  | Call pmatvecmul on pawns.
  | A: dictionary describing matrix from mat_create
  | x: dictionary describing vector from vec_create
  |
  /pmatvecmul {
    {
      2 {getid exch} repeat
      ~[3 1 roll ~pmatvecmul] sexecpawns
    } in_petsc
  } bind def  

  |-------------------------------------------------------- get_vector
  |
  |-------------------- vector_result --------------------
  | x <d data> | --
  | 
  | Called to store info for get_vector. Internal.
  | x: dictionary from vec_create.
  | data: array into which the current elements of a vector will be 
  |   stored.
  | Called by node procedures that return vector data:
  |   get_vector. get_matvecmul, get_ksp_solve, get_ksp_resolve
  |
  /vector_result {
    {/data name /xval name} in_petsc
  } bind def

  |--------------------- recv_vector_result ---------------------
  | save <d sub-vec> interval_start | --
  |
  | Called by pawns to report their local elements of a vector to 
  |   the node.
  | save: save-box from the send.
  | sub-vec: the local elements for the pawn reporting.
  | interval-start: the global offset of the first element for the 
  |   pawn-reporting.
  | Called from: indirectly by pawn calls called from those who call
  |   vector_result above.
  |
  /recv_vector_result {
    {
      data exch 2 index length getinterval copy pop
      restore
    } lock
  } bind def    
  
  |------------------- get_vector -------------------------------
  | x <d data> | <d data>
  |
  | Get the elements from all pawns for a vector.
  | x: dictionary from vec_create for the vector.
  | data: d-array in to which the elements of vector are inserted.
  |
  /get_vector {
    {
      vector_result
      ~[xval getid ~get_vector] sexecpawns
      data
    } in_petsc
  } bind def

  |------------------------------------------------------- get_matvecmul
  | A x <d data> | <d data>
  |
  | Multiplies Ax (into x) on pawns and returns the the data for x.
  | Calls get_matvecmul on pawns. Must be square.
  | A: dictionary for matrix from mat_create.
  | x: dictionary for vector from vec_create.
  | data: d-array into which the results of the multiplication will
  |  be stored on node.
  |
  /get_matvecmul {
    {
      vector_result
      ~[exch getid xval getid ~get_matvecmul] sexecpawns
      data
    } in_petsc
  } bind def
  

  |-------------------------------------------------- get_matrix
  |
  |---------------- matrix_result_splitters --------------------
  | pawn# local-offset | global-offset
  | 
  | Called by matrix_result to calculate global offset for matrix
  |   data reported by a pawn.
  | pawn#: rank of pawn reporting data.
  | local-offset: local offset of the data (from the start of the matrix)
  |   on the pawn.
  | global-offset: global-offset corresponding to local-offset.
  /matrix_result_splitters {
    /sparse {
      Aval /params get /icols_off get
      3 -1 roll get add
    }
    /dense {
      exch Aval /m get rangestart Aval /n get mul add
    }
    /blockdense {
      exch Aval /m get rangestart Aval /n get mul add
    }
  } bind makestruct def
  
  |------------------------ matrix_result -----------------------
  | A <d data> | --
  |
  | Store the info for recv_matrix_result. Called from get_matrix.
  | A: dictionary from mat_create for the matrix.
  | data: d-array into which the (non-zero for sparse) data for matrix
  |   will be stored.
  /matrix_result {
    {/data name /Aval name
      matrix_result_splitters Aval gettype get /splitter name
    } in_petsc
  } bind def

  |----------------------- recv_matrix_result ----------------------
  | save <d data> pawn# local_interval_start | --
  |
  | Called by pawn to report a sub-set of its local data for the matrix.
  | Called indirectly from pawn from get_matrix.
  |
  | save: save-box from send.
  | data: the data reported by the pawn
  | pawn#: rank of pawn reporting data
  | local_interval_start: the pawn local offset into the 
  |    matrix (as an array) of the data.
  |  
  /recv_matrix_result {
    {
      splitter 1 index length data 3 1 roll getinterval copy pop
      restore
    } lock
  } bind def  

  |----------------------- matrixers ------------------------------
  | A | ...
  |
  | Returns matrix type-specific parameters for get_matrix on the pawn.
  | A: dictionary for matrix from mat_create
  | ...| parameters for get_matrix on pawn.
  /matrixers {
    | A | ~irows
    | ~irows: executable (appended with ~exec if procedure) on pawn
    |   that returns the local offsets of rows into icols column number
    |   array
    /sparse {
      /params get /irows get construct_exec
    }
    | A | --
    /dense  pop
    | A | --
    /blockdense pop
  } bind makestruct def
  
  |----------------------- get_matrix ------------------------------
  | A <d data> | <d data>
  |
  | Calls get_matrix on pawns and return all the matrix's data.
  | A: dictionary for matrix from mat_create.
  | data: d-array into which the data (non-zero if sparse) for the matrix
  |   will be stored.
  |
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

  |---------------------------------------------------- ksp_create
  |
  |------------------- kspsettings --------------------
  | dictionary with parameters for ksp_create on pawns.
  | * signify use petsc default.
  |
  | rtol: relative convergence. Convergence achieved when
  |   ||b-Ax|| < rtol*||b|| Default is 1e-5.
  | atol: absolute convergence. Convergence achieved when
  |   ||b-Ax|| < atol. Default is 1e-50.
  | dtol: Divergence achieved when 
  |   ||b-Ax|| > dtol*||b||. Default is 1e50.
  | maxits: maximum iteration until divergence.
  |   Default is 1e5.
  | pctype: one of pctypes defining preconditioner for solver.
  |   Default is left preconditioner, block Jacobi
  | ksptype: one of the ksptypes defining the Krylov solver.
  |   Default is gmres, with classical Gram-Schmidt orthogonalization.
  | pcparam, kspparam: parameter for pctype, ksptype that requires
  |   additional data for initialization.
  | monitortype: one of the monitortypes defining monitoring output
  |  during the solver.
  |
  /kspsettings {
    /rtol     1e-12
    /atol     *
    /dtol     {1d kspsettings /rtol get div}
    /maxits   *
    /pctype   *
    /ksptype  *
    /kspparam null
    /pcparam  null
    /monitortype *
  } bind makestruct def
  
  |------------------------ kspcreate ----------------
  | /ksp | kspsettings
  |
  | Create a solver name /ksp on pawns.
  | /ksp: name of solver on pawns.
  | kspsettings: the kspsetting currently in dictionary stack,
  |   augmented with /id=/ksp.
  |
  /ksp_create {
    kspsettings dup used 1 add dict exch {merge
      2 copy /id put
      ~[3 -1 roll 2 index ~ksp_create] sexecpawns
    } in_petsc
  } bind def
  
  |---------------------------------------------- vec_destroy
  | x | --
  |
  | Call vec_destroy on pawns.
  | x: dictionary identifying vector from vec_create.
  |
  /vec_destroy {
    {
      ~[exch getid ~vec_destroy] sexecpawns
    } in_petsc
  } bind def
  
  |---------------------------------------------- mat_destroy
  | A | --
  |
  | Call mat_destroy on pawns.
  | A: dictionary identifying matrix from mat_create.
  |
  /mat_destroy {
    {
      ~[exch getid ~mat_destroy] sexecpawns
    } in_petsc
  } bind def
  
  |----------------------------------------------- ksp_destroy
  | ksp | --
  |
  | Call ksp_destroy on pawns.
  | ksp: dictionary identifying solver from ksp_create
  |
  /ksp_destroy {
    {
      ~[exch getid ~ksp_destroy] sexecpawns
    } in_petsc
  } bind def

  |------------------------------------------------ ksp_solve
  |
  |---------------------- ksp_solve -------------------------
  | ksp A x b | --
  |
  | Solve for x: Ax=b on pawns.
  | ksp: dictionary identifying solver from ksp_create.
  | A: dictionary identifying matrix from mat_create.
  | x: dictionary identifying left-hand side vector from vec_create.
  |   Will use current value of x to seed the solver.
  | b: dictionary identify right-hand side vector from vec_create.
  |
  /ksp_solve {
    {
      ~[5 1 roll 4 {getid 4 1 roll} repeat ~ksp_solve] sexecpawns
    } in_petsc
  } bind def
  
  |------------------ ksp_resolve ----------------------------
  | ksp x b | --
  |
  | Solve for x: Ax=b on pawns, where A is the last matrix used
  |   with this solver. Rest of parameters are same as ksp_solve.
  |
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

  |-------------------- get_ksp_solve ---------------------------
  | ksp A x b <d data> | <d data>
  |
  | Solve for x: Ax=b on pawns, and return the elements of x.
  | Parameters are the same as ksp_solve, except for:
  | data: d-array to insert elements of solution x.
  |
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

  |-------------------- get_ksp_resolve -------------------------
  | ksp x b <d data> | <d data>
  |
  | Solve for x: Ax=b on pawns, where A is the last matrix used
  |  for this solver, and return the x's value.
  | Same params as ksp_resolve, except for data, which is the same
  |  as in get_ksp_solve.
  |
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
      data
    } in_petsc
  } bind def
  
  |------------------------------------------------ report
  | bool | --
  |
  | set report on pawn rank 0. When true, solving a system will
  |  print out the iterations till convergence.
  |
  /report {
    0 ~[3 -1 roll {{/report name} in_petsc restore} ~lock] rsend
  } bind def
  
  |--------------------------------------------------- execrange
  | length ~active | --
  | active: global-offset elements | ...
  |
  | Execute ~active on all pawns. Pass to them the global range of
  |   some series.
  | length: number of elements for all pawns (as in 'range')
  | ~active: executable to execute on pawns.
  |    global-offset: as returned from range
  |    elements: as return from range.
  |
  /execrange {
    {/proc name /len name
      {
        ~[exch len range /proc construct_execn]
      } execpawns
    } in_petsc
  } bind def  
} def

|============================================== dm_type
|
| define on basis of node/pawn type
|
| Build this module on the basis of whether we are in a 
|  node or pawn.
| dm_type defined to /dnode or /dpawn in startup_common.d
|  loaded by d-machines when initialized.
|
dm_type mkact exec

end _module
