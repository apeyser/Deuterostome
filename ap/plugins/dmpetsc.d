100 dict dup begin

/plugin_version 1 def
/plugin_name /dmpetsc def

/plugin_types 3 dict dup begin |[
  /vector 2 dict dup begin |[
    /opaque 0 def
    /members 4 dict dup begin |[
      /vector [[/NUM /LONGBIGTYPE] /VECTOR_VAL /READONLY] def
      /n      [[/NUM /LONG32TYPE] /LONG32_VAL 0] def 
      /gn     [[/NUM /LONG32TYPE] /LONG32_VAL 0] def
      /ass    [/BOOL /BOOL_VAL 0] def |]
    end def |]
  end def

  /matrix 2 dict dup begin |[
    /opaque 0 def
    /members 7 dict dup begin |[
      /matrix [[/NUM /LONGBIGTYPE] /MATRIX_VAL  /READONLY] def
      /m      [[/NUM /LONG32TYPE]  /LONG32_VAL  0] def
      /n      [[/NUM /LONG32TYPE]  /LONG32_VAL  0] def
      /gm     [[/NUM /LONG32TYPE]  /LONG32_VAL  0] def 
      /ass    [[/NUM /LONGBIGTYPE] /ASS_STATE   /READONLY] def 
      /dupid  [[/NUM /LONG64TYPE]  /ULONG64_VAL /READONLY] def 
      /mtype  [[/NUM /LONG32TYPE]  /LONG32_VAL  0] def 
    end def |]
  end def

  /ksp 5 dict dup begin |[
    /opaque 0 def
    /members 7 dict dup begin |[
      /ksp        [[/NUM /LONGBIGTYPE] /KSP_VAL     /READONLY] def 
      /n          [[/NUM /LONG32TYPE]  /LONG32_VAL  0] def 
      /ksptype    [[/NUM /LONGBIGTYPE] /VALUE_BASE  /READONLY] def
      /pctype     [[/NUM /LONGBIGTYPE] /VALUE_BASE  /READONLY] def 
      /dupid      [[/NUM /LONG64TYPE]  /ULONG64_VAL /READONLY] def
      /pcsetupfd  [[/NUM /LONGBIGTYPE] /VALUE_PTR   /READONLY] def
      /kspsetupfd [[/NUM /LONGBIGTYPE] /VALUE_PTR   /READONLY] def |]
    end def |]
  end def |]
end def

/plugin_errs 100 dict dup begin |[
  /MATOVF (Wow!! 2^32 matrices created -- impressive!) def
  /INVVEC (Invalidated vector) def
  /INVMAT (Invalidated matrix) def
  /INVKSP (Invalidated ksp) def
  /ILLEGAL_OWNERSHIP (Changed ownership in dup) def
  /ILLEGAL_OP (Illegal operation on matrix type) def
  /NOMATCH (Non matching dimensions) def
  /NONLOCAL (Accessing non-local data) def
  /KSPSOLVE_NOINIT (Matrix for solution undefined) def
  /KSPSOLVE_NODUP (Matrix for solution is not a dup of last one) def
  /ERR_MEM (unable to allocate requested memory) def
  /ERR_SUP (no support for requested operation) def
  /ERR_SUP_SYS (no support for requested operation on this computer system) def
  /ERR_ORDER (operation done in wrong order) def
  /ERR_SIG (signal received) def
  /ERR_FP (floating point exception) def
  /ERR_COR (corrupted PETSc object) def
  /ERR_LIB (error in library called by PETSc) def
  /ERR_PLIB (PETSc library generated inconsistent data) def
  /ERR_MEMC (memory corruption) def
  /ERR_CONV_FAILED (iterative method \(KSP or SNES\) failed) def
  /ERR_USER (user has not provided needed function) def
  /ERR_ARG_SIZ (nonconforming object sizes used in operation) def
  /ERR_ARG_IDN (two arguments not allowed to be the same) def
  /ERR_ARG_WRONG (wrong argument \(but object probably ok\)) def
  /ERR_ARG_CORRUPT (null or corrupted PETSc object as argument) def
  /ERR_ARG_OUTOFRANGE (input argument, out of range) def
  /ERR_ARG_BADPTR (invalid pointer argument) def
  /ERR_ARG_NOTSAMETYPE (two args must be same object type) def
  /ERR_ARG_NOTSAMECOMM (two args must be same communicators) def
  /ERR_ARG_WRONGSTATE (object in argument is in wrong state, e.g. unassembled mat) def
  /ERR_ARG_INCOMP (two arguments are incompatible) def
  /ERR_ARG_NULL (argument is null that should not be) def
  /ERR_ARG_UNKNOWN_TYPE (type name doesn't match any registered type) def
  /ERR_ARG_DOMAIN (argument is not in domain of function) def
  /ERR_FILE_OPEN (unable to open file) def
  /ERR_FILE_READ (unable to read from file) def
  /ERR_FILE_WRITE (unable to write to file) def
  /ERR_FILE_UNEXPECTED (unexpected data in file) def
  /ERR_MAT_LU_ZRPVT (detected a zero pivot during LU factorization) def
  /ERR_MAT_CH_ZRPVT (detected a zero pivot during Cholesky factorization) def 
  /DIVERGED_NULL (diverged due to null) def
  /DIVERGED_ITS (diverged due to iterations) def
  /DIVERGED_DTOL (diverged due to solution magnitude \(dtol\)) def
  /DIVERGED_BREAKDOWN (diverged due to breakdown) def
  /DIVERGED_BREAKDOWN_BICG (diverged due to breakdown bigcg \(\\?\\?\)) def
  /DIVERGED_NONSYMMETRIC (diverged due to nonsymmetric) def
  /DIVERGED_INDEFINITE_PC (diverged due to indefinite preconditioner) def
  /DIVERGED_NAN (diverged due to Not-A-Number) def
  /DIVERGED_INDEFINITE_MAT (diverged due to indefinite matrix) def |]
end def

/plugin_ops 100 dict dup begin [
  /init_ 
  /fini_ 
  /petsc_log_begin 
  /petsc_log_summary 
  /petsc_vec_create 
  /petsc_vec_copy 
  /petsc_vec_dup 
  /petsc_vec_add 
  /petsc_vec_mul
  /petsc_vec_pwr
  /petsc_vec_sqrt
  /petsc_vec_reciprocal
  /petsc_vecvec_mul
  /petsc_vecvec_add
  /petsc_vecmat_copy
  /petsc_vecmat_sync
  /petsc_vec_copyto 
  /petsc_vec_copyfrom 
  /petsc_vec_syncto 
  /petsc_vec_syncfrom 
  /petsc_vec_max 
  /petsc_vec_min 
  /petsc_vec_norm 
  /petsc_vec_destroy  
  /petsc_mat_sparse_create  
  /petsc_mat_dense_create  
  /petsc_mat_blockdense_create 
  /petsc_mat_copy 
  /petsc_mat_copyto 
  /petsc_mat_copyfrom 
  /petsc_mat_syncto 
  /petsc_mat_syncfrom 
  /petsc_mat_endfill 
  /petsc_mat_fill 
  /petsc_mat_syncfill 
  /petsc_mat_destroy 
  /petsc_mat_dup 
  /petsc_mat_vecmul  
  /petsc_mat_transpose 
  /petsc_mat_getcsr 
  /petsc_mat_getnzs 
  /petsc_ksp_create 
  /petsc_ksp_destroy 
  /petsc_ksp_tol 
  /petsc_ksp_iterations 
  /petsc_ksp_solve  
] {null def} forall end def

/all {
  getstartupdir (new-plugin.d) fromfiles
  NEW_PLUGINS begin all end
} bind def

end userdict /dmpetsc put
