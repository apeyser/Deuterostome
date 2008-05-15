100 dict dup begin

/plugin_version 1 def
/plugin_name /petsc def

/plugin_types 3 dict dup begin |[
  /vector 2 dict dup begin |[
    /vector [[/NUM /LONGBIGTYPE] /VECTOR_VAL /READONLY] def
    /n      [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def |]
  end def

  /matrix 4 dict dup begin |[
    /matrix [[/NUM /LONGBIGTYPE] /MATRIX_VAL /READONLY] def
    /m      [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def
    /n      [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def
    /gm     [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def |]
  end def

  /ksp 4 dict dup begin |[
    /ksp     [[/NUM /LONGBIGTYPE] /KSP_VAL /READONLY] def 
    /m       [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def 
    /ksptype [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def
    /pctype  [[/NUM /LONG32TYPE] /L32_VAL /READONLY] def |]
  end def |]
end def

/plugin_errs 100 dict dup begin |[
  /DM_INVVEC (Invalidated vector) def
  /DM_INVMAT (Invalidated matrix) def
  /DM_INVKSP (Invalidated ksp) def
  /DM_ILLEGAL_OWNERSHIP (Changed ownership in dup) def
  /DM_NOMATCH (Non matching dimensions) def
  /DM_KSPSOLVE_NOINIT (Matrix for solution undefined) def
  /DM_ERR_MEM (unable to allocate requested memory) def
  /DM_ERR_SUP (no support for requested operation) def
  /DM_ERR_SUP_SYS (no support for requested operation on this computer system) def
  /DM_ERR_ORDER (operation done in wrong order) def
  /DM_ERR_SIG (signal received) def
  /DM_ERR_FP (floating point exception) def
  /DM_ERR_COR (corrupted PETSc object) def
  /DM_ERR_LIB (error in library called by PETSc) def
  /DM_ERR_PLIB (PETSc library generated inconsistent data) def
  /DM_ERR_MEMC (memory corruption) def
  /DM_ERR_CONV_FAILED (iterative method \(KSP or SNES\) failed) def
  /DM_ERR_USER (user has not provided needed function) def
  /DM_ERR_ARG_SIZ (nonconforming object sizes used in operation) def
  /DM_ERR_ARG_IDN (two arguments not allowed to be the same) def
  /DM_ERR_ARG_WRONG (wrong argument \(but object probably ok\)) def
  /DM_ERR_ARG_CORRUPT (null or corrupted PETSc object as argument) def
  /DM_ERR_ARG_OUTOFRANGE (input argument, out of range) def
  /DM_ERR_ARG_BADPTR (invalid pointer argument) def
  /DM_ERR_ARG_NOTSAMETYPE (two args must be same object type) def
  /DM_ERR_ARG_NOTSAMECOMM (two args must be same communicators) def
  /DM_ERR_ARG_WRONGSTATE (object in argument is in wrong state, e.g. unassembled mat) def
  /DM_ERR_ARG_INCOMP (two arguments are incompatible) def
  /DM_ERR_ARG_NULL (argument is null that should not be) def
  /DM_ERR_ARG_UNKNOWN_TYPE (type name doesn't match any registered type) def
  /DM_ERR_ARG_DOMAIN (argument is not in domain of function) def
  /DM_ERR_FILE_OPEN (unable to open file) def
  /DM_ERR_FILE_READ (unable to read from file) def
  /DM_ERR_FILE_WRITE (unable to write to file) def
  /DM_ERR_FILE_UNEXPECTED (unexpected data in file) def
  /DM_ERR_MAT_LU_ZRPVT (detected a zero pivot during LU factorization) def
  /DM_ERR_MAT_CH_ZRPVT (detected a zero pivot during Cholesky factorization) def |]
end def

/plugin_ops 100 dict dup begin |[
  /init_ null def
  /petsc_vec_create null def
  /petsc_vec_copy null def
  /petsc_vec_copyto null def
  /petsc_vec_copyfrom null def
  /petsc_vec_max null def
  /petsc_vec_min null def
  /petsc_vec_destroy null def 
  /petsc_mat_create null def 
  /petsc_mat_copy null def
  /petsc_mat_copyto null def
  /petsc_mat_copyfrom null def
  /petsc_mat_destroy null def
  /petsc_mat_dup null def
  /petsc_mat_vecmul null def 
  /petsc_ksp_create null def
  /petsc_ksp_destroy null def
  /petsc_ksp_tol null def
  /petsc_ksp_iterations null def
  /petsc_ksp_solve null def |]
end def

/all {
  getstartupdir (new-plugin.d) fromfiles
  NEW_PLUGINS begin all end
} bind def

end userdict /dm_petsc put
