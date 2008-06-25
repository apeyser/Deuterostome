#include "dm-dmpetsc-header.h"

PLUGIN_INTRO(1, );

P ll_errc[] = { 
  DMPETSC_MATOVF, 
  DMPETSC_INVVEC, 
  DMPETSC_INVMAT, 
  DMPETSC_INVKSP, 
  DMPETSC_ILLEGAL_OWNERSHIP, 
  DMPETSC_NOMATCH, 
  DMPETSC_NONLOCAL, 
  DMPETSC_KSPSOLVE_NOINIT, 
  DMPETSC_KSPSOLVE_NODUP, 
  DMPETSC_ERR_MEM, 
  DMPETSC_ERR_SUP, 
  DMPETSC_ERR_SUP_SYS, 
  DMPETSC_ERR_ORDER, 
  DMPETSC_ERR_SIG, 
  DMPETSC_ERR_FP, 
  DMPETSC_ERR_COR, 
  DMPETSC_ERR_LIB, 
  DMPETSC_ERR_PLIB, 
  DMPETSC_ERR_MEMC, 
  DMPETSC_ERR_CONV_FAILED, 
  DMPETSC_ERR_USER, 
  DMPETSC_ERR_ARG_SIZ, 
  DMPETSC_ERR_ARG_IDN, 
  DMPETSC_ERR_ARG_WRONG, 
  DMPETSC_ERR_ARG_CORRUPT, 
  DMPETSC_ERR_ARG_OUTOFRANGE, 
  DMPETSC_ERR_ARG_BADPTR, 
  DMPETSC_ERR_ARG_NOTSAMETYPE, 
  DMPETSC_ERR_ARG_NOTSAMECOMM, 
  DMPETSC_ERR_ARG_WRONGSTATE, 
  DMPETSC_ERR_ARG_INCOMP, 
  DMPETSC_ERR_ARG_NULL, 
  DMPETSC_ERR_ARG_UNKNOWN_TYPE, 
  DMPETSC_ERR_ARG_DOMAIN, 
  DMPETSC_ERR_FILE_OPEN, 
  DMPETSC_ERR_FILE_READ, 
  DMPETSC_ERR_FILE_WRITE, 
  DMPETSC_ERR_FILE_UNEXPECTED, 
  DMPETSC_ERR_MAT_LU_ZRPVT, 
  DMPETSC_ERR_MAT_CH_ZRPVT, 
  DMPETSC_DIVERGED_NULL, 
  DMPETSC_DIVERGED_ITS, 
  DMPETSC_DIVERGED_DTOL, 
  DMPETSC_DIVERGED_BREAKDOWN, 
  DMPETSC_DIVERGED_BREAKDOWN_BICG, 
  DMPETSC_DIVERGED_NONSYMMETRIC, 
  DMPETSC_DIVERGED_INDEFINITE_PC, 
  DMPETSC_DIVERGED_NAN, 
  DMPETSC_DIVERGED_INDEFINITE_MAT, 
  0L
};

B* ll_errm[] = { 
  (B*)"** dmpetsc: Woww!! 2^32 matrices created -- impressive!", 
  (B*)"** dmpetsc: Invalidated vector", 
  (B*)"** dmpetsc: Invalidated matrix", 
  (B*)"** dmpetsc: Invalidated ksp", 
  (B*)"** dmpetsc: Changed ownership in dup", 
  (B*)"** dmpetsc: Non matching dimensions", 
  (B*)"** dmpetsc: Accessing non-local data", 
  (B*)"** dmpetsc: Matrix for solution undefined", 
  (B*)"** dmpetsc: Matrix for solution is not a dup of last one", 
  (B*)"** dmpetsc: unable to allocate requested memory", 
  (B*)"** dmpetsc: no support for requested operation", 
  (B*)"** dmpetsc: no support for requested operation on this computer system", 
  (B*)"** dmpetsc: operation done in wrong order", 
  (B*)"** dmpetsc: signal received", 
  (B*)"** dmpetsc: floating point exception", 
  (B*)"** dmpetsc: corrupted PETSc object", 
  (B*)"** dmpetsc: error in library called by PETSc", 
  (B*)"** dmpetsc: PETSc library generated inconsistent data", 
  (B*)"** dmpetsc: memory corruption", 
  (B*)"** dmpetsc: iterative method (KSP or SNES) failed", 
  (B*)"** dmpetsc: user has not provided needed function", 
  (B*)"** dmpetsc: nonconforming object sizes used in operation", 
  (B*)"** dmpetsc: two arguments not allowed to be the same", 
  (B*)"** dmpetsc: wrong argument (but object probably ok)", 
  (B*)"** dmpetsc: null or corrupted PETSc object as argument", 
  (B*)"** dmpetsc: input argument, out of range", 
  (B*)"** dmpetsc: invalid pointer argument", 
  (B*)"** dmpetsc: two args must be same object type", 
  (B*)"** dmpetsc: two args must be same communicators", 
  (B*)"** dmpetsc: object in argument is in wrong state, e.g. unassembled mat", 
  (B*)"** dmpetsc: two arguments are incompatible", 
  (B*)"** dmpetsc: argument is null that should not be", 
  (B*)"** dmpetsc: type name doesn't match any registered type", 
  (B*)"** dmpetsc: argument is not in domain of function", 
  (B*)"** dmpetsc: unable to open file", 
  (B*)"** dmpetsc: unable to read from file", 
  (B*)"** dmpetsc: unable to write to file", 
  (B*)"** dmpetsc: unexpected data in file", 
  (B*)"** dmpetsc: detected a zero pivot during LU factorization", 
  (B*)"** dmpetsc: detected a zero pivot during Cholesky factorization", 
  (B*)"** dmpetsc: diverged due to null", 
  (B*)"** dmpetsc: diverged due to iterations", 
  (B*)"** dmpetsc: diverged due to solution magnitude (dtol)", 
  (B*)"** dmpetsc: diverged due to breakdown", 
  (B*)"** dmpetsc: diverged due to breakdown bigcg (\?\?)", 
  (B*)"** dmpetsc: diverged due to nonsymmetric", 
  (B*)"** dmpetsc: diverged due to indefinite preconditioner", 
  (B*)"** dmpetsc: diverged due to Not-A-Number", 
  (B*)"** dmpetsc: diverged due to indefinite matrix", 
  NULL
};

B* ll_export[] = { 
  PLUGIN_OPS,
  PLUGIN_OP(FINI_),
  PLUGIN_OP(petsc_vec_create),
  PLUGIN_OP(petsc_vec_copy),
  PLUGIN_OP(petsc_vec_copyto),
  PLUGIN_OP(petsc_vec_copyfrom),
  PLUGIN_OP(petsc_vec_syncto),
  PLUGIN_OP(petsc_vec_syncfrom),
  PLUGIN_OP(petsc_vec_max),
  PLUGIN_OP(petsc_vec_min),
  PLUGIN_OP(petsc_vec_destroy),
  PLUGIN_OP(petsc_mat_sparse_create),
  PLUGIN_OP(petsc_mat_dense_create),
  PLUGIN_OP(petsc_mat_copy),
  PLUGIN_OP(petsc_mat_copyto),
  PLUGIN_OP(petsc_mat_copyfrom),
  PLUGIN_OP(petsc_mat_syncto),
  PLUGIN_OP(petsc_mat_syncfrom),
  PLUGIN_OP(petsc_mat_destroy),
  PLUGIN_OP(petsc_mat_dup),
  PLUGIN_OP(petsc_mat_vecmul),
  PLUGIN_OP(petsc_ksp_create),
  PLUGIN_OP(petsc_ksp_destroy),
  PLUGIN_OP(petsc_ksp_tol),
  PLUGIN_OP(petsc_ksp_iterations),
  PLUGIN_OP(petsc_ksp_solve),
  PLUGIN_OP(INIT_),
  (B*)"", NULL
};

P op_INIT_(void) {
  makename(DMPETSC_VECTOR_string, DMPETSC_VECTOR_frame);
  makename(VECTOR_VECTOR_string, VECTOR_VECTOR_frame);
  makename(VECTOR_N_string, VECTOR_N_frame);
  makename(VECTOR_GN_string, VECTOR_GN_frame);
  makename(VECTOR_ASS_string, VECTOR_ASS_frame);
  makename(DMPETSC_MATRIX_string, DMPETSC_MATRIX_frame);
  makename(MATRIX_MATRIX_string, MATRIX_MATRIX_frame);
  makename(MATRIX_M_string, MATRIX_M_frame);
  makename(MATRIX_N_string, MATRIX_N_frame);
  makename(MATRIX_GM_string, MATRIX_GM_frame);
  makename(MATRIX_ASS_string, MATRIX_ASS_frame);
  makename(MATRIX_DUPID_string, MATRIX_DUPID_frame);
  makename(DMPETSC_KSP_string, DMPETSC_KSP_frame);
  makename(KSP_KSP_string, KSP_KSP_frame);
  makename(KSP_N_string, KSP_N_frame);
  makename(KSP_KSPTYPE_string, KSP_KSPTYPE_frame);
  makename(KSP_PCTYPE_string, KSP_PCTYPE_frame);
  makename(KSP_DUPID_string, KSP_DUPID_frame);
  return init_();
}

P op_FINI_(void) {return fini_();}

P op_petsc_vec_create(void) {return petsc_vec_create();}

P op_petsc_vec_copy(void) {return petsc_vec_copy();}

P op_petsc_vec_copyto(void) {return petsc_vec_copyto();}

P op_petsc_vec_copyfrom(void) {return petsc_vec_copyfrom();}

P op_petsc_vec_syncto(void) {return petsc_vec_syncto();}

P op_petsc_vec_syncfrom(void) {return petsc_vec_syncfrom();}

P op_petsc_vec_max(void) {return petsc_vec_max();}

P op_petsc_vec_min(void) {return petsc_vec_min();}

P op_petsc_vec_destroy(void) {return petsc_vec_destroy();}

P op_petsc_mat_sparse_create(void) {return petsc_mat_sparse_create();}

P op_petsc_mat_dense_create(void) {return petsc_mat_dense_create();}

P op_petsc_mat_copy(void) {return petsc_mat_copy();}

P op_petsc_mat_copyto(void) {return petsc_mat_copyto();}

P op_petsc_mat_copyfrom(void) {return petsc_mat_copyfrom();}

P op_petsc_mat_syncto(void) {return petsc_mat_syncto();}

P op_petsc_mat_syncfrom(void) {return petsc_mat_syncfrom();}

P op_petsc_mat_destroy(void) {return petsc_mat_destroy();}

P op_petsc_mat_dup(void) {return petsc_mat_dup();}

P op_petsc_mat_vecmul(void) {return petsc_mat_vecmul();}

P op_petsc_ksp_create(void) {return petsc_ksp_create();}

P op_petsc_ksp_destroy(void) {return petsc_ksp_destroy();}

P op_petsc_ksp_tol(void) {return petsc_ksp_tol();}

P op_petsc_ksp_iterations(void) {return petsc_ksp_iterations();}

P op_petsc_ksp_solve(void) {return petsc_ksp_solve();}

