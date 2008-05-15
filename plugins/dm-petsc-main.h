#include "dm-petsc-header.h"

PLUGIN_INTRO(1);

P ll_errc[] = { 
  PETSC_DM_INVVEC, 
  PETSC_DM_INVMAT, 
  PETSC_DM_INVKSP, 
  PETSC_DM_ILLEGAL_OWNERSHIP, 
  PETSC_DM_NOMATCH, 
  PETSC_DM_KSPSOLVE_NOINIT, 
  PETSC_DM_ERR_MEM, 
  PETSC_DM_ERR_SUP, 
  PETSC_DM_ERR_SUP_SYS, 
  PETSC_DM_ERR_ORDER, 
  PETSC_DM_ERR_SIG, 
  PETSC_DM_ERR_FP, 
  PETSC_DM_ERR_COR, 
  PETSC_DM_ERR_LIB, 
  PETSC_DM_ERR_PLIB, 
  PETSC_DM_ERR_MEMC, 
  PETSC_DM_ERR_CONV_FAILED, 
  PETSC_DM_ERR_USER, 
  PETSC_DM_ERR_ARG_SIZ, 
  PETSC_DM_ERR_ARG_IDN, 
  PETSC_DM_ERR_ARG_WRONG, 
  PETSC_DM_ERR_ARG_CORRUPT, 
  PETSC_DM_ERR_ARG_OUTOFRANGE, 
  PETSC_DM_ERR_ARG_BADPTR, 
  PETSC_DM_ERR_ARG_NOTSAMETYPE, 
  PETSC_DM_ERR_ARG_NOTSAMECOMM, 
  PETSC_DM_ERR_ARG_WRONGSTATE, 
  PETSC_DM_ERR_ARG_INCOMP, 
  PETSC_DM_ERR_ARG_NULL, 
  PETSC_DM_ERR_ARG_UNKNOWN_TYPE, 
  PETSC_DM_ERR_ARG_DOMAIN, 
  PETSC_DM_ERR_FILE_OPEN, 
  PETSC_DM_ERR_FILE_READ, 
  PETSC_DM_ERR_FILE_WRITE, 
  PETSC_DM_ERR_FILE_UNEXPECTED, 
  PETSC_DM_ERR_MAT_LU_ZRPVT, 
  PETSC_DM_ERR_MAT_CH_ZRPVT, 
  0L
};

B* ll_errm[] = { 
  (B*)"** petsc: Invalidated vector", 
  (B*)"** petsc: Invalidated matrix", 
  (B*)"** petsc: Invalidated ksp", 
  (B*)"** petsc: Changed ownership in dup", 
  (B*)"** petsc: Non matching dimensions", 
  (B*)"** petsc: Matrix for solution undefined", 
  (B*)"** petsc: unable to allocate requested memory", 
  (B*)"** petsc: no support for requested operation", 
  (B*)"** petsc: no support for requested operation on this computer system", 
  (B*)"** petsc: operation done in wrong order", 
  (B*)"** petsc: signal received", 
  (B*)"** petsc: floating point exception", 
  (B*)"** petsc: corrupted PETSc object", 
  (B*)"** petsc: error in library called by PETSc", 
  (B*)"** petsc: PETSc library generated inconsistent data", 
  (B*)"** petsc: memory corruption", 
  (B*)"** petsc: iterative method (KSP or SNES) failed", 
  (B*)"** petsc: user has not provided needed function", 
  (B*)"** petsc: nonconforming object sizes used in operation", 
  (B*)"** petsc: two arguments not allowed to be the same", 
  (B*)"** petsc: wrong argument (but object probably ok)", 
  (B*)"** petsc: null or corrupted PETSc object as argument", 
  (B*)"** petsc: input argument, out of range", 
  (B*)"** petsc: invalid pointer argument", 
  (B*)"** petsc: two args must be same object type", 
  (B*)"** petsc: two args must be same communicators", 
  (B*)"** petsc: object in argument is in wrong state, e.g. unassembled mat", 
  (B*)"** petsc: two arguments are incompatible", 
  (B*)"** petsc: argument is null that should not be", 
  (B*)"** petsc: type name doesn't match any registered type", 
  (B*)"** petsc: argument is not in domain of function", 
  (B*)"** petsc: unable to open file", 
  (B*)"** petsc: unable to read from file", 
  (B*)"** petsc: unable to write to file", 
  (B*)"** petsc: unexpected data in file", 
  (B*)"** petsc: detected a zero pivot during LU factorization", 
  (B*)"** petsc: detected a zero pivot during Cholesky factorization", 
  NULL
};

B* ll_export[] = { 
  PLUGIN_OPS,
  PLUGIN_OP(petsc_vec_create),
  PLUGIN_OP(petsc_vec_copy),
  PLUGIN_OP(petsc_vec_copyto),
  PLUGIN_OP(petsc_vec_copyfrom),
  PLUGIN_OP(petsc_vec_max),
  PLUGIN_OP(petsc_vec_min),
  PLUGIN_OP(petsc_vec_destroy),
  PLUGIN_OP(petsc_mat_create),
  PLUGIN_OP(petsc_mat_copy),
  PLUGIN_OP(petsc_mat_copyto),
  PLUGIN_OP(petsc_mat_copyfrom),
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
  makename(PETSC_VECTOR_string, PETSC_VECTOR_frame);
  makename(VECTOR_VECTOR_string, VECTOR_VECTOR_frame);
  makename(VECTOR_N_string, VECTOR_N_frame);
  makename(PETSC_MATRIX_string, PETSC_MATRIX_frame);
  makename(MATRIX_MATRIX_string, MATRIX_MATRIX_frame);
  makename(MATRIX_M_string, MATRIX_M_frame);
  makename(MATRIX_N_string, MATRIX_N_frame);
  makename(MATRIX_GM_string, MATRIX_GM_frame);
  makename(PETSC_KSP_string, PETSC_KSP_frame);
  makename(KSP_KSP_string, KSP_KSP_frame);
  makename(KSP_M_string, KSP_M_frame);
  makename(KSP_KSPTYPE_string, KSP_KSPTYPE_frame);
  makename(KSP_PCTYPE_string, KSP_PCTYPE_frame);
  return init_();
}

P op_petsc_vec_create(void) {return petsc_vec_create();}

P op_petsc_vec_copy(void) {return petsc_vec_copy();}

P op_petsc_vec_copyto(void) {return petsc_vec_copyto();}

P op_petsc_vec_copyfrom(void) {return petsc_vec_copyfrom();}

P op_petsc_vec_max(void) {return petsc_vec_max();}

P op_petsc_vec_min(void) {return petsc_vec_min();}

P op_petsc_vec_destroy(void) {return petsc_vec_destroy();}

P op_petsc_mat_create(void) {return petsc_mat_create();}

P op_petsc_mat_copy(void) {return petsc_mat_copy();}

P op_petsc_mat_copyto(void) {return petsc_mat_copyto();}

P op_petsc_mat_copyfrom(void) {return petsc_mat_copyfrom();}

P op_petsc_mat_destroy(void) {return petsc_mat_destroy();}

P op_petsc_mat_dup(void) {return petsc_mat_dup();}

P op_petsc_mat_vecmul(void) {return petsc_mat_vecmul();}

P op_petsc_ksp_create(void) {return petsc_ksp_create();}

P op_petsc_ksp_destroy(void) {return petsc_ksp_destroy();}

P op_petsc_ksp_tol(void) {return petsc_ksp_tol();}

P op_petsc_ksp_iterations(void) {return petsc_ksp_iterations();}

P op_petsc_ksp_solve(void) {return petsc_ksp_solve();}

