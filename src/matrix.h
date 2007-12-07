#ifndef MATRIX_H
#define MATRIX_H

#include "dm.h" 

#define MATRIX_UNDEF_CUT        (MATRIX_ERRS+0)
#define MATRIX_ILLEGAL_CUT      (MATRIX_ERRS+1)
#define MATRIX_UNDER_CUT        (MATRIX_ERRS+2)
#define MATRIX_NONMATCH_CUT     (MATRIX_ERRS+3)
#define MATRIX_NONMATCH_SHAPE   (MATRIX_ERRS+4)
#define MATRIX_PARAM_ERROR      (MATRIX_ERRS+5)
#define MATRIX_SINGULAR         (MATRIX_ERRS+6)
#define MATRIX_INT_ERR          (MATRIX_ERRS+7)

#if HAVE_ATLAS && ATLAS_LIB

#define BUILD_ATLAS 1

P op_matmul_blas(void);
P op_decompLU_lp(void);
P op_backsubLU_lp(void);
P op_invertLU_lp(void);
P op_norm2(void);
P op_matvecmul_blas(void);
P op_triangular_solve(void);
P op_givens_blas(void);
P op_rotate_blas(void);
P op_xerbla_test(void);

#endif //BUILD_ATLAS

#endif //MATRIX_H
