#ifndef MATRIX_H
#define MATRIX_H

#include "dm.h" 

#define MATRIX_UNDEF_CUT        0x00000D00L
#define MATRIX_ILLEGAL_CUT      0x00000D01L
#define MATRIX_UNDER_CUT        0x00000D02L
#define MATRIX_NONMATCH_CUT     0x00000D03L 
#define MATRIX_NONMATCH_SHAPE   0x00000D04L
#define MATRIX_PARAM_ERROR      0x00000D05L
#define MATRIX_SINGULAR         0x00000D06L

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

#endif //BUILD_ATLAS

#endif //MATRIX_H