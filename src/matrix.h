#ifndef MATRIX_H
#define MATRIX_H

#include "dm.h" 

#define MATRIX_UNDEF_CUT        0x00000D00L
#define MATRIX_ILLEGAL_CUT      0x00000D01L
#define MATRIX_UNDER_CUT        0x00000D02L
#define MATRIX_NONMATCH_CUT     0x00000D03L 
#define MATRIX_NONMATCH_SHAPE   0x00000D04L

#if HAVE_ATLAS && ATLAS_LIB

#define BUILD_ATLAS 1

L op_matmul_blas(void);

#endif //BUILD_ATLAS

#endif //MATRIX_H
