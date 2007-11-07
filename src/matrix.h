#ifndef MATRIX_H
#define MATRIX_H

#include "dm.h" 

#if HAVE_CLAPACK_H && CLAPACK_LIB

L op_matmul_blas(void);

#endif //HAVE_CLAPACK_H && CLAPACK_LIB

#endif //MATRIX_H
