/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
#ifndef MATRIX_H
#define MATRIX_H

#include "dm.h" 

#if HAVE_ATLAS && ATLAS_LIB

#define BUILD_ATLAS 1

P op_matmul_blas(void);
P op_decompLU_lp(void);
P op_backsubLU_lp(void);
P op_invertLU_lp(void);
P op_norm2_blas(void);
P op_matvecmul_blas(void);
P op_vecadd_blas(void);
P op_vecscale_blas(void);
P op_veccopy_blas(void);
P op_solvetriang_blas(void);
P op_givens_blas(void);
P op_rotate_blas(void);
P op_xerbla_test(void);

#endif //BUILD_ATLAS

#endif //MATRIX_H
