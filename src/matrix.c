#include "matrix.h"

#if BUILD_ATLAS
#include <cblas.h>
#include <clapack.h>

static L matrix_dims(B* cuts, B* array, L* m, L* n, L* lda) 
{
  if (CLASS(cuts) != ARRAY 
      || CLASS(array) != ARRAY) return OPD_CLA;
  
  if (TYPE(cuts) != LONGTYPE 
      || TYPE(array) != DOUBLETYPE) return OPD_TYP;

  if (ARRAY_SIZE(cuts) < 2) return MATRIX_UNDER_CUT;
  *n = ((L*) VALUE_PTR(cuts))[1];
  *lda = *n ? *n : 1;
  *m = ((L*) VALUE_PTR(cuts))[0]/(*lda);

  if (*n == LINF || *m == LINF) return MATRIX_UNDEF_CUT;
  if (*n < 0 || *m < 0) return MATRIX_ILLEGAL_CUT;
  if ((*n)*(*m) > ARRAY_SIZE(array)) return MATRIX_NONMATCH_CUT;

  return OK;
}

static L matrix_square(B* cuts, B* array, L* n) {
  L m, lda;
  L r = matrix_dims(cuts, array, &m, n,  &lda);
  if (r != OK) return r;
  if (m != *n) return MATRIX_NONMATCH_SHAPE;
  return OK;
}

static L get_trans(B* t, BOOLEAN* trans, enum CBLAS_TRANSPOSE* trans_) {
  if (CLASS(t) != BOOL) return OPD_CLA;
  *trans = BOOL_VAL(t);
  *trans_ = *trans ? CblasTrans : CblasNoTrans;
  return OK;
}

static L matrix_square_trans(B* cuts, B* array, L* n, 
                             B* t, enum CBLAS_TRANSPOSE* trans_) {
  BOOLEAN trans;
  L m, lda;
  L r = matrix_dims(cuts, array, &m, n, &lda);
  if (r != OK) return r;
  if (m != *n) return MATRIX_NONMATCH_SHAPE;
  return get_trans(t, &trans, trans_);
}

static L matrix_dims_trans(B* cuts, B* array, L* m, L* n, 
                           B* t, enum CBLAS_TRANSPOSE* trans_, 
                           L* lda) {
  BOOLEAN trans;
  L m_, n_;
  L r = matrix_dims(cuts, array, &m_, &n_, lda);
  if (r != OK) return r;
  if ((r = get_trans(t, &trans, trans_)) != OK) return r;
  
  if (trans) {
    *m = n_;
    *n = m_;
  }
  else {
    *m = m_;
    *n = n_;
  }

  return OK;
}

static L pivot_dims(B* pivot, L rows) {
  if (CLASS(pivot) != ARRAY) return OPD_CLA;
  if (TYPE(pivot) != DWORDTYPE) return OPD_TYP;
  if (rows != ARRAY_SIZE(pivot)) return MATRIX_NONMATCH_CUT;

  return OK;
}

static L vector_dim(B* vec, L rows) {
  if (CLASS(vec) != ARRAY) return OPD_CLA;
  if (TYPE(vec) != DOUBLETYPE) return OPD_TYP;
  if (rows != ARRAY_SIZE(vec)) return MATRIX_NONMATCH_CUT;
  
  return OK;
}

static L vector_get_dim(B* vec, L* rows) {
  if (CLASS(vec) != ARRAY) return OPD_CLA;
  if (TYPE(vec) != DOUBLETYPE) return OPD_TYP;
  *rows = ARRAY_SIZE(vec);
  
  return OK;
}

#define VECTOR_GET_DIM(vec, rows) do {          \
    L r = vector_get_dim(vec, &rows);           \
    if (r != OK) return r;                      \
  } while (0)

#define GET_TRANS(t, trans) do {                 \
    BOOLEAN trans_;                              \
    L r = get_trans((t), &(trans_), &(trans));   \
    if (r != OK) return r;                       \
  } while (0)

#define VECTOR_DIM(v, rows) do {      \
    L r = vector_dim((v), (rows));    \
    if (r != OK) return r;         \
  } while (0)

#define PIVOT_DIMS(pivot, rows) do { \
    L r = pivot_dims((pivot), (rows));          \
    if (r != OK) return r;           \
  } while (0)

#define MATRIX_DIMS_TRANS(cuts, a, m, n, t, trans, lda) do {    \
    L r = matrix_dims_trans((cuts), (a), &(m), &(n), (t), &(trans), &(lda)); \
    if (r != OK) return r;                                     \
  } while (0)

#define MATRIX_SQUARE_TRANS(cuts, a, n, t, trans) do { \
    L r = matrix_square_trans((cuts), (a), &(n), (t), &(trans));  \
    if (r != OK) return r;                             \
  } while (0)

#define MATRIX_SQUARE(cuts, a, n) do {          \
    L r = matrix_square((cuts), (a), &(n));     \
    if (r != OK) return r;                      \
  } while (0)

#define MATRIX_DIMS(cuts, a, m, n, lda) do {        \
    L r = matrix_dims((cuts), (a), &(m), &(n), &(lda)); \
    if (r != OK) return r;                          \
  } while (0)

static L mult(B* num, D* val) {
  if (CLASS(num) != NUM) return OPD_CLA;
  if (! DVALUE((num), (val))) return UNDF_VAL;
  return OK;
}

#define MULT(num, val) do { \
    L r = mult((num), &(val));                  \
    if (r != OK) return r; \
  } while (0)

/*--------------------------------------------- matmul_blas
 * C <cuts> beta A <cuts> transA B <cuts> transB alpha | C <cuts>
 * alpha*A*B + beta*C -> C
 */

L op_matmul_blas(void)
{
    L Nrowa,  Nrowb,  Ncola,  Ncolb, Ncolc, Nrowc, lda, ldb, ldc;
		D *ap, *bp, *cp;
		D alpha, beta;
		enum CBLAS_TRANSPOSE transA, transB;

		if (o_10 < FLOORopds) return OPDS_UNF;
		if (ATTR(o_10) & READONLY) return OPD_ATR;

    MATRIX_DIMS_TRANS(o_3, o_4, Nrowb, Ncolb, o_2, transB, ldb);
    MATRIX_DIMS_TRANS(o_6, o_7, Nrowa, Ncola, o_5, transA, lda);
    MATRIX_DIMS(o_9, o_10, Nrowc, Ncolc, ldc);
    MULT(o_1, alpha);
    MULT(o_8, beta);
    
    cp = (D*) VALUE_PTR(o_10);
    ap = (D*) VALUE_PTR(o_7);
    bp = (D*) VALUE_PTR(o_4);

    if (Ncola != Nrowb || Nrowc != Nrowa || Ncolc != Ncolb) 
      return MATRIX_NONMATCH_SHAPE;

		cblas_dgemm(CblasRowMajor,
								transA,
								transB,
								Nrowc, Ncolc, Ncola,
								alpha, ap, lda, 
                bp, ldb,
								beta, cp, ldc);
        
		FREEopds = o_8;
		return(OK);
}

// matrix <cuts> <l pivot> | lumatrix(matrix) <cuts> <l pivot> true
//                         | false
L op_decompLU_lp(void) {
  L N, info;
  BOOLEAN nsing = TRUE;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (o2 > CEILopds) return OPDS_OVF;
  if ((ATTR(o_3) & READONLY) || (ATTR(o_1) & READONLY)) return OPD_ATR;

  MATRIX_SQUARE(o_2, o_3, N);
  PIVOT_DIMS(o_1, N);

  if ((info = clapack_dgetrf(CblasRowMajor, N, N, 
                             (D*) VALUE_PTR(o_3), N,
                             (M*) VALUE_PTR(o_1))) < 0)
    return MATRIX_PARAM_ERROR;
  else if (info > 0) nsing = FALSE;

  FREEopds = nsing ? o2 : o_2;    
  TAG(o_1) = BOOL; ATTR(o_1) = 0;
  BOOL_VAL(o_1) = nsing;    
  return OK;
} 

// rhs lumatrix <cut> <pivot> | solution(rhs)
L op_backsubLU_lp(void) {
  L N;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_4) & READONLY) return OPD_ATR;

  MATRIX_SQUARE(o_2, o_3, N);
  PIVOT_DIMS(o_1, N);
  VECTOR_DIM(o_4, N);

  if (clapack_dgetrs(CblasRowMajor, CblasNoTrans,
                     N, 1, 
                     (D*) VALUE_PTR(o_3), N, 
                     (M*) VALUE_PTR(o_1),
                     (D*) VALUE_PTR(o_4), N))
    return MATRIX_PARAM_ERROR;

  FREEopds = o_3;
  return OK;
}

// lumatrix <cuts> pivot | invmatrix(lumatrix) <cuts>
L op_invertLU_lp(void) {
  L N;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_3) & READONLY) return OPD_ATR;

  MATRIX_SQUARE(o_2, o_3, N);
  PIVOT_DIMS(o_1, N);

  if (clapack_dgetri(CblasRowMajor, N, 
                     (D*) VALUE_PTR(o_3), N,
                     (M*) VALUE_PTR(o_1)))
    return MATRIX_PARAM_ERROR;

  FREEopds = o_1;
  return OK;
}

// array | ||array||_2
L op_norm2(void) {
  D r;
  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY) return OPD_CLA;
  if (TYPE(o_1) != DOUBLETYPE) return OPD_TYP;
  r = cblas_dnrm2(ARRAY_SIZE(o_1), (D*) VALUE_PTR(o_1), 1);
  TAG(o_1) = NUM | DOUBLETYPE; ATTR(o_1) = 0;
  *(D*) NUM_VAL(o_1) = r;
  return OK;
}

// y beta A <cuts> transpose x alpha | y=alpha*A*x+beta*y
L op_matvecmul_blas(void) {
  D alpha, beta;
  L Nrowa, Ncola, lda;
  enum CBLAS_TRANSPOSE trans;

  if (o_7 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_7) & READONLY) return OPD_ATR;

  MATRIX_DIMS_TRANS(o_4, o_5, Nrowa, Ncola, o_3, trans, lda);
  VECTOR_DIM(o_2, Ncola);
  VECTOR_DIM(o_7, Nrowa);
  MULT(o_1, alpha);
  MULT(o_6, beta);

  cblas_dgemv(CblasRowMajor,
              trans,
              Nrowa, Ncola, alpha,
              (D*) VALUE_PTR(o_5), lda,
              (D*) VALUE_PTR(o_2), 1, beta,
              (D*) VALUE_PTR(o_7), 1);
  
  FREEopds = o_6;
  return OK;
}

// x A <cuts> trans upper unit | x=A^(-1)x
L op_triangular_solve(void) {
  enum CBLAS_TRANSPOSE trans;
  BOOLEAN uplo, unit;
  L N;
  
  if (FLOORopds > o_6) return OPDS_UNF;
  if (CLASS(o_1) != BOOL || CLASS(o_2) != BOOL)
    return OPD_CLA;

  MATRIX_SQUARE_TRANS(o_4, o_5, N, o_3, trans);
  VECTOR_DIM(o_6, N);
  uplo = BOOL_VAL(o_2);
  unit = BOOL_VAL(o_3);
  
  cblas_dtrsv(CblasRowMajor, 
              uplo ? CblasUpper : CblasLower, 
              trans,
              unit ? CblasUnit : CblasNonUnit,
              N, (D*) VALUE_PTR(o_5), N, 
              (D*) VALUE_PTR(o_6), 1);

  FREEopds = o_5;
  return OK;
}

// <d h1 h2> | c s (h1=rot, h2=0)
L op_givens_blas(void) {
  D c, s;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (CEILopds < o_2) return OPDS_OVF;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  VECTOR_DIM(o_1, 2);
  
  cblas_drotg((D*) VALUE_PTR(o_1), ((D*) VALUE_PTR(o_1))+1, &c, &s);
  ((D*) VALUE_PTR(o_1))[1] = 0;

  TAG(o_1) = NUM | DOUBLETYPE; ATTR(o_1) = 0;
  *((D*) NUM_VAL(o_1)) = c;
  TAG(o1) = NUM | DOUBLETYPE; ATTR(o1) = 0;
  *((D*) NUM_VAL(o1)) = s;
  FREEopds = o2;
  return OK;
}

// c s <d x...> <d y...> | <xr...> <yr...>
L op_rotate_blas(void) {
  D c, s;
  L rows;

  if (FLOORopds > o_4) return OPDS_UNF;
  if ((ATTR(o_1) & READONLY) || (ATTR(o_2) & READONLY)) return OPD_ATR;
  VECTOR_GET_DIM(o_1, rows);
  VECTOR_DIM(o_2, rows);
  MULT(o_3, s);
  MULT(o_4, c);

  cblas_drot(rows, (D*) VALUE_PTR(o_2), 1, (D*) VALUE_PTR(o_1), 1,
             c, s);

  moveframes(o_2, o_4, 2);
  FREEopds = o_2;
  return OK;
}

#endif //BUILD_ATLAS
