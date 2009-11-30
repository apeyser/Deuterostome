#include "dm.h"

#include "matrix.h"

#if BUILD_ATLAS
#include <cblas.h>
#include <clapack.h>
#include <stdarg.h>

typedef L32 INDEX_SIZE;
#define INDEX_MAX (L32MAX)

static BOOLEAN xerbla_background = FALSE;
static BOOLEAN xerbla_err = FALSE;
void cblas_xerbla(int p, const char *rout, const char *form, ...) 
{
  B* f;
  char* buf;
  int len, nlen, len_;
  va_list argptr;
  va_start(argptr, form);
  xerbla_err = TRUE;

  if (CEILvm - FREEvm <= FRAMEBYTES || o1 >= CEILopds) goto xerbla_err;

  f = FREEvm;
  TAG(f) = ARRAY | BYTETYPE;
  ATTR(f) = PARENT;
  VALUE_PTR(f) = f+FRAMEBYTES;
  nlen = ARRAY_SIZE(f) = FREEvm - CEILvm - FRAMEBYTES;
  buf = (char*) VALUE_PTR(f);

  if (! p) len = 0;
  else len = snprintf(buf, nlen,
                      "Parameter %d to routine %s was incorrect\n", 
                      p, rout);
  if (len < 0 || len > nlen) goto xerbla_err;

  buf += len;
  nlen -= len;
  len_ = len;
  len = vsnprintf(buf, nlen, form, argptr);
  if (len < 0 || len > nlen) goto xerbla_err;
        
  ARRAY_SIZE(f) = len + len_;
  FREEvm = (B*) DALIGN(VALUE_PTR(f) + len + len_);
  moveframe(f, o1);
  FREEopds = o2;
  if (! xerbla_background) goto xerbla_end;

 xerbla_err:
  va_end(argptr);
  va_start(argptr, form);
  if (p) fprintf(stderr, "Parameter %d to routine %s was incorrect\n", 
                 p, rout);
  vfprintf(stderr, form, argptr);

 xerbla_end:
  va_end(argptr);
  return;
}

static BOOLEAN errprn_err = FALSE;
int cblas_errprn(int ierr, int info, char *form, ...) 
{
  B* f;
  char* buf;
  int len;
  va_list argptr;
  va_start(argptr, form);
  errprn_err = TRUE;

  if (CEILvm - FREEvm <= FRAMEBYTES || o1 >= CEILopds) goto errnprn_err;

  f = FREEvm;
  TAG(f) = ARRAY | BYTETYPE;
  ATTR(f) = PARENT;
  VALUE_PTR(f) = f+FRAMEBYTES;
  ARRAY_SIZE(f) =  FREEvm - CEILvm - FRAMEBYTES;
  buf = (char*) VALUE_PTR(f);

  len = vsnprintf(buf, ARRAY_SIZE(f), form, argptr);
  if (len < 0 || len > ARRAY_SIZE(f)) goto errnprn_err;

  ARRAY_SIZE(f) = len;
  FREEvm = (B*) DALIGN(VALUE_PTR(f) + len);
  moveframe(f, o1);
  FREEopds = o2; 
  if (! xerbla_background) goto errnprn_end;

 errnprn_err:
  va_end(argptr);
  va_start(argptr, form);
  vfprintf(stderr, form, argptr);

 errnprn_end:
  va_end(argptr);
  return (ierr < info) ? ierr : info;
}

#define CHECK_ERR do {                     \
    xerbla_background = FALSE;             \
    if (xerbla_err || errprn_err) {        \
      xerbla_err = errprn_err = FALSE;     \
      return MATRIX_INT_ERR;               \
    }                                      \
  } while (0)

DM_INLINE_STATIC P matrix_dims(B* cuts, B* array, 
			       INDEX_SIZE* m, INDEX_SIZE* n, INDEX_SIZE* lda)
{
  INDEX_SIZE t;

  if (CLASS(cuts) != ARRAY 
      || CLASS(array) != ARRAY) return OPD_CLA;
  
  if (TYPE(cuts) != LONG32TYPE
      || TYPE(array) != DOUBLETYPE) return OPD_TYP;

  if (ARRAY_SIZE(cuts) < 2) return MATRIX_UNDER_CUT;
  if ((t = ((L32*) VALUE_PTR(cuts))[1]) > INDEX_MAX
      || t < 1) 
    return MATRIX_UNDEF_CUT;
  *n = t;
  *lda = *n = t;

  if ((t = ((L32*) VALUE_PTR(cuts))[0]) > INDEX_MAX
      || t < 1) 
    return MATRIX_UNDEF_CUT;

  *m = t/(*lda);
  if (ISUNDEF(*m)) return MATRIX_UNDEF_CUT;  
  if ((*n)*(*m) > ARRAY_SIZE(array)) return MATRIX_NONMATCH_CUT;

  return OK;
}

DM_INLINE_STATIC P matrix_square(B* cuts, B* array, INDEX_SIZE* n) {
  INDEX_SIZE m, lda;
  P r;
  if ((r = matrix_dims(cuts, array, &m, n,  &lda))) return r;
  if (m != *n) return MATRIX_NONMATCH_SHAPE;
  return OK;
}

DM_INLINE_STATIC P get_trans(B* t, enum CBLAS_TRANSPOSE* trans_, BOOLEAN* trans) {
  if (CLASS(t) != BOOL) return OPD_CLA;
  *trans = BOOL_VAL(t);
  *trans_ = *trans ? CblasTrans : CblasNoTrans;
  return OK;
}

DM_INLINE_STATIC P matrix_square_trans(B* cuts, B* array, INDEX_SIZE* n, 
                             B* t, enum CBLAS_TRANSPOSE* trans_,
                             BOOLEAN* trans) {
  INDEX_SIZE m, lda;
  P r;
  if ((r = matrix_dims(cuts, array, &m, n, &lda))) return r;
  if (m != *n) return MATRIX_NONMATCH_SHAPE;
  return get_trans(t, trans_, trans);
}

DM_INLINE_STATIC P matrix_dims_trans(B* cuts, B* array, INDEX_SIZE* m, INDEX_SIZE* n, 
				     B* t, enum CBLAS_TRANSPOSE* trans_, 
				     BOOLEAN* trans, INDEX_SIZE* lda) {
  P r;
  if ((r = matrix_dims(cuts, array, m, n, lda))) return r;
  if ((r = get_trans(t, trans_, trans)) != OK) return r;
  return OK;
}

DM_INLINE_STATIC P pivot_dims(B* pivot, INDEX_SIZE rows)
{
  if (CLASS(pivot) != ARRAY) return OPD_CLA;
  if (TYPE(pivot) != LONG32TYPE) return OPD_TYP;
  if (rows != ARRAY_SIZE(pivot)) return MATRIX_NONMATCH_CUT;

  return OK;
}

DM_INLINE_STATIC P pivot_check(B* pivot, INDEX_SIZE rows)
{
    P r;
    L32* p;
    L32* p_;

    if ((r = pivot_dims(pivot, rows))) return r;

    p = ((L32*) VALUE_PTR(pivot));
    p_ = p + rows;
    while (p < p_ && *p >= 0 && *p < rows) p++;
    if (p != p_) return MATRIX_PIVOT_CORR;

    return OK;
}

DM_INLINE_STATIC P vector_dim(B* vec, INDEX_SIZE rows) {
  if (CLASS(vec) != ARRAY) return OPD_CLA;
  if (TYPE(vec) != DOUBLETYPE) return OPD_TYP;
  if (rows != ARRAY_SIZE(vec)) return MATRIX_NONMATCH_CUT;
  
  return OK;
}

DM_INLINE_STATIC P vector_get_dim(B* vec, INDEX_SIZE* rows) {
  P rows_;
  if (CLASS(vec) != ARRAY) return OPD_CLA;
  if (TYPE(vec) != DOUBLETYPE) return OPD_TYP;

  if ((rows_ = ARRAY_SIZE(vec)) > INDEX_MAX || rows_ < 0)
    return MATRIX_VECTOR_SIZE;
  *rows = (INDEX_SIZE) rows_;
  return OK;
}

DM_INLINE_STATIC void matrix_trans(INDEX_SIZE rows, INDEX_SIZE cols, 
				   BOOLEAN istrans, 
				   INDEX_SIZE* rowst, INDEX_SIZE* colst)
{
  if (istrans) {
    *rowst = cols;
    *colst = rows;
  } 
  else {
    *rowst = rows;
    *colst = cols;
  }
}

DM_INLINE_STATIC P mult(B* num, D* val) {
  if (CLASS(num) != NUM) return OPD_CLA;
  if (! DVALUE((num), (val))) return UNDF_VAL;
  return OK;
}

#define MATRIX_RETCHECK(func) do { \
    P r;			   \
    if ((r = func)) return r;	   \
  } while (0)

#define MATRIX_TRANS(rows, cols, istrans, rowst, colst) \
  matrix_trans((rows), (cols), (istrans), &(rowst), &(colst))

#define VECTOR_GET_DIM(vec, rows)		\
  MATRIX_RETCHECK(vector_get_dim(vec, &(rows)))

#define GET_TRANS(t, trans, trans_)			\
  MATRIX_RETCHECK(get_trans((t), &(trans), &(trans_)))

#define VECTOR_DIM(v, rows)			\
  MATRIX_RETCHECK(vector_dim((v), (rows)))

#define PIVOT_DIMS(pivot, rows)			\
  MATRIX_RETCHECK(pivot_dims((pivot), (rows)))

#define PIVOT_CHECK(pivot, rows)		\
  MATRIX_RETCHECK(pivot_check((pivot), (rows)))

#define MATRIX_DIMS_TRANS(cuts, a, m, n, t, trans, transb, lda)		\
  MATRIX_RETCHECK(matrix_dims_trans((cuts), (a), &(m), &(n),		\
				    (t), &(trans), &(transb), &(lda)))

#define MATRIX_SQUARE_TRANS(cuts, a, n, t, trans, transb)		\
  MATRIX_RETCHECK(matrix_square_trans((cuts), (a), &(n),		\
				      (t), &(trans), &(transb)))

#define MATRIX_SQUARE(cuts, a, n) \
  MATRIX_RETCHECK(matrix_square((cuts), (a), &(n)))

#define MATRIX_DIMS(cuts, a, m, n, lda) \
  MATRIX_RETCHECK(matrix_dims((cuts), (a), &(m), &(n), &(lda)))

#define MULT(num, val) \
  MATRIX_RETCHECK(mult((num), &(val)))

/*--------------------------------------------- matmul_blas
 * C <cuts> beta A <cuts> transA B <cuts> transB alpha | C <cuts>
 * alpha*A*B + beta*C -> C
 */

P op_matmul_blas(void)
{
    INDEX_SIZE Nrowa,  Nrowb,  Ncola,  Ncolb, Ncolc, Nrowc, lda, ldb, ldc;
    INDEX_SIZE Ncola_, Nrowa_, Ncolb_, Nrowb_;
    D *ap, *bp, *cp;
    D alpha, beta;
    enum CBLAS_TRANSPOSE transA, transB;
    BOOLEAN transA_, transB_;

    if (o_10 < FLOORopds) return OPDS_UNF;
    if (ATTR(o_10) & READONLY) return OPD_ATR;

    MATRIX_DIMS_TRANS(o_3, o_4, Nrowb, Ncolb, o_2, transB, transB_,ldb);
    MATRIX_DIMS_TRANS(o_6, o_7, Nrowa, Ncola, o_5, transA, transA_, lda);
    MATRIX_DIMS(o_9, o_10, Nrowc, Ncolc, ldc);
    MULT(o_1, alpha);
    MULT(o_8, beta);
    MATRIX_TRANS(Nrowb, Ncolb, transB_, Nrowb_, Ncolb_);
    MATRIX_TRANS(Nrowa, Ncola, transA_, Nrowa_, Ncola_);
    
    cp = (D*) VALUE_PTR(o_10);
    ap = (D*) VALUE_PTR(o_7);
    bp = (D*) VALUE_PTR(o_4);

    if (Ncola_ != Nrowb_ || Nrowc != Nrowa_ || Ncolc != Ncolb_) 
      return MATRIX_NONMATCH_SHAPE;

    cblas_dgemm(CblasRowMajor,
		transA,
		transB,
                Nrowc,  Ncolc,  Ncola_,
		alpha, ap, lda, 
                bp,  ldb,
		beta, cp,  ldc);
    CHECK_ERR;
        
    FREEopds = o_8;
    return(OK);
}

// matrix <l cuts> <l pivot> | lumatrix(matrix) <cuts> <l pivot> true
//                           | false
P op_decompLU_lp(void) {
  INDEX_SIZE N;
  P info;
  BOOLEAN nsing = TRUE;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (o2 > CEILopds) return OPDS_OVF;
  if ((ATTR(o_3) & READONLY) || (ATTR(o_1) & READONLY)) return OPD_ATR;

  MATRIX_SQUARE(o_2, o_3, N);
  PIVOT_DIMS(o_1, N);

  if ((info = clapack_dgetrf(CblasRowMajor,  N,  N, 
                             (D*) VALUE_PTR(o_3),  N,
                             (L32*) VALUE_PTR(o_1))) < 0)
    return MATRIX_PARAM_ERROR;
  else if (info > 0) nsing = FALSE;
  CHECK_ERR;
  
  FREEopds = nsing ? o2 : o_2;    
  TAG(o_1) = BOOL; 
  ATTR(o_1) = 0;
  BOOL_VAL(o_1) = nsing;    
  return OK;
}

// rhs lumatrix <cut> <pivot> | solution(rhs)
P op_backsubLU_lp(void) {
  INDEX_SIZE N;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_4) & READONLY) return OPD_ATR;
  
  MATRIX_SQUARE(o_2, o_3, N);
  PIVOT_CHECK(o_1, N);
  VECTOR_DIM(o_4, N);

  if (clapack_dgetrs(CblasRowMajor, CblasNoTrans,
                     N, 1, 
                     (D*) VALUE_PTR(o_3), N, 
                     (L32*) VALUE_PTR(o_1),
                     (D*) VALUE_PTR(o_4), N))
    return MATRIX_PARAM_ERROR;
  CHECK_ERR;
  
  FREEopds = o_3;
  return OK;
}

// lumatrix <cuts> pivot | invmatrix(lumatrix) <cuts>
P op_invertLU_lp(void) {
  INDEX_SIZE N;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_3) & READONLY) return OPD_ATR;

  MATRIX_SQUARE(o_2, o_3, N);
  PIVOT_CHECK(o_1, N);

  if (clapack_dgetri(CblasRowMajor, N,
                     (D*) VALUE_PTR(o_3), N,
                     (L32*) VALUE_PTR(o_1)))
    return MATRIX_PARAM_ERROR;

  FREEopds = o_1;
  return OK;
}

// x | sqrt(sum_i x_i^2)
P op_norm2_blas(void) {
  D r;
  if (o1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY) return OPD_CLA;
  if (TYPE(o_1) != DOUBLETYPE) return OPD_TYP;
  r = cblas_dnrm2(ARRAY_SIZE(o_1), (D*) VALUE_PTR(o_1), 1);
  CHECK_ERR;

  TAG(o_1) = NUM | DOUBLETYPE; 
  ATTR(o_1) = 0;
  *(D*) NUM_VAL(o_1) = r;
  return OK;
}

// y x a | y (yi=yi+a*xi)
P op_vecadd_blas(void) {
  D alpha;
  P sz;
  if (o3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY || CLASS(o_2) != ARRAY || CLASS(o_3) != NUM) 
    return OPD_CLA;
  if (TYPE(o_1) != DOUBLETYPE || TYPE(o_2) != DOUBLETYPE) return OPD_TYP;
  if ((sz = ARRAY_SIZE(o_1)) != ARRAY_SIZE(o_2)) return MATRIX_VECTOR_NONMATCH;
  if (! DVALUE(o_3, &alpha)) return UNDF_VAL;
  cblas_daxpy(sz, alpha, (D*) VALUE_PTR(o_3), 1, (D*) VALUE_PTR(o_2), 1);
  CHECK_ERR;

  FREEopds = o_2;
  return OK;
}

// x alpha | x (xi=alpha*xi)
P op_vecscale_blas(void) {
  D alpha;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM || CLASS(o_2) != ARRAY) return OPD_CLA;
  if (TYPE(o_2) != DOUBLETYPE) return OPD_TYP;
  if (! DVALUE(o_1, &alpha)) return UNDF_VAL;
  
  cblas_dscal(ARRAY_SIZE(o_2), alpha, (D*) VALUE_PTR(o_2), 1);
  CHECK_ERR;

  FREEopds = o_1;
  return OK;
}

// y x | y (y_i=x_i)
P op_veccopy_blas(void) {
  P sz;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY || CLASS(o_2) != ARRAY) return OPD_CLA;
  if (TYPE(o_1) != DOUBLETYPE || TYPE(o_2) != DOUBLETYPE) return OPD_TYP;
  if ((sz = ARRAY_SIZE(o_1)) != ARRAY_SIZE(o_2)) return MATRIX_VECTOR_NONMATCH;
  
  cblas_dcopy(sz, (D*) VALUE_PTR(o_1), 1, (D*) VALUE_PTR(o_2), 1);
  CHECK_ERR;

  FREEopds = o_1;
  return OK;
}

// x y | x.y
P op_dot_blas(void) {
  P sz;
  D r;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY || CLASS(o_2) != ARRAY) return OPD_CLA;
  if (TYPE(o_1) != DOUBLETYPE || TYPE(o_2) != DOUBLETYPE) return OPD_TYP;
  if ((sz = ARRAY_SIZE(o_1)) != ARRAY_SIZE(o_2)) return MATRIX_VECTOR_NONMATCH;

  r = cblas_ddot(sz, (D*) VALUE_PTR(o_1), 1, (D*) VALUE_PTR(o_2), 1);
  CHECK_ERR;
  
  TAG(o_2) = (NUM|DOUBLETYPE);
  ATTR(o_2) = 0;
  *(D*) NUM_VAL(o_2) = r;
  FREEopds = o_1;
  return OK;
}
 

// y beta A <cuts> transpose x alpha | y=alpha*A*x+beta*y
P op_matvecmul_blas(void) {
  D alpha, beta;
  INDEX_SIZE Nrowa, Ncola, Nrowa_, Ncola_, lda;
  enum CBLAS_TRANSPOSE trans;
  BOOLEAN trans_;

  if (o_7 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_7) & READONLY) return OPD_ATR;

  MATRIX_DIMS_TRANS(o_4, o_5, Nrowa, Ncola, o_3, trans, trans_, lda);
  MATRIX_TRANS(Nrowa, Ncola, trans_, Nrowa_, Ncola_);
  VECTOR_DIM(o_2, Ncola_);
  VECTOR_DIM(o_7, Nrowa_);
  MULT(o_1, alpha);
  MULT(o_6, beta);

  cblas_dgemv(CblasRowMajor,
              trans,
              Nrowa, Ncola, alpha,
              (D*) VALUE_PTR(o_5), lda,
              (D*) VALUE_PTR(o_2), 1, beta,
              (D*) VALUE_PTR(o_7), 1);
  CHECK_ERR;
  
  FREEopds = o_6;
  return OK;
}

// x A <cuts> trans upper unit | x=A^(-1)x
P op_solvetriang_blas(void) {
  enum CBLAS_TRANSPOSE trans;
  BOOLEAN uplo, unit, trans_;
  INDEX_SIZE N;
  
  if (FLOORopds > o_6) return OPDS_UNF;
  if (CLASS(o_1) != BOOL || CLASS(o_2) != BOOL)
    return OPD_CLA;

  MATRIX_SQUARE_TRANS(o_4, o_5, N, o_3, trans, trans_);
  VECTOR_DIM(o_6, N);
  uplo = trans_ ? !BOOL_VAL(o_2) : BOOL_VAL(o_2);
  unit = BOOL_VAL(o_1);
  
  cblas_dtrsv(CblasRowMajor, 
              uplo ? CblasUpper : CblasLower, 
              trans,
              unit ? CblasUnit : CblasNonUnit,
              N, (D*) VALUE_PTR(o_5), N, 
              (D*) VALUE_PTR(o_6), 1);
  CHECK_ERR;

  FREEopds = o_5;
  return OK;
}

// <d h1 h2> | c s (h1=rot, h2=0)
P op_givens_blas(void) {
  D c, s;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (CEILopds < o_2) return OPDS_OVF;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  VECTOR_DIM(o_1, 2);
  
  cblas_drotg((D*) VALUE_PTR(o_1), ((D*) VALUE_PTR(o_1))+1, &c, &s);
  CHECK_ERR;
  ((D*) VALUE_PTR(o_1))[1] = 0;

  TAG(o_1) = NUM | DOUBLETYPE; 
  ATTR(o_1) = 0;
  *((D*) NUM_VAL(o_1)) = c;
  TAG(o1) = NUM | DOUBLETYPE; 
  ATTR(o1) = 0;
  *((D*) NUM_VAL(o1)) = s;
  FREEopds = o2;
  return OK;
}

// c s <d x...> <d y...> | <xr...> <yr...>
P op_rotate_blas(void) {
  D c, s;
  INDEX_SIZE rows;

  if (FLOORopds > o_4) return OPDS_UNF;
  if ((ATTR(o_1) & READONLY) || (ATTR(o_2) & READONLY)) return OPD_ATR;
  VECTOR_GET_DIM(o_1, rows);
  VECTOR_DIM(o_2, rows);
  MULT(o_3, s);
  MULT(o_4, c);

  cblas_drot(rows, (D*) VALUE_PTR(o_2), 1, (D*) VALUE_PTR(o_1), 1,
             c, s);
  CHECK_ERR;

  moveframes(o_2, o_4, 2);
  FREEopds = o_2;
  return OK;
}

P op_xerbla_test(void) {
  BOOLEAN test;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != BOOL) return OPD_CLA;
  test = BOOL_VAL(o_1);
  
  xerbla_background = TRUE;
  if (test) {
    cblas_dgemv(CblasRowMajor, CblasTrans, 
                0, 0, 0, NULL, 0, NULL, 0, 0, NULL, 0);
  } 
  else {
    D A[1] = {0};
    D B[1] = {0};
    D C[1] = {0};
    cblas_dgemv(CblasRowMajor, CblasTrans,
                1, 1, 1, A, 1, B, 1, 1, C, 1);
  }
  CHECK_ERR;

  FREEopds = o_1;
  return OK;  
}

#endif //BUILD_ATLAS
