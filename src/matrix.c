#include "matrix.h"

#if BUILD_ATLAS
#include <cblas.h>
#include <clapack.h>
/*--------------------------------------------- matmul_blas
 * C <cuts> beta A <cuts> transA B <cuts> transB alpha | C
 * alpha*A*B + beta*C -> C
 */

L op_matmul_blas(void)
{
    L Nrowa,  Nrowb,  Ncola,  Ncolb, Ncolc, Nrowc, 
      Nrowa_, Nrowb_, Ncola_, Ncolb_;
		D *ap, *bp, *cp;
		D alpha, beta;
		BOOLEAN transA, transB;

		if (o_10 < FLOORopds) return OPDS_UNF;

		if (ATTR(o_10) & READONLY) return OPD_ATR;

		if (   CLASS(o_1)  != NUM
        || CLASS(o_2)  != BOOL
        || CLASS(o_3)  != ARRAY
        || CLASS(o_4)  != ARRAY
        || CLASS(o_5)  != BOOL
        || CLASS(o_6)  != ARRAY
        || CLASS(o_7)  != ARRAY
        || CLASS(o_8)  != NUM
        || CLASS(o_9)  != ARRAY
        || CLASS(o_10) != ARRAY)
      return OPD_CLA;

    if (   TYPE(o_3)  != LONGTYPE
        || TYPE(o_4)  != DOUBLETYPE
        || TYPE(o_6)  != LONGTYPE
        || TYPE(o_7)  != DOUBLETYPE
        || TYPE(o_9)  != LONGTYPE   
        || TYPE(o_10) != DOUBLETYPE)
      return OPD_TYP;

    if (ARRAY_SIZE(o_3) < 2 
        || ARRAY_SIZE(o_6) < 2
        || ARRAY_SIZE(o_9) < 2) return MATRIX_UNDER_CUT;

    if (! DVALUE(o_1, &alpha) || ! DVALUE(o_8, &beta)) return UNDF_VAL;

    Ncolb = ((L*) VALUE_PTR(o_3))[1];
    Nrowb = ((L*) VALUE_PTR(o_3))[0]/Ncolb;
    Ncola = ((L*) VALUE_PTR(o_6))[1];
    Nrowa = ((L*) VALUE_PTR(o_6))[0]/Ncola;
    Ncolc = ((L*) VALUE_PTR(o_9))[1];
    Nrowc = ((L*) VALUE_PTR(o_9))[0]/Ncolc;
    if (Nrowa == LINF  || Nrowb == LINF || Nrowc == LINF 
        || Ncola == LINF || Ncolb == LINF || Ncolc == LINF)
      return MATRIX_UNDEF_CUT;
    if (Nrowa < 1 || Nrowb < 1 || Nrowc < 1
        || Ncola < 1 || Ncolb < 1 || Ncolc < 1)
      return MATRIX_ILLEGAL_CUT;
    if (Nrowb*Ncolb < ARRAY_SIZE(o_4)
        || Nrowa*Ncola < ARRAY_SIZE(o_7)
        || Nrowc*Ncolc <  ARRAY_SIZE(o_10))
      return MATRIX_NONMATCH_CUT;

    cp = (D*) VALUE_PTR(o_10);
    ap = (D*) VALUE_PTR(o_7);
    bp = (D*) VALUE_PTR(o_4);

    transA = BOOL_VAL(o_5);
    transB = BOOL_VAL(o_2);
		if (transA) {
				Nrowa_ = Ncola;
				Ncola_ = Nrowa;
		}
		else {
				Nrowa_ = Nrowa;
				Ncola_ = Ncola;
		}
		if (transB) {
				Nrowb_ = Ncolb;
				Ncolb_ = Nrowb;
		}
		else {
				Nrowb_ = Nrowb;
				Ncolb_ = Ncolb;
		}

    if (Ncola_ != Nrowb_ || Nrowc != Nrowa_ || Ncolc != Ncolb_) 
      return MATRIX_NONMATCH_SHAPE;

		cblas_dgemm(CblasRowMajor,
								transA ? CblasTrans : CblasNoTrans,
								transB ? CblasTrans : CblasNoTrans,
								Nrowc, Ncolc, Ncola_,
								alpha, ap, Ncola, bp, Ncolb,
								beta, cp, Ncolc);
        
		FREEopds = o_8;
		return(OK);
}

// matrix <cuts> <l pivot> | true/false
L op_decompLU_lp(void) {
  L Nrow, Ncol, info;
  BOOLEAN nsing = TRUE;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY
      || CLASS(o_2) != ARRAY
      || CLASS(o_3) != ARRAY)
    return OPD_CLA;
  
  if (TYPE(o_1) != LONGTYPE
      || TYPE(o_2) != LONGTYPE
      || TYPE(o_3) != DOUBLETYPE)
    return OPD_TYP;

  if (ARRAY_SIZE(o_2) < 2) return MATRIX_UNDER_CUT;
  Ncol = ((L*) VALUE_PTR(o_2))[1];
  Nrow = ((L*) VALUE_PTR(o_2))[0]/Ncol;
  if (Ncol == LINF) return MATRIX_UNDEF_CUT;
  if (Ncol < 1) return MATRIX_ILLEGAL_CUT;
  if (Ncol != Nrow) return MATRIX_NONMATCH_SHAPE;
  if (Ncol*Ncol > ARRAY_SIZE(o_3) || Ncol > ARRAY_SIZE(o_1))
    return MATRIX_NONMATCH_CUT;

  if ((info = clapack_dgetrf(CblasRowMajor, Ncol, Ncol, 
                             (D*) VALUE_PTR(o_3), Ncol,
                             (L*) VALUE_PTR(o_1))) < 0) 
    return MATRIX_PARAM_ERROR;
  else if (info > 0) nsing = FALSE;

  TAG(o_3) = BOOL; ATTR(o_3) = 0;
  BOOL_VAL(o_3) = nsing;
  FREEopds = o_2;
  return OK;
} 

// rhs lumatrix <cut> trans pivot| solution(rhs)
L op_backsubLU_lp(void) {
  L Nrow, Ncol;
  BOOLEAN trans;

  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY
      && CLASS(o_2) != BOOL
      && CLASS(o_3) != ARRAY
      && CLASS(o_4) != ARRAY
      && CLASS(o_5) != ARRAY)
    return OPD_CLA;

  if (TYPE(o_1) != LONGTYPE
      || TYPE(o_3) != LONGTYPE
      || TYPE(o_4) != DOUBLETYPE
      || TYPE(o_5) != DOUBLETYPE)
    return OPD_TYP;

  if (ARRAY_SIZE(o_3) < 2) return MATRIX_UNDER_CUT;
  Ncol = ((L*) VALUE_PTR(o_3))[1];
  Nrow = ((L*) VALUE_PTR(o_3))[0]/Ncol;
  if (Ncol == LINF) return MATRIX_UNDEF_CUT;
  if (Ncol < 1) return MATRIX_ILLEGAL_CUT;
  if (Ncol != Nrow) return MATRIX_NONMATCH_SHAPE;
  if (Ncol*Ncol > ARRAY_SIZE(o_4) 
      || Ncol > ARRAY_SIZE(o_5)
      || Ncol > ARRAY_SIZE(o_1))
    return MATRIX_NONMATCH_CUT;

  trans = BOOL_VAL(o_2);

  if (clapack_dgetrs(CblasRowMajor, 
                     trans ? CblasTrans : CblasNoTrans,
                     Ncol, 1, 
                     (D*) VALUE_PTR(o_4), Ncol, 
                     (L*) VALUE_PTR(o_1),
                     (D*) VALUE_PTR(o_5), Ncol))
    return MATRIX_PARAM_ERROR;

  ARRAY_SIZE(o_5) = Ncol;
  FREEopds = o_4;
  return OK;
}

// invmatrix <cuts> pivot lumatrix <cuts> | invmatrix <cuts>
L op_invertLU_lp(void) {
  L Nrowi, Ncoli, Nrowl, Ncoll, info;
  
  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY
      || CLASS(o_2) != ARRAY
      || CLASS(o_3) != ARRAY
      || CLASS(o_4) != ARRAY
      || CLASS(o_5) != ARRAY)
    return OPD_CLA;

  if (TYPE(o_1) != LONGTYPE
      || TYPE(o_2) != DOUBLETYPE
      || TYPE(o_3) != LONGTYPE
      || TYPE(o_4) != LONGTYPE
      || TYPE(o_5) != DOUBLETYPE)
    return OPD_TYP;
  
  if (ARRAY_SIZE(o_1) < 2
      || ARRAY_SIZE(o_4) < 2)
    return MATRIX_UNDER_CUT;
  
  Ncoll = ((L*) VALUE_PTR(o_1))[1];
  Nrowl = ((L*) VALUE_PTR(o_1))[0]/Ncoll;
  Ncoli = ((L*) VALUE_PTR(o_4))[1];
  Nrowi = ((L*) VALUE_PTR(o_4))[0]/Ncoli;
  
  if (Ncoll == LINF) return MATRIX_UNDEF_CUT;
  if (Ncoll < 1) return MATRIX_ILLEGAL_CUT;
  if (Ncoll != Nrowl 
      || Ncoll != Nrowi
      || Ncoll != Ncoli) 
    return MATRIX_NONMATCH_SHAPE;

  if (Ncoll*Ncoll > ARRAY_SIZE(o_2) 
      || Ncoll*Ncoll > ARRAY_SIZE(o_5)
      || Ncoll > ARRAY_SIZE(o_3))
    return MATRIX_NONMATCH_CUT;

  moveD((D*) VALUE_PTR(o_2), (D*) VALUE_PTR(o_5), Ncoll*Ncoll);
  
  if ((info = clapack_dgetri(CblasRowMajor, Ncoll, 
                             (D*) VALUE_PTR(o_5), Ncoll,
                             (L*) VALUE_PTR(o_3))) < 0)
    return MATRIX_PARAM_ERROR;
  else if (info) return MATRIX_SINGULAR;

  FREEopds = o_3;
  return OK;
}



#endif //BUILD_ATLAS
