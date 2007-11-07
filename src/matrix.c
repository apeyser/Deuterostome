#include "matrix.h"

#if HAVE_CLAPACK_H && CLAPACK_LIB
#include <cblas.h>
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
        || ARRAY_SIZE(o_9) < 2) return RNG_CHK;
    if (! DVALUE(o_1, &alpha) || ! DVALUE(o_8, &beta)) return UNDF_VAL;

    Ncolb = ((L*) VALUE_PTR(o_3))[1];
    Nrowb = ((L*) VALUE_PTR(o_3))[0]/Ncolb;
    Ncola = ((L*) VALUE_PTR(o_6))[1];
    Nrowa = ((L*) VALUE_PTR(o_6))[0]/Ncola;
    Ncolc = ((L*) VALUE_PTR(o_9))[1];
    Nrowc = ((L*) VALUE_PTR(o_10))[0]/Ncolc;
    if (Nrowa == LINF  || Nrowb == LINF || Nrowc == LINF 
        || Ncola == LINF || Ncolb == LINF || Ncolc == LINF)
      return UNDF_VAL;
    if (Nrowa < 1 || Nrowb < 1 || Nrowc < 1
        || Ncola < 1 || Ncolb < 1 || Ncolc < 1)
      return RNG_CHK;
    if (Nrowb*Ncolb < ARRAY_SIZE(o_4)
        || Nrowa*Ncola < ARRAY_SIZE(o_7)
        || Nrowc*Ncolc <  ARRAY_SIZE(o_10))
      return RNG_CHK;

    cp = (D*) VALUE_PTR(o_10);
    ap = (D*) VALUE_PTR(o_7);
    bp = (D*) VALUE_PTR(o_4);

    transA = BOOL_VAL(o_2);
    transB = BOOL_VAL(o_4);
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
      return RNG_CHK;

		cblas_dgemm(CblasRowMajor,
								transA ? CblasTrans : CblasNoTrans,
								transB ? CblasTrans : CblasNoTrans,
								Nrowc, Ncolc, Ncola_,
								alpha, ap, Ncola, bp, Ncolb,
								beta, cp, Ncolc);
        
		FREEopds = o_8;
		return(OK);
}

#endif //HAVE_CLAPACK_H && CLAPACK_LIB
