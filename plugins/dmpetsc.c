#line 2 "./dmpetsc.c.in"
#include <petscksp.h>

#ifdef PETSC_USE_SINGLE
#error "PetscReal is single"
#endif

#ifdef PETSC_USE_LONG_DOUBLE
#error "PetscReal is double"
#endif

#ifdef PETSC_USE_INT
#error "PetscReal is int"
#endif

#include <limits.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

#include "dm.h"
#include "dm-mpi.h"

enum AssState {
  ASSEMBLED,
  ASSEMBLING,
  ASSEMBLE_WRITE
};

#define ASS_STATE(frame)     (*(enum AssState*) NUM_VAL(frame))
#define VECTOR_VAL(frame)    (*(Vec*) NUM_VAL(frame))
#define MATRIX_VAL(frame)    (*(Mat*) NUM_VAL(frame))
#define KSP_VAL(frame)       (*(KSP*) NUM_VAL(frame))
#define ULONG64_VAL(frame)   (*(UL64*) &LONG64_VAL(frame))
				   
#include "dm-dmpetsc-header.h"

static PetscErrorCode petscsig(int sig __attribute__ ((__unused__)), 
			       void* ptr __attribute__ ((__unused__)) ) {
  return PETSC_ERR_SIG;
}

static P errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_INDEFINITE_MAT) + 1];

DM_INLINE_STATIC P init_(void) {
  PetscErrorCode r;

  if (setenv("PETSC_DIR", "/opt/petsc/", 1))
    return -errno;
  if (setenv("PETSC_ARCH", "linux_deb_d", 1))
    return -errno;

  errmap[0] = OK;
  errmap[PETSC_ERR_MEM] = DMPETSC_ERR_MEM;
  errmap[PETSC_ERR_SUP] = DMPETSC_ERR_SUP;
  errmap[PETSC_ERR_SUP_SYS] = DMPETSC_ERR_SUP_SYS;
  errmap[PETSC_ERR_ORDER] = DMPETSC_ERR_ORDER;
  errmap[PETSC_ERR_SIG] = DMPETSC_ERR_SIG;
  errmap[PETSC_ERR_FP] = DMPETSC_ERR_FP;
  errmap[PETSC_ERR_COR] = DMPETSC_ERR_COR;
  errmap[PETSC_ERR_LIB] = DMPETSC_ERR_LIB;
  errmap[PETSC_ERR_PLIB] = DMPETSC_ERR_PLIB;
  errmap[PETSC_ERR_MEMC] = DMPETSC_ERR_MEMC;
  errmap[PETSC_ERR_CONV_FAILED] = DMPETSC_ERR_CONV_FAILED;
  errmap[PETSC_ERR_USER] = DMPETSC_ERR_USER;
  errmap[PETSC_ERR_ARG_SIZ] = DMPETSC_ERR_ARG_SIZ;
  errmap[PETSC_ERR_ARG_IDN] = DMPETSC_ERR_ARG_IDN;
  errmap[PETSC_ERR_ARG_WRONG] = DMPETSC_ERR_ARG_WRONG;
  errmap[PETSC_ERR_ARG_CORRUPT] = DMPETSC_ERR_ARG_CORRUPT;
  errmap[PETSC_ERR_ARG_OUTOFRANGE] = DMPETSC_ERR_ARG_OUTOFRANGE;
  errmap[PETSC_ERR_ARG_BADPTR] = DMPETSC_ERR_ARG_BADPTR;
  errmap[PETSC_ERR_ARG_NOTSAMETYPE] = DMPETSC_ERR_ARG_NOTSAMETYPE;
  errmap[PETSC_ERR_ARG_NOTSAMECOMM] = DMPETSC_ERR_ARG_NOTSAMECOMM;
  errmap[PETSC_ERR_ARG_WRONGSTATE] = DMPETSC_ERR_ARG_WRONGSTATE;
  errmap[PETSC_ERR_ARG_INCOMP] = DMPETSC_ERR_ARG_INCOMP;
  errmap[PETSC_ERR_ARG_NULL] = DMPETSC_ERR_ARG_NULL;
  errmap[PETSC_ERR_ARG_UNKNOWN_TYPE] = DMPETSC_ERR_ARG_UNKNOWN_TYPE;
  errmap[PETSC_ERR_ARG_DOMAIN] = DMPETSC_ERR_ARG_DOMAIN;
  errmap[PETSC_ERR_FILE_OPEN] = DMPETSC_ERR_FILE_OPEN;
  errmap[PETSC_ERR_FILE_READ] = DMPETSC_ERR_FILE_READ;
  errmap[PETSC_ERR_FILE_WRITE] = DMPETSC_ERR_FILE_WRITE;
  errmap[PETSC_ERR_FILE_UNEXPECTED] = DMPETSC_ERR_FILE_UNEXPECTED;
  errmap[PETSC_ERR_MAT_LU_ZRPVT] = DMPETSC_ERR_MAT_LU_ZRPVT;
  errmap[PETSC_ERR_MAT_CH_ZRPVT] = DMPETSC_ERR_MAT_CH_ZRPVT;

  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_NULL)] 
    = DMPETSC_DIVERGED_NULL;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_ITS)] 
    = DMPETSC_DIVERGED_ITS;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_DTOL)] 
    = DMPETSC_DIVERGED_DTOL;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_BREAKDOWN)] 
    = DMPETSC_DIVERGED_BREAKDOWN;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_BREAKDOWN_BICG)] 
    = DMPETSC_DIVERGED_BREAKDOWN_BICG;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_NONSYMMETRIC)] 
    = DMPETSC_DIVERGED_NONSYMMETRIC;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_INDEFINITE_PC)] 
    = DMPETSC_DIVERGED_INDEFINITE_PC;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_NAN)] 
    = DMPETSC_DIVERGED_NAN;
  errmap[PETSC_ERR_MAX_VALUE + (-KSP_DIVERGED_INDEFINITE_MAT)] 
    = DMPETSC_DIVERGED_INDEFINITE_MAT;

  if ((r = PetscInitialize(PETSC_NULL, PETSC_NULL, PETSC_NULL, PETSC_NULL)))
    RETURN_ERROR(errmap[r]);

  if ((r = PetscPushSignalHandler(petscsig, NULL)))
    RETURN_ERROR(errmap[r]);

  return OK;
}

DM_INLINE_STATIC P fini_(void) {
  PetscErrorCode r;
  if ((r = PetscPopSignalHandler()))
    RETURN_ERROR(errmap[r]);
  
  return OK;
}

DM_INLINE_STATIC P INIT_VEC_(B* frame, Vec* x) {
  BOOLEAN* t;
  PetscErrorCode r;

  TEST_DMPETSC_VECTOR(frame);
  if (! (*x = DMPETSC_VECTOR_VECTOR(frame))) 
    RETURN_ERROR(DMPETSC_INVVEC);

  if (! *(t = &DMPETSC_VECTOR_ASS(frame))) {
    if ((r = VecAssemblyEnd(*x))) RETURN_ERROR(errmap[r]);
    *t = TRUE;
  }

  return OK;
}

#define INIT_VEC(frame, x) do {						\
    P _retc_ = INIT_VEC_(frame, &x);					\
    if (_retc_) return _retc_;						\
  } while (0)

DM_INLINE_STATIC P INIT_MAT_(B* frame, Mat* A, enum AssState** t) {
  TEST_DMPETSC_MATRIX(frame);
  if (! (*A = DMPETSC_MATRIX_MATRIX(frame))) 
    RETURN_ERROR(DMPETSC_INVMAT);

  if (t) *t = &DMPETSC_MATRIX_ASS(frame);
  
  return OK;
}

DM_INLINE_STATIC P INIT_MAT_ASS_(Mat A, enum AssState* t) {
  PetscErrorCode r;

  switch (*t) {
    case ASSEMBLE_WRITE:
      if ((r = MatAssemblyBegin(A, MAT_FINAL_ASSEMBLY)))
	RETURN_ERROR(errmap[r]);
      //and fall through
    case ASSEMBLING: 
      if ((r = MatAssemblyEnd(A, MAT_FINAL_ASSEMBLY)))
	RETURN_ERROR(errmap[r]);
      *t = ASSEMBLED;
      break;
    default: break;//do nothing if assembled
  }
  
  return OK;
}

DM_INLINE_STATIC P INIT_MAT_WRITE_(Mat A, enum AssState* t) {
  PetscErrorCode r;

  switch (*t) {
    case ASSEMBLE_WRITE: break;
    case ASSEMBLING: 
      if ((r = MatAssemblyEnd(A, MAT_FINAL_ASSEMBLY)))
	RETURN_ERROR(errmap[r]);
      //fall through
    default:
      *t = ASSEMBLE_WRITE;
      break;
  }
  
  return OK;
}

DM_INLINE_STATIC P INIT_MAT_WRITEEND_(Mat A, enum AssState* t) {
  PetscErrorCode r;

  if (*t == ASSEMBLE_WRITE) { 
    if ((r = MatAssemblyBegin(A, MAT_FINAL_ASSEMBLY)))
      RETURN_ERROR(errmap[r]);
    *t = ASSEMBLING;
  }

  return OK;
}


#define INIT_MAT(frame, A) do {						\
    P _retc_;								\
    enum AssState* t = NULL;						\
    if ((_retc_ = INIT_MAT_(frame, &A, &t))				\
	|| (_retc_ = INIT_MAT_ASS_(A, t)))				\
      return _retc_;							\
  } while (0)

#define INIT_MAT_WRITE(frame, A) do {					\
    P _retc_;								\
    enum AssState* t = NULL;						\
    if ((_retc_ = INIT_MAT_(frame, &A, &t))				\
	|| (_retc_ = INIT_MAT_WRITE_(A, t)))				\
      return _retc_;							\
  } while (0)

#define INIT_MAT_WRITEEND(frame, A) do {				\
    P _retc_;								\
    enum AssState* t = NULL;						\
    if ((_retc_ = INIT_MAT_(frame, &A, &t))				\
	|| (_retc_ = INIT_MAT_WRITEEND_(A, t)))				\
      return _retc_;							\
  } while (0)

DM_INLINE_STATIC P INIT_KSP_(B* frame, KSP* ksp) {
  TEST_DMPETSC_KSP(frame);
  if (! (*ksp = DMPETSC_KSP_KSP(frame)))
    RETURN_ERROR(DMPETSC_INVKSP);
  return OK;
}

#define INIT_KSP(frame, ksp) do {		\
    P _retc_ = INIT_KSP_(frame, &ksp);		\
    if (_retc_) return _retc_;			\
  } while (0)

#define DMPETSC_ERRCHECK(func) do {					\
    PetscErrorCode _r_;							\
    if ((_r_ = func)) RETURN_ERROR(errmap[_r_]);			\
  } while (0)


//------------------------ petsc_vec_create
// n | vec-handle
// n is the size of the local portion of the vector.
// the total size is n1+n2... over all mpi processes
//
DM_INLINE_STATIC P petsc_vec_create(void) {
  PetscErrorCode r;
  P n;
  B* pframe;
  B* oldfreevm = FREEvm;
  Vec* x = NULL;
  PetscInt gn;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &n)) return UNDF_VAL;

  MAKE_DMPETSC_VECTOR(pframe);
  *(x = &DMPETSC_VECTOR_VECTOR(pframe)) = NULL;
  DMPETSC_VECTOR_N(pframe) = n;
  DMPETSC_VECTOR_ASS(pframe) = FALSE;

  if ((r = VecCreateMPI(PETSC_COMM_WORLD, n, PETSC_DECIDE, x))) goto err;
  if ((r = VecGetOwnershipRange(*x, &gn, PETSC_NULL))) goto err;
  DMPETSC_VECTOR_GN(pframe) = gn;
  if ((r = VecAssemblyBegin(*x))) goto err;

  moveframe(pframe, o_1);
  return OK;

 err:
  if (x && *x) VecDestroy(*x);
  FREEvm = oldfreevm;
  RETURN_ERROR(errmap[r]);
}

//--------------------------------- petsc_vec_dup
// x | y
// y is a duplicate of x in structure
//   -- the data is not copied.
//
DM_INLINE_STATIC P petsc_vec_dup(void) {
  Vec x, y;
  B* pframe;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);

  DMPETSC_ERRCHECK(VecDuplicate(x, &y));
  MAKE_DMPETSC_VECTOR(pframe);
  DMPETSC_VECTOR_VECTOR(pframe) = y;
  DMPETSC_VECTOR_N(pframe) = DMPETSC_VECTOR_N(o_1);
  DMPETSC_VECTOR_GN(pframe) = DMPETSC_VECTOR_GN(o_1);
  DMPETSC_VECTOR_ASS(pframe) = TRUE;

  moveframe(pframe, o_1);
  return OK;
}

// v1 v2 | v2
DM_INLINE_STATIC P petsc_vec_copy(void) {
  Vec x, y;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, y);
  INIT_VEC(o_2, x);
  if (DMPETSC_VECTOR_N(o_1) != DMPETSC_VECTOR_N(o_2)) return RNG_CHK;
  
  DMPETSC_ERRCHECK(VecCopy(x, y));
  
  moveframe(o_1, o_2);
  FREEopds = o_1;
  return OK;
}

// v | v
DM_INLINE_STATIC P petsc_vec_syncto(void) {
  Vec x;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);

  DMPETSC_ERRCHECK(VecAssemblyBegin(x));
  DMPETSC_VECTOR_ASS(o_1) = FALSE;

  return OK;
}

// <d > n v | v
DM_INLINE_STATIC P petsc_vec_copyto(void) {
  Vec x;
  P n, len_arr, len_x;
  PetscInt* ix;
  P i;
  L32 gn;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &n)) return UNDF_VAL;
  if (TAG(o_3) != (ARRAY|DOUBLETYPE)) return OPD_ERR;

  len_x = DMPETSC_VECTOR_N(o_1);
  gn = DMPETSC_VECTOR_GN(o_1);
  len_arr = ARRAY_SIZE(o_3);
  if (n < 0 || n+len_arr > len_x) return RNG_CHK;
  if (FREEvm + len_arr*sizeof(PetscInt) > CEILvm) return VM_OVF;
  ix = (PetscInt*) FREEvm;
  for (i = 0; i < len_arr; ++i) ix[i] = gn + n + i;

  DMPETSC_ERRCHECK(VecSetValues(x, len_arr, ix, 
				(D*) VALUE_PTR(o_3), INSERT_VALUES));
  DMPETSC_ERRCHECK(VecAssemblyBegin(x));
  DMPETSC_VECTOR_ASS(o_1) = FALSE;
  
  moveframe(o_1, o_3);
  FREEopds = o_2;
  return OK;
}

// v | v
DM_INLINE_STATIC P petsc_vec_syncfrom(void) {
  Vec x;
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);

  return OK;
}

// v n <d > | <d sub-array>
DM_INLINE_STATIC P petsc_vec_copyfrom(void) {
  Vec x;
  P n, len_arr, len_x, ncols;
  D* a = NULL;

  if (o_3 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_3, x);
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &n)) return UNDF_VAL;
  if (TAG(o_1) != (ARRAY|DOUBLETYPE)) return OPD_ERR;

  len_x = DMPETSC_VECTOR_N(o_3);
  len_arr = ARRAY_SIZE(o_1);
  if (n < 0 || n >= len_x) return RNG_CHK;
  
  ncols = (len_arr > len_x - n) ? len_x - n : len_arr;
  DMPETSC_ERRCHECK(VecGetArray(x, &a));
  moveD(a+n, (D*) VALUE_PTR(o_1), ncols);
  DMPETSC_ERRCHECK(VecRestoreArray(x, PETSC_NULL));

  moveframe(o_1, o_3);
  ARRAY_SIZE(o_3) = ncols;
  ATTR(o_3) &= ~PARENT;
  FREEopds = o_2;
  return OK;
}

// x a | x (x_i += a)
DM_INLINE_STATIC P petsc_vec_add(void) {
  Vec x;
  D a;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!DVALUE(o_1, &a)) return UNDF_VAL;
  INIT_VEC(o_2, x);

  if (TYPE(o_1) < SINGLETYPE) {
    P v;
    PVALUE(o_1, &v);
    if (v == 0) goto out;
  }

  DMPETSC_ERRCHECK(VecShift(x, a));

 out:
  FREEopds = o_1;
  return OK;
}

// x a | x (x_i *= a)
DM_INLINE_STATIC P petsc_vec_mul(void) {
  Vec x;
  D a;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!DVALUE(o_1, &a)) return UNDF_VAL;
  INIT_VEC(o_2, x);

  if (TYPE(o_1) < SINGLETYPE) {
    P v;
    PVALUE(o_1, &v);
    if (v == 1) goto out;
    if (v == 0) {
      VecSet(x, 0);
      goto out;
    }
  }

  DMPETSC_ERRCHECK(VecScale(x, a));

 out:
  FREEopds = o_1;
  return OK;
}

// x | x (x_i = sqrt(x_i))
DM_INLINE_STATIC P petsc_vec_sqrt(void) {
  Vec x;
  
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);

  DMPETSC_ERRCHECK(VecSqrt(x));
  return OK;
}

// x | x (x_i = 1/x_i)
DM_INLINE_STATIC P petsc_vec_reciprocal(void) {
  Vec x;
  
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);

  DMPETSC_ERRCHECK(VecReciprocal(x));
  return OK;
}

DM_INLINE_STATIC P petsc_vec_denan_int(Vec x, 
				       B* xf,
				       D v,
				       L32 *restrict iout, 
				       D *restrict out, 
				       const P n,
				       const D *restrict in) {
  P i;
  P i_;
  DMPETSC_ERRCHECK(VecAssemblyBegin(x));
  DMPETSC_VECTOR_ASS(xf) = FALSE;

  for (i = 0, i_ = 0; i < n; i++)
    if (! isfinite(in[i])) {
      out[i_] = v;
      iout[i_++] = i;
    }
  
  DMPETSC_ERRCHECK(VecSetValues(x, i_, iout, out, INSERT_VALUES));
  return OK;
} 

// x d | x (x_i = d iff x_i == *) 
DM_INLINE_STATIC P petsc_vec_denan(void) {
  Vec x;
  D *restrict in;
  L32 *restrict iout;
  D *restrict out;
  P n;
  D v;
  P retc;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! DVALUE(o_1, &v)) return UNDF_VAL;

  INIT_VEC(o_2, x);

  n = DMPETSC_VECTOR_N(o_2);
  iout = (L32*) FREEvm;
  out = (D*) (iout + n);
  if (out + n > (D*) CEILvm) return VM_OVF;

  DMPETSC_ERRCHECK(VecGetArray(x, (D**)&in));
  retc = petsc_vec_denan_int(x, o_2, v, iout, out, n, in);
  DMPETSC_ERRCHECK(VecRestoreArray(x, PETSC_NULL));
  if (retc) return retc;

  FREEopds = o_1;
  return OK;
}

// x a | x (x_i = x_i^a)
DM_INLINE_STATIC P petsc_vec_pwr(void) {
  Vec x;
  D *restrict in;
  D *restrict out;
  L32 *restrict iout;
  P n, i;

  P retc;
  static const B af[FRAMEBYTES] = {ARRAY|DOUBLETYPE, 0, 0};
  static B vf[FRAMEBYTES];
  B *restrict xf = o_2;
      
  if (xf < FLOORopds) return OPDS_UNF;

  INIT_VEC(xf, x);
  n = DMPETSC_VECTOR_N(vf);
  out = (D*) FREEvm;
  iout = (L32*) (out + n);
  if (iout + n > (L32*)CEILvm) return VM_OVF;

  DMPETSC_ERRCHECK(VecGetArray(x, (D**)&in));
  moveD(in, out, n);
  DMPETSC_ERRCHECK(VecRestoreArray(x, PETSC_NULL));

  moveframe(xf, vf);
  moveframe(af, xf);
  VALUE_PTR(xf) = (B*) out;
  ARRAY_SIZE(xf) = n;

  retc = op_pwr();
  FREEopds = xf+FRAMEBYTES*2;
  moveframe(vf, xf);
  if (retc) return retc;

  DMPETSC_ERRCHECK(VecAssemblyBegin(x));
  DMPETSC_VECTOR_ASS(vf) = FALSE;
  for (i = 0; i < n; ++i) iout[i] = i;
  DMPETSC_ERRCHECK(VecSetValues(x, n, iout, out, INSERT_VALUES));

  FREEopds = o_1;
  return OK;
}

DM_INLINE_STATIC P petsc_vecvec_add_single(Vec y, B *restrict yf, 
					   L32 n, D *restrict xarr) {
  L32 i;
  L32 *restrict ix = (L32*) FREEvm;
  if (ix + n > (L32*) CEILvm) return VM_OVF;

  DMPETSC_ERRCHECK(VecAssemblyBegin(y));
  DMPETSC_VECTOR_ASS(yf) = FALSE;

  for (i = 0; i < n; i++) ix[i] = i;
  DMPETSC_ERRCHECK(VecSetValues(y, n, ix, xarr, ADD_VALUES));

  return OK;
}

// y alpha x beta | y(y=a*y+b*x)
DM_INLINE_STATIC P petsc_vecvec_add(void) {
  Vec x, y;
  D alpha, beta;
  BOOLEAN alpha_1 = FALSE, alpha_0 = FALSE, beta_1 = FALSE, beta_0 = FALSE;
  L32 n;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM || CLASS(o_3) != NUM)
    return OPD_CLA;

  if (!DVALUE(o_3, &alpha) || !DVALUE(o_1, &beta)) 
    return UNDF_VAL;

  if (TYPE(o_3) < SINGLETYPE) {
    P v;
    PVALUE(o_3, &v);
    alpha_1 = (v == 1) ? TRUE : FALSE;
    alpha_0 = (v == 0) ? TRUE : FALSE;
  }

  if (TYPE(o_1) < SINGLETYPE) {
    P v;
    PVALUE(o_1, &v);
    beta_1 = (v == 1) ? TRUE : FALSE;
    beta_0 = (v == 0) ? TRUE : FALSE;
  }

  INIT_VEC(o_2, x);
  INIT_VEC(o_4, y);
  n = DMPETSC_VECTOR_N(o_2);
  if (n != DMPETSC_VECTOR_N(o_4)) RETURN_ERROR(DMPETSC_NOMATCH);

  if (alpha_0) {
    if (beta_0) DMPETSC_ERRCHECK(VecSet(y, 0));
    else {
      DMPETSC_ERRCHECK(VecCopy(x, y));
      if (! beta_1) DMPETSC_ERRCHECK(VecScale(y, beta));
    }
  }
  else if (alpha_1) {
    if (!beta_0) {
      if (! beta_1) DMPETSC_ERRCHECK(VecAXPY(y, beta, x));
      else {
	D *restrict xarr;
	P retc;
	DMPETSC_ERRCHECK(VecGetArray(x, (D**)&xarr));
	retc = petsc_vecvec_add_single(y, o_4, n, xarr);
	DMPETSC_ERRCHECK(VecRestoreArray(x, PETSC_NULL));
	if (retc) return retc;
      }
    }
  }
  else if (beta_0) DMPETSC_ERRCHECK(VecScale(y, alpha));
  else if (beta_1) DMPETSC_ERRCHECK(VecAYPX(y, alpha, x));
  else DMPETSC_ERRCHECK(VecAXPBY(y, beta, alpha, x));

  FREEopds = o_3;
  return OK;
}

// y x | y(y_i *= x_i)
DM_INLINE_STATIC P petsc_vecvec_mul(void) {
  Vec x, y;

  if (o_2 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);
  INIT_VEC(o_2, y);

  DMPETSC_ERRCHECK(VecPointwiseMult(y, y, x));

  FREEopds = o_1;
  return OK;
}

DM_INLINE_STATIC P petsc_vecmat_copy_mat(B* mf, D* arr, P row, P n) {
  static const B af[FRAMEBYTES] = {ARRAY|DOUBLETYPE, 0, 0};
  static const B rf[FRAMEBYTES] = {NUM|LONGBIGTYPE, 0, 0};
  static const B nf[FRAMEBYTES] = {NUM|LONGBIGTYPE, 0, 0};
  P retc;
  B *sf = o1;

  moveframe(af, o1);
  VALUE_PTR(o1) = (B*) arr;
  ARRAY_SIZE(o1) = n;

  moveframe(rf, o2);
  LONGBIG_VAL(o2) = row;

  moveframe(nf, o3);
  LONGBIG_VAL(o3) = n;

  moveframe(mf, o4);  

  FREEopds = o5;
  if ((retc = op_petsc_mat_copyto()))
    FREEopds = sf;

  return retc;
}

// x offset length/* A m | A (A_m=x)
DM_INLINE_STATIC P petsc_vecmat_copy(void) {
  Vec x;
  P off, n, row;
  D* arr;
  P retc;

  if (o_5 < FLOORopds) return OPDS_OVF;
  if (o5 > CEILopds) return OPDS_OVF;
  if (CLASS(o_1) != NUM 
      || CLASS(o_3) != NUM
      || CLASS(o_4) != NUM) 
    return OPD_CLA;
  if (TYPE(o_1) >= SINGLETYPE
      || TYPE(o_3) >= SINGLETYPE
      || TYPE(o_4) >= SINGLETYPE) 
    return OPD_TYP;
  if (!PVALUE(o_1, &row) || !PVALUE(o_4, &off)) return UNDF_VAL;
  if (off < 0) return RNG_CHK;
  if (PVALUE(o_3, &n) && n < 0) return RNG_CHK;

  INIT_VEC(o_5, x);
  if (n == PINF) n = DMPETSC_VECTOR_N(o_5) - off;
  else if (n + off > DMPETSC_VECTOR_N(o_5)) RETURN_ERROR(DMPETSC_NOMATCH);

  DMPETSC_ERRCHECK(VecGetArray(x, &arr));
  if (! (retc = petsc_vecmat_copy_mat(o_2, arr + off, row, n))) {
    moveframe(o_2, o_5);
    FREEopds = o_4;
  }
  DMPETSC_ERRCHECK(VecRestoreArray(x, PETSC_NULL));
  
  return retc;
}

// x A | A
DM_INLINE_STATIC P petsc_vecmat_sync(void) {
  P retc;
  if ((retc = op_petsc_mat_syncto()))
    return retc;
  
  moveframe(o_1, o_2);
  FREEopds = o_1;
  return OK;
}

// vec-handle | max(v)
DM_INLINE_STATIC P petsc_vec_max(void) {
  Vec x;
  D v;
  PetscInt p;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);
  DMPETSC_ERRCHECK(VecMax(x, &p, &v));

  TAG(o_1) = (NUM|DOUBLETYPE);
  ATTR(o_1) = 0;
  *(D*)NUM_VAL(o_1) = v;
  return OK;
}

// vec-handle | min(v)
DM_INLINE_STATIC P petsc_vec_min(void) {
  Vec x;
  D v;
  PetscInt p;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);
  DMPETSC_ERRCHECK(VecMin(x, &p, &v));

  TAG(o_1) = (NUM|DOUBLETYPE);
  ATTR(o_1) = 0;
  *(D*)NUM_VAL(o_1) = v;
  return OK;
}
  
// vec-handle | --
DM_INLINE_STATIC P petsc_vec_destroy(void) {
  Vec x;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);

  DMPETSC_ERRCHECK(VecDestroy(x));
  DMPETSC_VECTOR_VECTOR(o_1) = NULL;

  KILL_OPAQUE();
  return OK;
}

static UL64 dupid = 1;

typedef P (*PreassFunc)(Mat A, B *restrict pframe, void *restrict arg);
typedef P (*MatCopytoFunc)(Mat A, P grow, D *restrict row, L32 ncols);
typedef P (*MatGetrowFunc)(Mat A, L32 *restrict ncols, L32 growi);
typedef P (*MatGetnzsFunc)(Mat A, L32 *restrict nzs, L32 gm, L32 m);

DM_INLINE_STATIC P petsc_mat_dense_copyto(
  Mat A, P grow, 
  D* restrict row, L32 ncols) 
{
  const PetscInt idxm[] = {grow};
  PetscInt* const idxn = (PetscInt*) (row + ncols);
  PetscInt* c;
  PetscInt i;

  if ((B*) idxn >= CEILvm) return VM_OVF;
  for (c = idxn, i = 0; c < idxn+ncols; c++, i++) *c = i;
    
  DMPETSC_ERRCHECK(MatSetValues(A, 1, idxm, ncols, idxn, row, 
				INSERT_VALUES));

  return OK;
}

DM_INLINE_STATIC P petsc_mat_sparse_copyto(
  Mat A, P grow, D* restrict row, 
  L32 ncols __attribute__ ((__unused__)) ) 
{
  DMPETSC_ERRCHECK(MatSetValuesRow(A, grow, row));
  return OK;
}

DM_INLINE_STATIC P petsc_mat_getrow(Mat A, L32 *restrict ncols, L32 gm) 
{
  const D* data;
  DMPETSC_ERRCHECK(MatGetRow(A, gm, ncols, PETSC_NULL, &data));
  if (FREEvm + sizeof(D)*(*ncols) > CEILvm) {
    MatRestoreRow(A, gm, ncols, PETSC_NULL, &data);
    return VM_OVF;
  }
  moveD((D*) data, (D*) FREEvm, *ncols);
  DMPETSC_ERRCHECK(MatRestoreRow(A, gm, ncols, PETSC_NULL, &data));

  return OK;
}

DM_INLINE_STATIC P petsc_mat_sparse_getnzs(Mat A, L32 *restrict nzs, 
					   L32 gm, L32 m) {
  L32 ncols;
  L32 i;
  *nzs = 0;
  for (i = gm; i < gm+m; i++) {
    DMPETSC_ERRCHECK(MatGetRow(A, i, &ncols, PETSC_NULL, PETSC_NULL));
    *nzs += ncols;
    DMPETSC_ERRCHECK(MatRestoreRow(A, i, &ncols, PETSC_NULL, PETSC_NULL));
  }
  return OK;
}

DM_INLINE_STATIC P petsc_mat_getvalues(Mat A, L32 *restrict ncols, L32 gm) 
{
  PetscInt* idxn;
  L32 i;
  DMPETSC_ERRCHECK(MatGetSize(A, PETSC_NULL, ncols));
  if ((idxn = (PetscInt*) (((D*)FREEvm) + *ncols)) + *ncols
      >= (PetscInt*) CEILvm)
    return VM_OVF;

  for (i = 0; i < *ncols; ++i) idxn[i] = i;
  DMPETSC_ERRCHECK(MatGetValues(A, 1, &gm, *ncols, idxn, (D*) FREEvm));

  return OK;
}

DM_INLINE_STATIC P petsc_mat_dense_getnzs(Mat A, L32 *restrict nzs,
					  L32 gm __attribute__((__unused__)),
					  L32 m)
{
  DMPETSC_ERRCHECK(MatGetSize(A, PETSC_NULL, nzs));
  *nzs *= m;
  return OK;
}

struct SparsePreass {
  L32 *restrict irows;
  L32 *restrict icols;
};
  
DM_INLINE_STATIC P petsc_mat_sparse_preass(
  Mat A,
  B* restrict pframe __attribute__ ((__unused__)), 
  void* restrict arg)
{
  struct SparsePreass *const params = (struct SparsePreass*) arg;

  DMPETSC_ERRCHECK(MatSetOption(A, MAT_DO_NOT_USE_INODES));
  DMPETSC_ERRCHECK(MatMPIAIJSetPreallocationCSR(A, 
						params->irows, 
						params->icols, 
						PETSC_NULL));
  return OK;
}

struct BlockDensePreass {
  PetscInt bs;
  PetscInt nb;
  PetscInt Nb;
};

DM_INLINE_STATIC P petsc_mat_blockdense_preass(
  Mat A,
  B* restrict pframe __attribute__ ((__unused__)), 
  void* restrict arg) 
{
  struct BlockDensePreass *params = (struct BlockDensePreass*) arg;
  DMPETSC_ERRCHECK(MatMPIBAIJSetPreallocation(A, params->bs, 
					      params->nb, PETSC_NULL,
					      params->Nb, PETSC_NULL));
  return OK;
}

DM_INLINE_STATIC P petsc_mat_dense_preass(
  Mat A,
  B* restrict pframe __attribute__ ((__unused__)), 
  void* restrict arg __attribute__ ((__unused__)) )
{
  DMPETSC_ERRCHECK(MatSetOption(A, MAT_ROW_ORIENTED));
  return OK;
}

typedef P (*MatTransposeFunc)(Mat A, Mat *At);

DM_INLINE_STATIC P petsc_mat_dense_transpose(Mat A, Mat *At) {
  extern PetscErrorCode dm_MatTranspose_MPIDense(Mat A,Mat *matout);
 
  DMPETSC_ERRCHECK(dm_MatTranspose_MPIDense(A, At));
  return OK;
}

DM_INLINE_STATIC P petsc_mat_other_transpose(Mat A, Mat *At) {
  extern PetscErrorCode dm_MatTranspose_MPIAIJ(Mat A,Mat *matout);

  DMPETSC_ERRCHECK(dm_MatTranspose_MPIAIJ(A, At));
  return OK;
}

// these are the types/indexes.
enum DMMatrixType {
  DENSETYPE = 0,
  SPARSETYPE = 1,
  BLOCKDENSETYPE = 2
};

static const struct {
  char *restrict type;
  PreassFunc preass;
  MatCopytoFunc copyto;
  MatGetrowFunc getrow;
  MatGetnzsFunc getnzs;
  BOOLEAN hascsr;
  MatTransposeFunc trans;
} matType[] = {
  {
    MATMPIDENSE,
    petsc_mat_dense_preass,
    petsc_mat_dense_copyto,
    petsc_mat_getrow,
    petsc_mat_dense_getnzs,
    FALSE,
    petsc_mat_dense_transpose,
  },
  {
    MATMPIAIJ,
    petsc_mat_sparse_preass,
    petsc_mat_sparse_copyto,
    petsc_mat_getrow,
    petsc_mat_sparse_getnzs,
    TRUE,
    petsc_mat_other_transpose,
  },
  {
    MATMPIBAIJ,
    petsc_mat_blockdense_preass,
    petsc_mat_dense_copyto,
    petsc_mat_getvalues,
    petsc_mat_sparse_getnzs,
    TRUE,
    petsc_mat_other_transpose,
  },
};

DM_INLINE_STATIC P mat_create(
  P m, P n, enum DMMatrixType mtype, 
  B* restrict rpframe, void* restrict arg) 
{
  B* pframe;
  PetscErrorCode r;
  P retc = OK;
  B* oldfreevm = FREEvm;
  Mat* A = NULL;
  char* type = matType[mtype].type;
  PreassFunc preass = matType[mtype].preass;

  if (! dupid) RETURN_ERROR(DMPETSC_MATOVF);

  MAKE_DMPETSC_MATRIX(pframe);
  *(A = &DMPETSC_MATRIX_MATRIX(pframe)) = NULL;
  DMPETSC_MATRIX_M(pframe) = m;
  DMPETSC_MATRIX_N(pframe) = n;
  DMPETSC_MATRIX_DUPID(pframe) = dupid++;
  DMPETSC_MATRIX_MTYPE(pframe) = mtype;

  if ((r = MatCreate(PETSC_COMM_WORLD, A))) 
    goto err;
  if ((r = MatSetSizes(*A, m, n, PETSC_DETERMINE, PETSC_DETERMINE))) 
    goto err;
  if ((r = MatSetType(*A, type))) 
    goto err;
  if ((r = MatGetOwnershipRange(*A, &DMPETSC_MATRIX_GM(pframe), PETSC_NULL)))
    goto err;
  
  if (preass && (retc = preass(*A, pframe, arg)))
    goto err;

  if ((r = MatSetOption(*A, MAT_ROWS_SORTED)))
    goto err;
  if ((r = MatSetOption(*A, MAT_COLUMNS_SORTED)))
    goto err;
  if ((r = MatSetOption(*A, MAT_NO_NEW_NONZERO_LOCATIONS)))
    goto err;

  DMPETSC_MATRIX_ASS(pframe) = ASSEMBLING;
  if ((r = MatAssemblyBegin(*A, MAT_FINAL_ASSEMBLY)))
    goto err;

  moveframe(pframe, rpframe);
  return OK;

 err:
  if (A && *A) MatDestroy(*A);
  FREEvm = oldfreevm;
  if (retc) return retc;
  RETURN_ERROR(errmap[r]);
}

// m n | mat
DM_INLINE_STATIC P petsc_mat_dense_create(void) {
  static B pframe[FRAMEBYTES];
  P retc;
  P m, n;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM || CLASS(o_1) != NUM) 
    return RNG_CHK;
  if (! PVALUE(o_2, &m) || m > L32MAX
      || ! PVALUE(o_1, &n) || n > L32MAX) 
    return UNDF_VAL;
  if (m < 1 || n < 1)
    return RNG_CHK;

  if ((retc = mat_create(m, n, DENSETYPE, pframe, NULL)))
    return retc;

  moveframe(pframe, o_2);
  FREEopds = o_1;
  return OK;
}

// <l irows> <l icols> n | mat
DM_INLINE_STATIC P petsc_mat_sparse_create(void) {
  static B pframe[FRAMEBYTES];
  P retc;
  P m, n;
  struct SparsePreass params;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_3) != (ARRAY|LONG32TYPE) || TAG(o_2) != (ARRAY|LONG32TYPE))
    return OPD_ERR;
  if ((m = ARRAY_SIZE(o_3)-1) < 1) return RNG_CHK;
  if (! PVALUE(o_1, &n)) return UNDF_VAL;
  if (n > L32MAX || n < 1 || m > L32MAX) return RNG_CHK;

  params.irows = (L32*) VALUE_PTR(o_3);
  params.icols = (L32*) VALUE_PTR(o_2);
  if ((retc = mat_create(m, n, SPARSETYPE, pframe, &params)))
    return retc;

  moveframe(pframe, o_3);
  FREEopds = o_2;
  return OK;
}

// Stein's algorithm or binary gcd algorithm
// http://www.nist.gov/dads/HTML/binaryGCD.html
DM_INLINE_STATIC P gcd(P u, P v) {
  P g;

  // divide by two until we get an odd
  // u,v even: gcd(u,v) = 2*gcd(u/2, v/2)
  for (g = 0; ! ((u|v) & 1); ++g) {
    u >>= 1;
    v >>= 1;
  }

  // gcd(0, v) = v
  while (u)
    // u even, v odd: gcd(u,v) = gcd(u/2, v)
    if (! (u&1)) u >>= 1;
    else if (! (v&1)) v >>= 1;
    // u,v odd: gcd(u,v) = gcd(|u-v|/2, v)
    else {
      P t = (u-v) >> 1;
      if (u < v) v = -t;
      else u = t;
    }
  
  return v << g;
}

// m n M N | mat
DM_INLINE_STATIC P petsc_mat_blockdense_create(void) {
  static B pframe[FRAMEBYTES];
  P m, n, M, N;
  struct BlockDensePreass params;
  P retc;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_4) != NUM 
      || CLASS(o_3) != NUM 
      || CLASS(o_2) != NUM 
      || CLASS(o_1) != NUM) 
    return OPD_CLA;
  if (! PVALUE(o_1, &N)
      || ! PVALUE(o_2, &M)
      || ! PVALUE(o_3, &n) 
      || ! PVALUE(o_4, &m))
    return UNDF_VAL;
  if (N > L32MAX || M > L32MAX
      || n < 1 || m < 1 
      || m > M || n > N)
    return RNG_CHK;

  params.bs = (PetscInt) gcd(gcd(M-m, m), gcd(N-n, n));
  params.nb = (PetscInt) (n/params.bs);
  params.Nb = (PetscInt) ((N-n)/params.bs);

  if ((retc = mat_create(m, n, BLOCKDENSETYPE, pframe, &params)))
    return retc;

  moveframe(pframe, o_4);
  FREEopds = o_3;
  return OK;
}
  

DM_INLINE_STATIC P petsc_mat_destroy(void) {
  Mat A;
  
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);
  
  DMPETSC_ERRCHECK(MatDestroy(A));
  DMPETSC_MATRIX_MATRIX(o_1) = NULL;
  
  KILL_OPAQUE();
  return OK;
}

// A B | B
DM_INLINE_STATIC P petsc_mat_copy(void) {
  Mat A, B;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_2, A);
  INIT_MAT(o_1, B);

  DMPETSC_ERRCHECK(MatCopy(A, B, SAME_NONZERO_PATTERN));

  moveframe(o_1, o_2);
  FREEopds = o_1;
  return OK;
}

// row column A | d
DM_INLINE_STATIC P petsc_mat_get(void) {
  Mat A;
  P r, c;
  L32 r32, c32;
  D v;
  
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM || CLASS(o_3) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &c) || ! PVALUE(o_3, &r)) return UNDF_VAL;
  if (r < 0 || c < 0) return RNG_CHK;

  INIT_MAT(o_1, A);
  if (r >=  DMPETSC_MATRIX_M(o_1) || c >= DMPETSC_MATRIX_N(o_1))
    RETURN_ERROR(DMPETSC_NONLOCAL);
  
  r32 = r;
  c32 = c;
  DMPETSC_ERRCHECK(MatGetValues(A, 1, &r32, 1, &c32, &v));

  TAG(o_3) = NUM|DOUBLETYPE;
  ATTR(o_3) = 0;
  DOUBLE_VAL(o_3) = v;
  FREEopds = o_2;
  return OK;
}

// A | B(same shape as A)
DM_INLINE_STATIC P petsc_mat_dup(void) {
  PetscErrorCode r;
  P retc = OK;
  Mat A;
  B* pframe;
  Mat* Bm = NULL;
  B* oldfreevm = FREEvm;
  L32* row;
  
  if (! dupid) RETURN_ERROR(DMPETSC_MATOVF);

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);

  MAKE_DMPETSC_MATRIX(pframe);
  *(Bm = &DMPETSC_MATRIX_MATRIX(pframe)) = NULL;
  DMPETSC_MATRIX_M(pframe) = DMPETSC_MATRIX_M(o_1);
  DMPETSC_MATRIX_N(pframe) = DMPETSC_MATRIX_N(o_1);
  DMPETSC_MATRIX_DUPID(pframe) = DMPETSC_MATRIX_DUPID(o_1);
  DMPETSC_MATRIX_MTYPE(pframe) = DMPETSC_MATRIX_MTYPE(o_1);

  row = &DMPETSC_MATRIX_GM(pframe);
  if ((r = MatDuplicate(A, MAT_DO_NOT_COPY_VALUES, Bm))
      || (r = MatGetOwnershipRange(*Bm, row, PETSC_NULL)))
    goto err;
  
  if (DMPETSC_MATRIX_GM(o_1) != *row) {
    retc = DMPETSC_ILLEGAL_OWNERSHIP;
    goto err;
  }

  DMPETSC_MATRIX_ASS(pframe) = ASSEMBLED;
  moveframe(pframe, o_1);
  return OK;

 err:
  if (Bm && *Bm) MatDestroy(*Bm);
  FREEvm = oldfreevm;
  RETURN_ERROR(retc ? retc : errmap[r]);
}

// A | A
DM_INLINE_STATIC P petsc_mat_syncto(void) {
  Mat A;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);

  return OK;
}

// A | A
DM_INLINE_STATIC P petsc_mat_syncfill(void) {
  Mat A;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT_WRITE(o_1, A);

  return OK;
}

// A | A
DM_INLINE_STATIC P petsc_mat_endfill(void) {
  Mat A;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT_WRITEEND(o_1, A);
  return OK;
}

// d n m A | A
DM_INLINE_STATIC P petsc_mat_fillone(void) {
  Mat A;
  P m, n;
  L32 m32, n32;
  D v;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM 
      || CLASS(o_3) != NUM
      || CLASS(o_4) != NUM)
    return OPD_CLA;
  if (!PVALUE(o_2, &m) || !PVALUE(o_3, &n)) return UNDF_VAL;
  if (m < 0 || n < 0) return RNG_CHK;
  DVALUE(o_4, &v);

  INIT_MAT_WRITE(o_1, A);
  if (m >= DMPETSC_MATRIX_M(o_1) || n >= DMPETSC_MATRIX_N(o_1))
    return RNG_CHK;
  
  m32 = m;
  n32 = n;
  DMPETSC_ERRCHECK(MatSetValues(A, 1, &m32, 1, &n32, &v, INSERT_VALUES));
  
  moveframe(o_1, o_4);
  FREEopds = o_3;
  return OK;
} 

// <d > <l icols> m A | A
DM_INLINE_STATIC P petsc_mat_fill(void) {
  Mat A;
  L32* icols;
  D* row;
  P m;
  P n;
  L32 irows;

  if (o_4 < FLOORopds) return OPDS_UNF;

  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &m)) return UNDF_VAL;
  if (TAG(o_3) != (ARRAY|LONG32TYPE)) return OPD_ERR;
  if (TAG(o_4) != (ARRAY|DOUBLETYPE)) return OPD_ERR;

  INIT_MAT_WRITE(o_1, A);
  if (m < 0 || m >= DMPETSC_MATRIX_M(o_1)) return RNG_CHK;

  icols = (L32*) VALUE_PTR(o_3);

  if ((n = ARRAY_SIZE(o_4)) != ARRAY_SIZE(o_3)) return RNG_CHK;
  if (n < 1 || n > L32MAX) return RNG_CHK;
  row = (D*) VALUE_PTR(o_4);

  irows = DMPETSC_MATRIX_GM(o_1) + (L32) m;
  DMPETSC_ERRCHECK(MatSetValues(A, 1, &irows,
				(L32) n, icols, 
				row, INSERT_VALUES));

  moveframe(o_1, o_4);
  FREEopds = o_3;
  return OK;
}

// <d > m n A | A
DM_INLINE_STATIC P petsc_mat_copyto(void) {
  P retc;
  Mat A;
  P m, n, len_arr, gm;
  L32 ncols;
  D* a;
  D* row;
  MatCopytoFunc copyto;
  MatGetrowFunc getrow;

  if (o_4 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);
  if (CLASS(o_2) != NUM || CLASS(o_3) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &n) || ! PVALUE(o_3, &m)) return UNDF_VAL;
  if (n < 0) return RNG_CHK;
  if (TAG(o_4) != (ARRAY|DOUBLETYPE)) return OPD_ERR;
  if (m >= DMPETSC_MATRIX_M(o_1) || m < 0) RETURN_ERROR(DMPETSC_NONLOCAL);

  len_arr = ARRAY_SIZE(o_4);
  gm = DMPETSC_MATRIX_GM(o_1);
  getrow = matType[DMPETSC_MATRIX_MTYPE(o_1)].getrow;
  if ((retc = getrow(A, &ncols, gm+m))) return retc;
  if (n + len_arr > ncols) return RNG_CHK;
  a = (D*) VALUE_PTR(o_4);
  row = (D*) FREEvm;
  moveD(a, row+n, len_arr);

  copyto = matType[DMPETSC_MATRIX_MTYPE(o_1)].copyto;
  if ((retc = copyto(A, gm+m, row, ncols))) return retc;

  moveframe(o_1, o_4);
  FREEopds = o_3;
  return OK;
}

// A | A
DM_INLINE_STATIC P petsc_mat_syncfrom(void) {
  Mat A;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);
  
  return OK;
}

// A m n <d > | <d sub-array>
DM_INLINE_STATIC P petsc_mat_copyfrom(void) {
  P retc;
  Mat A;
  P m, n, len_arr, gm;
  L32 ncols, ncols_;
  D* row;
  MatGetrowFunc getrow;

  if (o_4 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_4, A);
  if (CLASS(o_2) != NUM || CLASS(o_3) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &n) || ! PVALUE(o_3, &m)) return UNDF_VAL;
  if (n < 0) return RNG_CHK;
  if (TAG(o_1) != (ARRAY|DOUBLETYPE)) return OPD_ERR;
  if (m >= DMPETSC_MATRIX_M(o_4) || m < 0) 
    RETURN_ERROR(DMPETSC_NONLOCAL);

  gm = DMPETSC_MATRIX_GM(o_4);
  getrow = matType[DMPETSC_MATRIX_MTYPE(o_4)].getrow;
  if ((retc = getrow(A, &ncols, gm+m))) return retc;
  len_arr = ARRAY_SIZE(o_1);
  if (n >= ncols) return RNG_CHK;
  ncols_ = (len_arr < ncols - n) ? len_arr : ncols - n;
	row = (D*) FREEvm;
	moveD(row+n, (D*) VALUE_PTR(o_1), ncols_);

  moveframe(o_1, o_4);
  ARRAY_SIZE(o_4) = ncols_;
  ATTR(o_4) &= ~PARENT;
  FREEopds = o_3;
  return OK;
}

typedef P (*KSPFunc)(KSP ksp, B *restrict param, B *restrict kspframe);
typedef P (*KSPPostFunc)(KSP ksp, B *restrict kspframe);
static const struct {
  KSPType t;
  KSPFunc f;
  KSPPostFunc pf;
} ksptypes[] = {
  {KSPRICHARDSON, NULL, NULL},
  {KSPCHEBYCHEV, NULL, NULL},
  {KSPCG, NULL, NULL},
  {KSPCGNE, NULL, NULL},
  {KSPSTCG, NULL, NULL},
  {KSPGLTR, NULL, NULL},
  {KSPGMRES, NULL, NULL},
  {KSPFGMRES, NULL, NULL},
  {KSPLGMRES, NULL, NULL},
  {KSPTCQMR, NULL, NULL},
  {KSPBCGS, NULL, NULL},
  {KSPBCGSL, NULL, NULL},
  {KSPCGS, NULL, NULL},
  {KSPTFQMR, NULL, NULL},
  {KSPCR, NULL, NULL},
  {KSPLSQR, NULL, NULL},
  {KSPPREONLY, NULL, NULL},
  {KSPQCG, NULL, NULL},
  {KSPBICG, NULL, NULL},
  {KSPMINRES, NULL, NULL},
  {KSPSYMMLQ, NULL, NULL},
  {KSPLCD, NULL, NULL}
};

DM_INLINE_STATIC P finalize_BJACOBI_ILU(
  KSP ksp, 
  B *restrict kspframe __attribute__ ((__unused__)) ) 
{
  PetscInt n_local;
  PC pc;
  KSP* subksp; 
  KSP* i;

  DMPETSC_ERRCHECK(KSPGetPC(ksp, &pc));
  DMPETSC_ERRCHECK(PCBJacobiGetSubKSP(pc, &n_local, PETSC_NULL, &subksp));
  for (i = subksp; i < subksp + n_local; i++) {
    PC subpc;
    DMPETSC_ERRCHECK(KSPSetType(*i, KSPPREONLY));
    DMPETSC_ERRCHECK(KSPGetPC(*i, &subpc));
    DMPETSC_ERRCHECK(PCSetType(subpc, PCILU));
    //DMPETSC_ERRCHECK(PCFactorSetReuseFill(subpc, PETSC_TRUE));
    //DMPETSC_ERRCHECK(PCFactorSetReuseOrdering(subpc, PETSC_TRUE));
  }

  return OK;
}

DM_INLINE_STATIC P finalize_BJACOBI_LU(
  KSP ksp, 
  B *restrict kspframe __attribute__ ((__unused__)) ) 
{
  PetscInt n_local;
  PC pc;
  KSP* subksp; 
  KSP* i;
  
  DMPETSC_ERRCHECK(KSPGetPC(ksp, &pc));
  DMPETSC_ERRCHECK(PCBJacobiGetSubKSP(pc, &n_local, PETSC_NULL, &subksp));
  for (i = subksp; i < subksp + n_local; i++) {
    PC subpc;
    DMPETSC_ERRCHECK(KSPSetType(*i, KSPPREONLY));
    DMPETSC_ERRCHECK(KSPGetPC(*i, &subpc));
    DMPETSC_ERRCHECK(PCSetType(subpc, PCLU));
  }

  return OK;
}

DM_INLINE_STATIC P setup_LU(
  PC pc __attribute__ ((__unused__)),
  B* restrict param,
  B* restrict kspframe __attribute__ ((__unused__)) ) 
{
  if (TAG(param) != NULLOBJ) return OPD_ERR;
  //DMPETSC_ERRCHECK(PCFactorSetUseInPlace(pc));
  return OK;
}

DM_INLINE_STATIC P setup_ILU(
  PC pc __attribute__ ((__unused__)),
  B* restrict param,
  B* restrict kspframe __attribute__ ((__unused__)) ) 
{
  if (TAG(param) != NULLOBJ) return OPD_ERR;
  //DMPETSC_ERRCHECK(PCFactorSetUseInPlace(pc));
  //DMPETSC_ERRCHECK(PCFactorSetReuseFill(pc, PETSC_TRUE));
  //DMPETSC_ERRCHECK(PCFactorSetReuseOrdering(pc, PETSC_TRUE));
  return OK;
}

DM_INLINE_STATIC P setup_ICC(
  PC pc __attribute__ ((__unused__)),
  B* restrict param,
  B* restrict kspframe __attribute__ ((__unused__)) )
{
  if (TAG(param) != NULLOBJ) return OPD_ERR;
  //DMPETSC_ERRCHECK(PCFactorSetUseInPlace(pc));
  //DMPETSC_ERRCHECK(PCFactorSetReuseFill(pc, PETSC_TRUE));
  //DMPETSC_ERRCHECK(PCFactorSetReuseOrdering(pc, PETSC_TRUE));
  return OK;
}

typedef P (*PCFunc)(PC pc, B *restrict param, B *restrict kspframe);
typedef P (*PCPostFunc)(KSP ksp, B *restrict kspframe);
static const struct {
  PCType t;
  PCFunc f;
  PCPostFunc pf;
} pctypes[] = {
  {PCNONE, NULL, NULL},
  {PCJACOBI, NULL, NULL},
  {PCSOR, NULL, NULL},
  {PCLU, setup_LU, NULL},
  {PCSHELL, NULL, NULL},
  {PCBJACOBI, NULL, NULL},
  {PCMG, NULL, NULL},
  {PCEISENSTAT, NULL, NULL},
  {PCILU, setup_ILU, NULL},
  {PCICC, setup_ICC, NULL},
  {PCASM, NULL, NULL},
  {PCKSP, NULL, NULL},
  {PCCOMPOSITE, NULL, NULL},
  {PCREDUNDANT, NULL, NULL},
  {PCSPAI, NULL, NULL},
  {PCNN, NULL, NULL},
  {PCCHOLESKY, NULL, NULL},
  {PCSAMG, NULL, NULL},
  {PCPBJACOBI, NULL, NULL},
  {PCMAT, NULL, NULL},
  {PCHYPRE, NULL, NULL},
  {PCFIELDSPLIT, NULL, NULL},
  {PCTFS, NULL, NULL},
  {PCML, NULL, NULL},
  {PCPROMETHEUS, NULL, NULL},
  {PCGALERKIN, NULL, NULL},
  {PCOPENMP, NULL, NULL},
  {PCBJACOBI, NULL, finalize_BJACOBI_ILU},
  {PCBJACOBI, NULL, finalize_BJACOBI_LU}
};

typedef PetscErrorCode (*monitorfunc)(KSP ksp, PetscInt i, 
				      PetscReal r, void* v);
static const struct {
  monitorfunc f;
} monitors[] = {
  {PETSC_NULL},
  {KSPMonitorTrueResidualNorm},
  {KSPMonitorSingularValue},
};

#define kspretc(retc_code) do {				\
    retc = retc_code;					\
    goto err;						\
  } while (0)

// ksptype ksparam pctype pcparams monitor | KSP
DM_INLINE_STATIC P petsc_ksp_create(void) {
  PetscErrorCode r;
  P retc = 0;
  B* pframe;
  KSP* ksp = NULL;
  PC pc;
  P ksptype, pctype;
  B* oldfreevm = FREEvm;
  B* pcparam; B* kspparam;
  P monitor;

  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_5) != NUM) return OPD_CLA;
  if (PVALUE(o_5, &ksptype) 
      && (ksptype < 0 
	  || ksptype >= (P) (sizeof(ksptypes)/sizeof(ksptypes[0]))))
    return RNG_CHK;
  kspparam = o_4;

  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (PVALUE(o_3, &pctype)
      && (pctype < 0 || pctype >= (P) (sizeof(pctypes)/sizeof(pctypes[0]))))
    return RNG_CHK;
  pcparam = o_2;

  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (PVALUE(o_1, &monitor)
      && (monitor < 0 
	  || monitor >= (P) (sizeof(monitors)/sizeof(monitors[0]))))
    return RNG_CHK;

  MAKE_DMPETSC_KSP(pframe);
  ksp = &DMPETSC_KSP_KSP(pframe);
  *ksp = NULL;
  DMPETSC_KSP_KSPTYPE(pframe) = ksptype;
  DMPETSC_KSP_PCTYPE(pframe) = pctype;
  DMPETSC_KSP_DUPID(pframe) = 0;
  DMPETSC_KSP_N(pframe) = 0;
  DMPETSC_KSP_PCSETUPFD(pframe) = NULL;
  DMPETSC_KSP_KSPSETUPFD(pframe) = NULL;

  if ((r = KSPCreate(PETSC_COMM_WORLD, ksp))) goto err;
  if ((r = KSPSetInitialGuessNonzero(*ksp, PETSC_TRUE))) goto err;
  if ((r = KSPMonitorSet(*ksp, 
			 ((monitor == PINF) 
			  ? KSPMonitorDefault : monitors[monitor].f),
			 PETSC_NULL, PETSC_NULL))) goto err;

  if (ksptype != PINF) {
    if ((r = KSPSetType(*ksp, ksptypes[ksptype].t))) goto err;
    if (ksptypes[ksptype].f) { 
      if ((retc = ksptypes[ksptype].f(*ksp, kspparam, pframe)))
	goto err;
    }
    else if (TAG(kspparam) != NULLOBJ) kspretc(OPD_ERR);
  }
  else if (TAG(kspparam) != NULLOBJ) kspretc(OPD_ERR);

  if (pctype != PINF) {
    if ((r = KSPGetPC(*ksp, &pc))) goto err;
    if ((r = PCSetType(pc, pctypes[pctype].t))) goto err;
    if (pctypes[pctype].f) {
      if ((retc = pctypes[pctype].f(pc, pcparam, pframe))) 
	goto err;
    }
    else if (TAG(pcparam) != NULLOBJ) kspretc(OPD_ERR);
  }
  else if (TAG(pcparam) != NULLOBJ) kspretc(OPD_ERR);

  moveframe(pframe, o_5);
  FREEopds = o_4;
  return OK;

 err:
  if (ksp && *ksp) {
    KSPDestroy(*ksp);
    *ksp = NULL;
  }
  FREEvm = oldfreevm;
  if (retc) return retc;
  RETURN_ERROR(errmap[r]);
}

#undef kspretc

// ksp | --
DM_INLINE_STATIC P petsc_ksp_destroy(void) {
  KSP ksp;
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_KSP(o_1, ksp);

  DMPETSC_ERRCHECK(KSPDestroy(ksp));
  DMPETSC_KSP_KSP(o_1) = NULL;

  KILL_OPAQUE();
  return OK;
}

// ksp rtol atol dtol maxits | --
DM_INLINE_STATIC P petsc_ksp_tol(void) {
  D rtol, atol, dtol;
  LBIG maxits;
  KSP ksp;

  if (o_5 < FLOORopds) return OPDS_UNF;
  INIT_KSP(o_5, ksp);

  if (CLASS(o_1) != NUM 
      || CLASS(o_2) != NUM 
      || CLASS(o_3) != NUM 
      || CLASS(o_4) != NUM) return OPD_CLA;

  VALUE(o_1, &maxits);
  DVALUE(o_2, &dtol);
  DVALUE(o_3, &atol);
  DVALUE(o_4, &rtol);

  DMPETSC_ERRCHECK(
     KSPSetTolerances(ksp, 
		      ! ISUNDEF(rtol) ? rtol : PETSC_DEFAULT,
		      ! ISUNDEF(atol) ? atol : PETSC_DEFAULT,
		      ! ISUNDEF(dtol) ? dtol : PETSC_DEFAULT,
		      maxits != LBIGINF ? maxits : PETSC_DEFAULT));

  FREEopds = o_5;
  return OK;
}

DM_INLINE_STATIC P petsc_ksp_pcsetup(B* kspframe, KSP ksp) {
  const P pctype = DMPETSC_KSP_PCTYPE(kspframe);
  if (pctype != PINF && pctypes[pctype].pf) 
    return pctypes[pctype].pf(ksp, kspframe);
  return OK;
}

DM_INLINE_STATIC P petsc_ksp_kspsetup(B* kspframe, KSP ksp) {
  const P ksptype = DMPETSC_KSP_KSPTYPE(kspframe);
  if (ksptype != PINF && ksptypes[ksptype].pf) 
    return ksptypes[ksptype].pf(ksp, kspframe);
  return OK;
}

// KSP A/null x b | x
DM_INLINE_STATIC P petsc_ksp_solve(void) {
  KSP ksp;
  Mat A;
  Vec x;
  Vec b;
  UL64* ldupid;
  L32* n;
  KSPConvergedReason rzn;

  if (o_4 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, b);
  INIT_VEC(o_2, x);
  if (TAG(o_3) != NULLOBJ) INIT_MAT(o_3, A);
  else A = NULL;
  INIT_KSP(o_4, ksp);

  ldupid = &DMPETSC_KSP_DUPID(o_4);
  n      = &DMPETSC_KSP_N(o_4);
  if (! A) {
    if (! *ldupid) RETURN_ERROR(DMPETSC_KSPSOLVE_NOINIT);
  }
  else if (! *ldupid) {
    *ldupid = DMPETSC_MATRIX_DUPID(o_3);
    *n      = DMPETSC_MATRIX_N(o_3);
  }
  else if (*ldupid != DMPETSC_MATRIX_DUPID(o_3))
    RETURN_ERROR(DMPETSC_KSPSOLVE_NODUP);

  if (*n !=  DMPETSC_VECTOR_N(o_2) || *n != DMPETSC_VECTOR_N(o_1)) 
    RETURN_ERROR(DMPETSC_NOMATCH);

  if (A) {
    P retc;
    DMPETSC_ERRCHECK(KSPSetOperators(ksp, A, A, SAME_NONZERO_PATTERN));
    DMPETSC_ERRCHECK(KSPSetUp(ksp));
    if ((retc = petsc_ksp_kspsetup(o_4, ksp))) return retc;
    if ((retc = petsc_ksp_pcsetup(o_4, ksp))) return retc;
  }

  DMPETSC_ERRCHECK(KSPSolve(ksp, b, x));
  DMPETSC_ERRCHECK(KSPGetConvergedReason(ksp, &rzn));
  if (rzn < 0) RETURN_ERROR(errmap[PETSC_ERR_MAX_VALUE + (-rzn)]);

  moveframe(o_2, o_4);
  FREEopds = o_3;
  return OK;
}

// ksp | iterations
DM_INLINE_STATIC P petsc_ksp_iterations(void) {
  KSP ksp;
  L32 its;
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_KSP(o_1, ksp);

  if (! DMPETSC_KSP_DUPID(o_1))
    RETURN_ERROR(DMPETSC_KSPSOLVE_NOINIT);
  DMPETSC_ERRCHECK(KSPGetIterationNumber(ksp, &its));

  TAG(o_1) = (NUM|LONG32TYPE);
  ATTR(o_1) = 0;
  LONG32_VAL(o_1) = abs(its);

  return OK;
}

DM_INLINE_STATIC P petsc_log_begin(void) {
  DMPETSC_ERRCHECK(PetscLogBegin());
  return OK;
}

// (dir) (filename) | --
DM_INLINE_STATIC P petsc_log_summary(void) {
  B* start = FREEvm;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (BYTETYPE|ARRAY)
      || TAG(o_2) != (BYTETYPE|ARRAY)) return OPD_ERR;

  if (start + ARRAY_SIZE(o_2) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_2), start, ARRAY_SIZE(o_2));
  start += ARRAY_SIZE(o_2);
  if (ARRAY_SIZE(o_2) > 0 && start[-1] != '/') *(start++) = '/';
  if (start + ARRAY_SIZE(o_1) + 1 >= CEILvm) return VM_OVF;
  moveB(VALUE_PTR(o_1), start, ARRAY_SIZE(o_1));
  start += ARRAY_SIZE(o_1);
  *start = '\0';
  
  DMPETSC_ERRCHECK(PetscLogPrintSummary(PETSC_COMM_WORLD, (char*) FREEvm));
  FREEopds = o_2;
  return OK;
}

// vecx | ||vecx||
DM_INLINE_STATIC P petsc_vec_norm(void) {
  Vec x;
  D norm;
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_VEC(o_1, x);
  DMPETSC_ERRCHECK(VecNorm(x, NORM_2, &norm));
  TAG(o_1) = (NUM|DOUBLETYPE);
  DOUBLE_VAL(o_1) = norm;
  return OK;
}


/*   { */
/*     PetscInt m1, n1, m2, n2, m3, n3; */
/*     P r = getworldrank(); */
/*     MatGetOwnershipRange(A, &m1, &n1); */
/*     MatGetLocalSize(A, &m2, &n2); */
/*     MatGetSize(A, &m3, &n3); */
/*     fprintf(stderr, "Post: Rank: %i, Range: (%i,%i), LSize: (%i,%i), Size(%i,%i)\n", */
/* 	    (int) r, (int) m1, (int) n1, (int) m2, (int) n2,  */
/* 	    (int) m3, (int) n3); */
/*   } */

// MatA | MatA(transposed)
DM_INLINE_STATIC P petsc_mat_transpose(void) {
  Mat A, At;
  L32 m, n, gm;
  P retc;
  MatTransposeFunc trans;

  if (! dupid) RETURN_ERROR(DMPETSC_MATOVF);
  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);

  //fprintf(stderr, "transpose: start\n");
  m = DMPETSC_MATRIX_M(o_1);
  n = DMPETSC_MATRIX_N(o_1);
  gm = DMPETSC_MATRIX_GM(o_1);
  trans = matType[DMPETSC_MATRIX_MTYPE(o_1)].trans;
  //fprintf(stderr, "transpose: m=%i, n=%i, gm=%i, mtype=%i\n",
  //	  (int) m, (int) n, (int) gm, (int) DMPETSC_MATRIX_MTYPE(o_1));

  if ((retc = trans(A, /*m == n ? PETSC_NULL :*/ &At))) {
    //fprintf(stderr, "transpose: err\n");
    return retc;
  }

  //fprintf(stderr, "transpose: destroy\n");
  //  if (m != n) {
    DMPETSC_ERRCHECK(MatDestroy(A));
    A = DMPETSC_MATRIX_MATRIX(o_1) = At;
    DMPETSC_MATRIX_M(o_1) = n;
    DMPETSC_MATRIX_N(o_1) = m;
    //fprintf(stderr, "transpose: ownershiprange\n");
    DMPETSC_ERRCHECK(MatGetOwnershipRange(A, &DMPETSC_MATRIX_GM(o_1), 
					  PETSC_NULL));
    //  }
  DMPETSC_MATRIX_DUPID(o_1) = dupid++;

  //fprintf(stderr, "transpose: return\n");
  return OK;
}

// MatA | number-of-local-nonzeros
DM_INLINE_STATIC P petsc_mat_getnzs(void) {
  Mat A;
  L32 nzs, gm, m;
  P retc;
  enum DMMatrixType mtype;
  MatGetnzsFunc getnzs;

  if (o_1 < FLOORopds) return OPDS_UNF;
  INIT_MAT(o_1, A);

  gm = DMPETSC_MATRIX_GM(o_1);
  m = DMPETSC_MATRIX_M(o_1);
  mtype = DMPETSC_MATRIX_MTYPE(o_1);
  getnzs = matType[mtype].getnzs;
  

  if ((retc = getnzs(A, &nzs, gm, m))) return retc;

  TAG(o_1) = (NUM|LONG32TYPE);
  ATTR(o_1) = 0;
  LONG32_VAL(o_1) = nzs;
  return OK;
}

// <l irows> <l icols> MatA | <l irows> <l icols>
// icols is allocated by getcsr
DM_INLINE_STATIC P petsc_mat_getcsr(void) {
  Mat A;
  L32 m, gm;
  L32* crows;
  L32* ccols;

  L32 ncols;
  const L32* cols;
  P i, j;
  P left, used;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_3) != (ARRAY|LONG32TYPE)
      || TAG(o_2) != (ARRAY|LONG32TYPE)) 
    return OPD_ERR;
  INIT_MAT(o_1, A);
  if (! matType[DMPETSC_MATRIX_MTYPE(o_1)].hascsr)
    RETURN_ERROR(DMPETSC_ILLEGAL_OP);

  gm = DMPETSC_MATRIX_GM(o_1);
  m = DMPETSC_MATRIX_M(o_1);
  if (ARRAY_SIZE(o_3) <= m) return RNG_CHK;  

  crows = (L32*) VALUE_PTR(o_3);
  ccols = (L32*) VALUE_PTR(o_2);
  left = ARRAY_SIZE(o_2);
  used = 0;

  for (i = 0; i < m; i++) {
    DMPETSC_ERRCHECK(MatGetRow(A, gm+i, &ncols, &cols, PETSC_NULL));
    if ((left -= ncols) < 0) return RNG_CHK;
    *(crows++) = used;
    used += ncols;
    for (j = 0; j < ncols; j++) *(ccols++) = cols[j];
    DMPETSC_ERRCHECK(MatRestoreRow(A, gm+i, &ncols, &cols, PETSC_NULL));
  }

  ARRAY_SIZE(o_2) = *crows = used;
  ATTR(o_2) &= ~PARENT;
  ARRAY_SIZE(o_3) = m+1;
  ATTR(o_3) &= ~PARENT;
  FREEopds = o_1;
  return OK;
}
  
  

// y beta A trans x alpha | y=beta*y+alpha*A'*x
// there are shortcuts for integers 1,0 in alpha,beta
DM_INLINE_STATIC P petsc_mat_vecmul(void) {
  Vec x, y;
  Mat A;
  BOOLEAN trans;
  D alpha, beta;
  BOOLEAN alpha_ = TRUE, beta_ = TRUE, 
    alpha_0 = FALSE, beta_0 = FALSE;

  if (o_6 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_3) != BOOL) return OPD_CLA;
  trans = BOOL_VAL(o_3);
  if (CLASS(o_1) != NUM || CLASS(o_5) != NUM)
    return OPD_CLA;

  if (TYPE(o_1) < SINGLETYPE) {
    P v;
    PVALUE(o_1, &v);
    alpha_ = (v == 1) ? FALSE : TRUE;
    alpha_0 = (v == 0) ? TRUE : FALSE;
  }

  if (TYPE(o_5) < SINGLETYPE) {
    P v;
    PVALUE(o_5, &v);
    beta_ = (v == 1) ? FALSE : TRUE;
    beta_0 = (v == 0) ? TRUE : FALSE;
  }

  if (alpha_ && ! alpha_0 && ! DVALUE(o_1, &alpha)) return UNDF_VAL;
  if (beta_ && ! beta_0 && ! DVALUE(o_5, &beta)) return UNDF_VAL;
  
  INIT_VEC(o_6, y); 
  if (beta_0) DMPETSC_ERRCHECK(VecSet(y, 0));
  else if (beta_) DMPETSC_ERRCHECK(VecScale(y, beta));

  if (! alpha_0) {
    INIT_MAT(o_4, A);
    INIT_VEC(o_2, x);

    if (trans
	? (DMPETSC_MATRIX_M(o_4) != DMPETSC_VECTOR_N(o_2)
	   || DMPETSC_MATRIX_N(o_4) != DMPETSC_VECTOR_N(o_6))
	: (DMPETSC_MATRIX_N(o_4) != DMPETSC_VECTOR_N(o_2)
	   || DMPETSC_MATRIX_M(o_4) != DMPETSC_VECTOR_N(o_6)))
      RETURN_ERROR(DMPETSC_NOMATCH);

    if (alpha_) DMPETSC_ERRCHECK(VecScale(x, alpha));
    if (trans)
      DMPETSC_ERRCHECK(MatMultTransposeAdd(A, x, y, y));
    else
      DMPETSC_ERRCHECK(MatMultAdd(A, x, y, y));
  }
  
  FREEopds = o_5;
  return OK;
}

#include "dm-dmpetsc-main.h"