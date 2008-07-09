#ifndef DM_DMPETSC_H
#define DM_DMPETSC_H

#define PLUGIN_NAME dmpetsc

#include "../src/plugin.h"

#define DMPETSC_VECTOR_VECTOR_FRAME(dframe) OPAQUE_MEM(dframe, VECTOR_VECTOR_frame)

#define DMPETSC_VECTOR_VECTOR(dframe) (VECTOR_VAL(DMPETSC_VECTOR_VECTOR_FRAME(dframe)))

#define DMPETSC_VECTOR_VECTOR_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONGBIGTYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, VECTOR_VECTOR_frame, frame); \
} while (0)

#define DMPETSC_VECTOR_N_FRAME(dframe) OPAQUE_MEM(dframe, VECTOR_N_frame)

#define DMPETSC_VECTOR_N(dframe) (LONG32_VAL(DMPETSC_VECTOR_N_FRAME(dframe)))

#define DMPETSC_VECTOR_N_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, VECTOR_N_frame, frame); \
} while (0)

#define DMPETSC_VECTOR_GN_FRAME(dframe) OPAQUE_MEM(dframe, VECTOR_GN_frame)

#define DMPETSC_VECTOR_GN(dframe) (LONG32_VAL(DMPETSC_VECTOR_GN_FRAME(dframe)))

#define DMPETSC_VECTOR_GN_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, VECTOR_GN_frame, frame); \
} while (0)

#define DMPETSC_VECTOR_ASS_FRAME(dframe) OPAQUE_MEM(dframe, VECTOR_ASS_frame)

#define DMPETSC_VECTOR_ASS(dframe) (BOOL_VAL(DMPETSC_VECTOR_ASS_FRAME(dframe)))

#define DMPETSC_VECTOR_ASS_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (BOOL); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, VECTOR_ASS_frame, frame); \
} while (0)

#define MAKE_DMPETSC_VECTOR(frame) do { \
  if (! (frame = make_opaque_frame(0, DMPETSC_VECTOR_frame, VECTOR_VECTOR_frame, VECTOR_N_frame, VECTOR_GN_frame, VECTOR_ASS_frame, NULL))) \
    return VM_OVF; \
  DMPETSC_VECTOR_VECTOR_INIT(frame); \
  DMPETSC_VECTOR_N_INIT(frame); \
  DMPETSC_VECTOR_GN_INIT(frame); \
  DMPETSC_VECTOR_ASS_INIT(frame); \
} while (0)

#define TEST_DMPETSC_VECTOR(frame) do { \
  if (TAG(frame) != (DICT|OPAQUETYPE)) return OPD_TYP;\
  if (! check_opaque_name(DMPETSC_VECTOR_frame, VALUE_PTR(frame))) return ILL_OPAQUE; \
} while (0)

#define DMPETSC_MATRIX_MATRIX_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_MATRIX_frame)

#define DMPETSC_MATRIX_MATRIX(dframe) (MATRIX_VAL(DMPETSC_MATRIX_MATRIX_FRAME(dframe)))

#define DMPETSC_MATRIX_MATRIX_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONGBIGTYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, MATRIX_MATRIX_frame, frame); \
} while (0)

#define DMPETSC_MATRIX_M_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_M_frame)

#define DMPETSC_MATRIX_M(dframe) (LONG32_VAL(DMPETSC_MATRIX_M_FRAME(dframe)))

#define DMPETSC_MATRIX_M_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, MATRIX_M_frame, frame); \
} while (0)

#define DMPETSC_MATRIX_N_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_N_frame)

#define DMPETSC_MATRIX_N(dframe) (LONG32_VAL(DMPETSC_MATRIX_N_FRAME(dframe)))

#define DMPETSC_MATRIX_N_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, MATRIX_N_frame, frame); \
} while (0)

#define DMPETSC_MATRIX_GM_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_GM_frame)

#define DMPETSC_MATRIX_GM(dframe) (LONG32_VAL(DMPETSC_MATRIX_GM_FRAME(dframe)))

#define DMPETSC_MATRIX_GM_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, MATRIX_GM_frame, frame); \
} while (0)

#define DMPETSC_MATRIX_ASS_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_ASS_frame)

#define DMPETSC_MATRIX_ASS(dframe) (BOOL_VAL(DMPETSC_MATRIX_ASS_FRAME(dframe)))

#define DMPETSC_MATRIX_ASS_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (BOOL); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, MATRIX_ASS_frame, frame); \
} while (0)

#define DMPETSC_MATRIX_DUPID_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_DUPID_frame)

#define DMPETSC_MATRIX_DUPID(dframe) (ULONG64_VAL(DMPETSC_MATRIX_DUPID_FRAME(dframe)))

#define DMPETSC_MATRIX_DUPID_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG64TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, MATRIX_DUPID_frame, frame); \
} while (0)

#define DMPETSC_MATRIX_MTYPE_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_MTYPE_frame)

#define DMPETSC_MATRIX_MTYPE(dframe) (LONG32_VAL(DMPETSC_MATRIX_MTYPE_FRAME(dframe)))

#define DMPETSC_MATRIX_MTYPE_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, MATRIX_MTYPE_frame, frame); \
} while (0)

#define MAKE_DMPETSC_MATRIX(frame) do { \
  if (! (frame = make_opaque_frame(0, DMPETSC_MATRIX_frame, MATRIX_MATRIX_frame, MATRIX_M_frame, MATRIX_N_frame, MATRIX_GM_frame, MATRIX_ASS_frame, MATRIX_DUPID_frame, MATRIX_MTYPE_frame, NULL))) \
    return VM_OVF; \
  DMPETSC_MATRIX_MATRIX_INIT(frame); \
  DMPETSC_MATRIX_M_INIT(frame); \
  DMPETSC_MATRIX_N_INIT(frame); \
  DMPETSC_MATRIX_GM_INIT(frame); \
  DMPETSC_MATRIX_ASS_INIT(frame); \
  DMPETSC_MATRIX_DUPID_INIT(frame); \
  DMPETSC_MATRIX_MTYPE_INIT(frame); \
} while (0)

#define TEST_DMPETSC_MATRIX(frame) do { \
  if (TAG(frame) != (DICT|OPAQUETYPE)) return OPD_TYP;\
  if (! check_opaque_name(DMPETSC_MATRIX_frame, VALUE_PTR(frame))) return ILL_OPAQUE; \
} while (0)

#define DMPETSC_KSP_KSP_FRAME(dframe) OPAQUE_MEM(dframe, KSP_KSP_frame)

#define DMPETSC_KSP_KSP(dframe) (KSP_VAL(DMPETSC_KSP_KSP_FRAME(dframe)))

#define DMPETSC_KSP_KSP_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONGBIGTYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_KSP_frame, frame); \
} while (0)

#define DMPETSC_KSP_N_FRAME(dframe) OPAQUE_MEM(dframe, KSP_N_frame)

#define DMPETSC_KSP_N(dframe) (LONG32_VAL(DMPETSC_KSP_N_FRAME(dframe)))

#define DMPETSC_KSP_N_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = 0; \
  OPAQUE_MEM_SET(dframe, KSP_N_frame, frame); \
} while (0)

#define DMPETSC_KSP_KSPTYPE_FRAME(dframe) OPAQUE_MEM(dframe, KSP_KSPTYPE_frame)

#define DMPETSC_KSP_KSPTYPE(dframe) (LONG32_VAL(DMPETSC_KSP_KSPTYPE_FRAME(dframe)))

#define DMPETSC_KSP_KSPTYPE_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_KSPTYPE_frame, frame); \
} while (0)

#define DMPETSC_KSP_PCTYPE_FRAME(dframe) OPAQUE_MEM(dframe, KSP_PCTYPE_frame)

#define DMPETSC_KSP_PCTYPE(dframe) (LONG32_VAL(DMPETSC_KSP_PCTYPE_FRAME(dframe)))

#define DMPETSC_KSP_PCTYPE_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_PCTYPE_frame, frame); \
} while (0)

#define DMPETSC_KSP_DUPID_FRAME(dframe) OPAQUE_MEM(dframe, KSP_DUPID_frame)

#define DMPETSC_KSP_DUPID(dframe) (ULONG64_VAL(DMPETSC_KSP_DUPID_FRAME(dframe)))

#define DMPETSC_KSP_DUPID_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG64TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_DUPID_frame, frame); \
} while (0)

#define MAKE_DMPETSC_KSP(frame) do { \
  if (! (frame = make_opaque_frame(0, DMPETSC_KSP_frame, KSP_KSP_frame, KSP_N_frame, KSP_KSPTYPE_frame, KSP_PCTYPE_frame, KSP_DUPID_frame, NULL))) \
    return VM_OVF; \
  DMPETSC_KSP_KSP_INIT(frame); \
  DMPETSC_KSP_N_INIT(frame); \
  DMPETSC_KSP_KSPTYPE_INIT(frame); \
  DMPETSC_KSP_PCTYPE_INIT(frame); \
  DMPETSC_KSP_DUPID_INIT(frame); \
} while (0)

#define TEST_DMPETSC_KSP(frame) do { \
  if (TAG(frame) != (DICT|OPAQUETYPE)) return OPD_TYP;\
  if (! check_opaque_name(DMPETSC_KSP_frame, VALUE_PTR(frame))) return ILL_OPAQUE; \
} while (0)

#define DMPETSC_MATOVF (1L)
#define DMPETSC_INVVEC (2L)
#define DMPETSC_INVMAT (3L)
#define DMPETSC_INVKSP (4L)
#define DMPETSC_ILLEGAL_OWNERSHIP (5L)
#define DMPETSC_NOMATCH (6L)
#define DMPETSC_NONLOCAL (7L)
#define DMPETSC_KSPSOLVE_NOINIT (8L)
#define DMPETSC_KSPSOLVE_NODUP (9L)
#define DMPETSC_ERR_MEM (10L)
#define DMPETSC_ERR_SUP (11L)
#define DMPETSC_ERR_SUP_SYS (12L)
#define DMPETSC_ERR_ORDER (13L)
#define DMPETSC_ERR_SIG (14L)
#define DMPETSC_ERR_FP (15L)
#define DMPETSC_ERR_COR (16L)
#define DMPETSC_ERR_LIB (17L)
#define DMPETSC_ERR_PLIB (18L)
#define DMPETSC_ERR_MEMC (19L)
#define DMPETSC_ERR_CONV_FAILED (20L)
#define DMPETSC_ERR_USER (21L)
#define DMPETSC_ERR_ARG_SIZ (22L)
#define DMPETSC_ERR_ARG_IDN (23L)
#define DMPETSC_ERR_ARG_WRONG (24L)
#define DMPETSC_ERR_ARG_CORRUPT (25L)
#define DMPETSC_ERR_ARG_OUTOFRANGE (26L)
#define DMPETSC_ERR_ARG_BADPTR (27L)
#define DMPETSC_ERR_ARG_NOTSAMETYPE (28L)
#define DMPETSC_ERR_ARG_NOTSAMECOMM (29L)
#define DMPETSC_ERR_ARG_WRONGSTATE (30L)
#define DMPETSC_ERR_ARG_INCOMP (31L)
#define DMPETSC_ERR_ARG_NULL (32L)
#define DMPETSC_ERR_ARG_UNKNOWN_TYPE (33L)
#define DMPETSC_ERR_ARG_DOMAIN (34L)
#define DMPETSC_ERR_FILE_OPEN (35L)
#define DMPETSC_ERR_FILE_READ (36L)
#define DMPETSC_ERR_FILE_WRITE (37L)
#define DMPETSC_ERR_FILE_UNEXPECTED (38L)
#define DMPETSC_ERR_MAT_LU_ZRPVT (39L)
#define DMPETSC_ERR_MAT_CH_ZRPVT (40L)
#define DMPETSC_DIVERGED_NULL (41L)
#define DMPETSC_DIVERGED_ITS (42L)
#define DMPETSC_DIVERGED_DTOL (43L)
#define DMPETSC_DIVERGED_BREAKDOWN (44L)
#define DMPETSC_DIVERGED_BREAKDOWN_BICG (45L)
#define DMPETSC_DIVERGED_NONSYMMETRIC (46L)
#define DMPETSC_DIVERGED_INDEFINITE_PC (47L)
#define DMPETSC_DIVERGED_NAN (48L)
#define DMPETSC_DIVERGED_INDEFINITE_MAT (49L)

#define op_petsc_vec_create EXPORTNAME(op_petsc_vec_create)
P op_petsc_vec_create(void);

#define op_petsc_vec_copy EXPORTNAME(op_petsc_vec_copy)
P op_petsc_vec_copy(void);

#define op_petsc_vec_copyto EXPORTNAME(op_petsc_vec_copyto)
P op_petsc_vec_copyto(void);

#define op_petsc_vec_copyfrom EXPORTNAME(op_petsc_vec_copyfrom)
P op_petsc_vec_copyfrom(void);

#define op_petsc_vec_syncto EXPORTNAME(op_petsc_vec_syncto)
P op_petsc_vec_syncto(void);

#define op_petsc_vec_syncfrom EXPORTNAME(op_petsc_vec_syncfrom)
P op_petsc_vec_syncfrom(void);

#define op_petsc_vec_max EXPORTNAME(op_petsc_vec_max)
P op_petsc_vec_max(void);

#define op_petsc_vec_min EXPORTNAME(op_petsc_vec_min)
P op_petsc_vec_min(void);

#define op_petsc_vec_destroy EXPORTNAME(op_petsc_vec_destroy)
P op_petsc_vec_destroy(void);

#define op_petsc_mat_sparse_create EXPORTNAME(op_petsc_mat_sparse_create)
P op_petsc_mat_sparse_create(void);

#define op_petsc_mat_dense_create EXPORTNAME(op_petsc_mat_dense_create)
P op_petsc_mat_dense_create(void);

#define op_petsc_mat_copy EXPORTNAME(op_petsc_mat_copy)
P op_petsc_mat_copy(void);

#define op_petsc_mat_copyto EXPORTNAME(op_petsc_mat_copyto)
P op_petsc_mat_copyto(void);

#define op_petsc_mat_copyfrom EXPORTNAME(op_petsc_mat_copyfrom)
P op_petsc_mat_copyfrom(void);

#define op_petsc_mat_syncto EXPORTNAME(op_petsc_mat_syncto)
P op_petsc_mat_syncto(void);

#define op_petsc_mat_syncfrom EXPORTNAME(op_petsc_mat_syncfrom)
P op_petsc_mat_syncfrom(void);

#define op_petsc_mat_destroy EXPORTNAME(op_petsc_mat_destroy)
P op_petsc_mat_destroy(void);

#define op_petsc_mat_dup EXPORTNAME(op_petsc_mat_dup)
P op_petsc_mat_dup(void);

#define op_petsc_mat_vecmul EXPORTNAME(op_petsc_mat_vecmul)
P op_petsc_mat_vecmul(void);

#define op_petsc_ksp_create EXPORTNAME(op_petsc_ksp_create)
P op_petsc_ksp_create(void);

#define op_petsc_ksp_destroy EXPORTNAME(op_petsc_ksp_destroy)
P op_petsc_ksp_destroy(void);

#define op_petsc_ksp_tol EXPORTNAME(op_petsc_ksp_tol)
P op_petsc_ksp_tol(void);

#define op_petsc_ksp_iterations EXPORTNAME(op_petsc_ksp_iterations)
P op_petsc_ksp_iterations(void);

#define op_petsc_ksp_solve EXPORTNAME(op_petsc_ksp_solve)
P op_petsc_ksp_solve(void);

static B DMPETSC_VECTOR_frame[FRAMEBYTES];
static B* DMPETSC_VECTOR_string = "DMPETSC_VECTOR";
static B VECTOR_VECTOR_frame[FRAMEBYTES];
static B* VECTOR_VECTOR_string = "VECTOR_VECTOR";
static B VECTOR_N_frame[FRAMEBYTES];
static B* VECTOR_N_string = "VECTOR_N";
static B VECTOR_GN_frame[FRAMEBYTES];
static B* VECTOR_GN_string = "VECTOR_GN";
static B VECTOR_ASS_frame[FRAMEBYTES];
static B* VECTOR_ASS_string = "VECTOR_ASS";
static B DMPETSC_MATRIX_frame[FRAMEBYTES];
static B* DMPETSC_MATRIX_string = "DMPETSC_MATRIX";
static B MATRIX_MATRIX_frame[FRAMEBYTES];
static B* MATRIX_MATRIX_string = "MATRIX_MATRIX";
static B MATRIX_M_frame[FRAMEBYTES];
static B* MATRIX_M_string = "MATRIX_M";
static B MATRIX_N_frame[FRAMEBYTES];
static B* MATRIX_N_string = "MATRIX_N";
static B MATRIX_GM_frame[FRAMEBYTES];
static B* MATRIX_GM_string = "MATRIX_GM";
static B MATRIX_ASS_frame[FRAMEBYTES];
static B* MATRIX_ASS_string = "MATRIX_ASS";
static B MATRIX_DUPID_frame[FRAMEBYTES];
static B* MATRIX_DUPID_string = "MATRIX_DUPID";
static B MATRIX_MTYPE_frame[FRAMEBYTES];
static B* MATRIX_MTYPE_string = "MATRIX_MTYPE";
static B DMPETSC_KSP_frame[FRAMEBYTES];
static B* DMPETSC_KSP_string = "DMPETSC_KSP";
static B KSP_KSP_frame[FRAMEBYTES];
static B* KSP_KSP_string = "KSP_KSP";
static B KSP_N_frame[FRAMEBYTES];
static B* KSP_N_string = "KSP_N";
static B KSP_KSPTYPE_frame[FRAMEBYTES];
static B* KSP_KSPTYPE_string = "KSP_KSPTYPE";
static B KSP_PCTYPE_frame[FRAMEBYTES];
static B* KSP_PCTYPE_string = "KSP_PCTYPE";
static B KSP_DUPID_frame[FRAMEBYTES];
static B* KSP_DUPID_string = "KSP_DUPID";
#endif //DM_DMPETSC_H

