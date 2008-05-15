#ifndef DM_PETSC_H
#define DM_PETSC_H

#define PLUGIN_NAME petsc

#include "../src/plugin.h"

#define PETSC_VECTOR_VECTOR_FRAME(dframe) OPAQUE_MEM(dframe, VECTOR_VECTOR_frame)

#define PETSC_VECTOR_VECTOR(dframe) (VECTOR_VAL(PETSC_VECTOR_VECTOR_FRAME(dframe)))

#define PETSC_VECTOR_VECTOR_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONGBIGTYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, VECTOR_VECTOR_frame, frame); \
} while (0)

#define PETSC_VECTOR_N_FRAME(dframe) OPAQUE_MEM(dframe, VECTOR_N_frame)

#define PETSC_VECTOR_N(dframe) (L32_VAL(PETSC_VECTOR_N_FRAME(dframe)))

#define PETSC_VECTOR_N_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, VECTOR_N_frame, frame); \
} while (0)

#define MAKE_PETSC_VECTOR(frame, size) do { \
  if (! (frame = make_opaque_frame(size, PETSC_VECTOR_frame, VECTOR_VECTOR_frame, VECTOR_N_frame, NULL))) \
    return VM_OVF; \
  PETSC_VECTOR_VECTOR_INIT(frame); \
  PETSC_VECTOR_N_INIT(frame); \
} while (0)

#define TEST_PETSC_VECTOR(frame) do { \
  if (TAG(frame) != (DICT|OPAQUETYPE)) return OPD_TYP;\
  if (! check_opaque_name(PETSC_VECTOR_frame, VALUE_PTR(frame))) return ILL_OPAQUE; \
} while (0)

#define PETSC_MATRIX_MATRIX_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_MATRIX_frame)

#define PETSC_MATRIX_MATRIX(dframe) (MATRIX_VAL(PETSC_MATRIX_MATRIX_FRAME(dframe)))

#define PETSC_MATRIX_MATRIX_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONGBIGTYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, MATRIX_MATRIX_frame, frame); \
} while (0)

#define PETSC_MATRIX_M_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_M_frame)

#define PETSC_MATRIX_M(dframe) (L32_VAL(PETSC_MATRIX_M_FRAME(dframe)))

#define PETSC_MATRIX_M_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, MATRIX_M_frame, frame); \
} while (0)

#define PETSC_MATRIX_N_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_N_frame)

#define PETSC_MATRIX_N(dframe) (L32_VAL(PETSC_MATRIX_N_FRAME(dframe)))

#define PETSC_MATRIX_N_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, MATRIX_N_frame, frame); \
} while (0)

#define PETSC_MATRIX_GM_FRAME(dframe) OPAQUE_MEM(dframe, MATRIX_GM_frame)

#define PETSC_MATRIX_GM(dframe) (L32_VAL(PETSC_MATRIX_GM_FRAME(dframe)))

#define PETSC_MATRIX_GM_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, MATRIX_GM_frame, frame); \
} while (0)

#define MAKE_PETSC_MATRIX(frame, size) do { \
  if (! (frame = make_opaque_frame(size, PETSC_MATRIX_frame, MATRIX_MATRIX_frame, MATRIX_M_frame, MATRIX_N_frame, MATRIX_GM_frame, NULL))) \
    return VM_OVF; \
  PETSC_MATRIX_MATRIX_INIT(frame); \
  PETSC_MATRIX_M_INIT(frame); \
  PETSC_MATRIX_N_INIT(frame); \
  PETSC_MATRIX_GM_INIT(frame); \
} while (0)

#define TEST_PETSC_MATRIX(frame) do { \
  if (TAG(frame) != (DICT|OPAQUETYPE)) return OPD_TYP;\
  if (! check_opaque_name(PETSC_MATRIX_frame, VALUE_PTR(frame))) return ILL_OPAQUE; \
} while (0)

#define PETSC_KSP_KSP_FRAME(dframe) OPAQUE_MEM(dframe, KSP_KSP_frame)

#define PETSC_KSP_KSP(dframe) (KSP_VAL(PETSC_KSP_KSP_FRAME(dframe)))

#define PETSC_KSP_KSP_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONGBIGTYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_KSP_frame, frame); \
} while (0)

#define PETSC_KSP_M_FRAME(dframe) OPAQUE_MEM(dframe, KSP_M_frame)

#define PETSC_KSP_M(dframe) (L32_VAL(PETSC_KSP_M_FRAME(dframe)))

#define PETSC_KSP_M_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_M_frame, frame); \
} while (0)

#define PETSC_KSP_KSPTYPE_FRAME(dframe) OPAQUE_MEM(dframe, KSP_KSPTYPE_frame)

#define PETSC_KSP_KSPTYPE(dframe) (L32_VAL(PETSC_KSP_KSPTYPE_FRAME(dframe)))

#define PETSC_KSP_KSPTYPE_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_KSPTYPE_frame, frame); \
} while (0)

#define PETSC_KSP_PCTYPE_FRAME(dframe) OPAQUE_MEM(dframe, KSP_PCTYPE_frame)

#define PETSC_KSP_PCTYPE(dframe) (L32_VAL(PETSC_KSP_PCTYPE_FRAME(dframe)))

#define PETSC_KSP_PCTYPE_INIT(dframe)do { \
  B frame[FRAMEBYTES]; \
  TAG(frame) = (NUM|LONG32TYPE); \
  ATTR(frame) = READONLY; \
  OPAQUE_MEM_SET(dframe, KSP_PCTYPE_frame, frame); \
} while (0)

#define MAKE_PETSC_KSP(frame, size) do { \
  if (! (frame = make_opaque_frame(size, PETSC_KSP_frame, KSP_KSP_frame, KSP_M_frame, KSP_KSPTYPE_frame, KSP_PCTYPE_frame, NULL))) \
    return VM_OVF; \
  PETSC_KSP_KSP_INIT(frame); \
  PETSC_KSP_M_INIT(frame); \
  PETSC_KSP_KSPTYPE_INIT(frame); \
  PETSC_KSP_PCTYPE_INIT(frame); \
} while (0)

#define TEST_PETSC_KSP(frame) do { \
  if (TAG(frame) != (DICT|OPAQUETYPE)) return OPD_TYP;\
  if (! check_opaque_name(PETSC_KSP_frame, VALUE_PTR(frame))) return ILL_OPAQUE; \
} while (0)

#define PETSC_DM_INVVEC (1L)
#define PETSC_DM_INVMAT (2L)
#define PETSC_DM_INVKSP (3L)
#define PETSC_DM_ILLEGAL_OWNERSHIP (4L)
#define PETSC_DM_NOMATCH (5L)
#define PETSC_DM_KSPSOLVE_NOINIT (6L)
#define PETSC_DM_ERR_MEM (7L)
#define PETSC_DM_ERR_SUP (8L)
#define PETSC_DM_ERR_SUP_SYS (9L)
#define PETSC_DM_ERR_ORDER (10L)
#define PETSC_DM_ERR_SIG (11L)
#define PETSC_DM_ERR_FP (12L)
#define PETSC_DM_ERR_COR (13L)
#define PETSC_DM_ERR_LIB (14L)
#define PETSC_DM_ERR_PLIB (15L)
#define PETSC_DM_ERR_MEMC (16L)
#define PETSC_DM_ERR_CONV_FAILED (17L)
#define PETSC_DM_ERR_USER (18L)
#define PETSC_DM_ERR_ARG_SIZ (19L)
#define PETSC_DM_ERR_ARG_IDN (20L)
#define PETSC_DM_ERR_ARG_WRONG (21L)
#define PETSC_DM_ERR_ARG_CORRUPT (22L)
#define PETSC_DM_ERR_ARG_OUTOFRANGE (23L)
#define PETSC_DM_ERR_ARG_BADPTR (24L)
#define PETSC_DM_ERR_ARG_NOTSAMETYPE (25L)
#define PETSC_DM_ERR_ARG_NOTSAMECOMM (26L)
#define PETSC_DM_ERR_ARG_WRONGSTATE (27L)
#define PETSC_DM_ERR_ARG_INCOMP (28L)
#define PETSC_DM_ERR_ARG_NULL (29L)
#define PETSC_DM_ERR_ARG_UNKNOWN_TYPE (30L)
#define PETSC_DM_ERR_ARG_DOMAIN (31L)
#define PETSC_DM_ERR_FILE_OPEN (32L)
#define PETSC_DM_ERR_FILE_READ (33L)
#define PETSC_DM_ERR_FILE_WRITE (34L)
#define PETSC_DM_ERR_FILE_UNEXPECTED (35L)
#define PETSC_DM_ERR_MAT_LU_ZRPVT (36L)
#define PETSC_DM_ERR_MAT_CH_ZRPVT (37L)

#define op_petsc_vec_create EXPORTNAME(op_petsc_vec_create)
P op_petsc_vec_create(void);

#define op_petsc_vec_copy EXPORTNAME(op_petsc_vec_copy)
P op_petsc_vec_copy(void);

#define op_petsc_vec_copyto EXPORTNAME(op_petsc_vec_copyto)
P op_petsc_vec_copyto(void);

#define op_petsc_vec_copyfrom EXPORTNAME(op_petsc_vec_copyfrom)
P op_petsc_vec_copyfrom(void);

#define op_petsc_vec_max EXPORTNAME(op_petsc_vec_max)
P op_petsc_vec_max(void);

#define op_petsc_vec_min EXPORTNAME(op_petsc_vec_min)
P op_petsc_vec_min(void);

#define op_petsc_vec_destroy EXPORTNAME(op_petsc_vec_destroy)
P op_petsc_vec_destroy(void);

#define op_petsc_mat_create EXPORTNAME(op_petsc_mat_create)
P op_petsc_mat_create(void);

#define op_petsc_mat_copy EXPORTNAME(op_petsc_mat_copy)
P op_petsc_mat_copy(void);

#define op_petsc_mat_copyto EXPORTNAME(op_petsc_mat_copyto)
P op_petsc_mat_copyto(void);

#define op_petsc_mat_copyfrom EXPORTNAME(op_petsc_mat_copyfrom)
P op_petsc_mat_copyfrom(void);

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

static B PETSC_VECTOR_frame[FRAMEBYTES];
static B* PETSC_VECTOR_string = "PETSC_VECTOR";
static B VECTOR_VECTOR_frame[FRAMEBYTES];
static B* VECTOR_VECTOR_string = "VECTOR_VECTOR";
static B VECTOR_N_frame[FRAMEBYTES];
static B* VECTOR_N_string = "VECTOR_N";
static B PETSC_MATRIX_frame[FRAMEBYTES];
static B* PETSC_MATRIX_string = "PETSC_MATRIX";
static B MATRIX_MATRIX_frame[FRAMEBYTES];
static B* MATRIX_MATRIX_string = "MATRIX_MATRIX";
static B MATRIX_M_frame[FRAMEBYTES];
static B* MATRIX_M_string = "MATRIX_M";
static B MATRIX_N_frame[FRAMEBYTES];
static B* MATRIX_N_string = "MATRIX_N";
static B MATRIX_GM_frame[FRAMEBYTES];
static B* MATRIX_GM_string = "MATRIX_GM";
static B PETSC_KSP_frame[FRAMEBYTES];
static B* PETSC_KSP_string = "PETSC_KSP";
static B KSP_KSP_frame[FRAMEBYTES];
static B* KSP_KSP_string = "KSP_KSP";
static B KSP_M_frame[FRAMEBYTES];
static B* KSP_M_string = "KSP_M";
static B KSP_KSPTYPE_frame[FRAMEBYTES];
static B* KSP_KSPTYPE_string = "KSP_KSPTYPE";
static B KSP_PCTYPE_frame[FRAMEBYTES];
static B* KSP_PCTYPE_string = "KSP_PCTYPE";
#endif //DM_PETSC_H

