#include "rthreads.h"
#include "dm-rthreads-main.h"

#include <mpi.h>
#include <stdlib.h>

#define me(arg) do {				\
    P r;					\
    if ((r = arg)) RETURN_ERROR(r);		\
  } while (0)

#define meg(arg) do {				\
    if ((r = arg)) {r |= ll_type; goto ERR;};	\
  } while (0)

static char* mpi_thread_comm = "@mpi_thread_comm@";

enum VarTypeEnum {VECTOR, MATRIX};

struct VarType {
  enum VarTypeEnum type;
  union {
    struct {
      P rows, cols;
    } m;
    struct {
      P len;
    } v;
  } info;
};

struct Var {
  LBIG data;
  struct VarType* type;
  struct Var* next;
  struct Var* last;
};

static int id = -1;
static int parentid = -1;
static MPI_Comm intercomm = MPI_COMM_NULL;
static MPI_Comm intracomm = MPI_COMM_NULL;
static struct Var vars = {0, NULL, NULL, NULL};
static LBIG nvars = 0;
static BOOLEAN running = FALSE;

DM_INLINE_STATIC void kill_vars(void) {
  struct Var* next = vars.next;
  struct Var* nnext = NULL;

  if (parentid != -1) return;

  while (next) {
    nnext = next->next;
    free(next->type);
    free(next);
    next = nnext;
  }
  vars.next = NULL;
  vars.last = NULL;
}

DM_INLINE_STATIC BOOLEAN make_var(LBIG data, struct VarType* type) {
  struct Var* next;

  if (! (next = (struct Var*) malloc(sizeof(struct Var))))
    return FALSE;

  next->type = type;
  next->next = NULL;
  next->last = vars.last;
  next->data = data;
  
  if (! vars.last) vars.next = vars.last = next;
  else {
    vars.last->next = next;
    vars.last = next;
  }

  return TRUE;
}

typedef void Vector;
typedef void Matrix;

DM_INLINE_STATIC BOOLEAN make_vector(Vector* v, P len) {
  struct VarType* vtype = (struct VarType*) malloc(sizeof(struct VarType));
  if (! vtype) return FALSE;
  
  vtype->type = VECTOR;
  vtype->info.v.len = len;
  return make_var((P) v, vtype);
}

DM_INLINE_STATIC BOOLEAN make_matrix(Matrix* m, P rows, P cols) {
  struct VarType* mtype = (struct VarType*) malloc(sizeof(struct VarType));
  if (! mtype) return FALSE;

  mtype->type = MATRIX;
  mtype->info.m.rows = rows;
  mtype->info.m.cols = cols;
  return make_var((P) m, mtype);
}

DM_INLINE_STATIC void kill_var(P n) {
  struct Var* next = vars.next;
  while (n-- && next) next = next->next;
  if (next == vars.next) vars.next = next->next;
  if (next == vars.last) vars.last = next->last;
  free(next->type);
  free(next);
}

DM_INLINE_STATIC P find_var(LBIG data) {
  struct Var* next = vars.next;
  P n = 0;
  while (next && next->data != data) {
    n++;
    next = next->next;
  }

  return next ? n : -1;
}

DM_INLINE_STATIC struct Var* get_var(P n) {
  struct Var* next = vars.next;
  while (n-- && next) next = next->next;
  return next;
}

DM_INLINE_STATIC P init_(void) {
  int flag;
  me(MPI_Initialized(&flag));

  return flag ? OK : MPI_Init(NULL, NULL);
}

#define DM_MPI_QUIT 0
static P dm_mpi_quit(LBIG* data) {
  kill_vars();
  return parentid != -1 ? QUIT : OK;
}

static B paramsize[] = {0};

static P (*funcs[])(LBIG* data) = {dm_mpi_quit};

DM_INLINE_STATIC P send_mpicommand(B command, LBIG* params) {
  me(MPI_Bcast(&command, 1, MPI_UNSIGNED_CHAR, 
	       MPI_ROOT, intercomm));
  me(MPI_Bcast(params, paramsize[command], MPI_UNSIGNED_LONG, 
	       MPI_ROOT, intercomm));
  
  return funcs[command](params);
}

DM_INLINE_STATIC P killrthreads(void) {
  static LBIG data[0] = {};
  P r, r0, r1, r2;
  if (! running) return OK;
  
  r0 = send_mpicommand(DM_MPI_QUIT, data);
  r1 = MPI_Comm_disconnect(&intracomm);
  r2 = MPI_Comm_disconnect(&intercomm);
  running = FALSE;
  if (r0) return r0;
  if (r1) RETURN_ERROR(r1);
  if (r2) RETURN_ERROR(r2);
  return OK;
}

DM_INLINE_STATIC P fini_(void) {
  return killrthreads();
}

P child_main(const char* parent_rank) {
  P r, r1, r2, r3;
  B command;
  LBIG* params = NULL;

  if (! sscanf(parent_rank, "%i", &parentid)) return RTHREADS_PARENT_RANK_MISSING;
  meg(MPI_Init(NULL, NULL));

  meg(MPI_Comm_get_parent(&intercomm));
  meg(MPI_Bcast(&nvars, 1, MPI_UNSIGNED_LONG, 
		parentid, intercomm));

  meg(MPI_Intercomm_merge(intercomm, 1, &intracomm));
  meg(MPI_Comm_rank(intracomm, &id));
  
  while (1) {
    meg(MPI_Bcast(&command, 1, MPI_UNSIGNED_CHAR, 
		  parentid, intercomm));

    params = (LBIG*) malloc(paramsize[command]*sizeof(LBIG));
    meg(MPI_Bcast(params, paramsize[command], MPI_UNSIGNED_LONG, 
		  parentid, intercomm));

    if ((r = funcs[command](params))) goto ERR;
    free(params);
  }

 ERR:
  if (r == QUIT) r = OK;

  r &= ~(ll_type);
  if (params) free(params);
  r1 = MPI_Comm_disconnect(&intracomm);
  r2 = MPI_Comm_disconnect(&intercomm);
  r3 = MPI_Finalize();

  return r ? r : r1 ? r1 : r2 ? r2 : r3 ? r3 : OK;
}

/* --------------------------------------- makerthreads
   #vars #threads | --
*/
DM_INLINE_STATIC P makerthreads(void) {
  LBIG threads;
  int* errors;
  P r = OK;
  B* mpiframe; B* lframe; B* lsframe;
  int i;
  B* oldfreevm = FREEvm;

  char  rank[10];
  char* argv[] = {NULL, NULL};

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM || CLASS(o_2) != NUM) return OPD_CLA;
  if (! VALUE(o_1, &threads) || ! VALUE(o_2, &nvars)) return UNDF_VAL;

  if ((r = killrthreads())) return r;

  me(MPI_Comm_rank(MPI_COMM_WORLD, &id));
  if (snprintf(rank, sizeof(rank), "%i", id) >= sizeof(rank))
    RETURN_ERROR(RTHREADS_RANK_PRINTF_ERR);
  argv[0] = rank;
  
  if (! (errors = (int*) malloc(threads*sizeof(int)))) {
    r = MEM_OVF;
    goto ERR;
  }

  meg(MPI_Comm_spawn(mpi_thread_comm, argv, threads, MPI_INFO_NULL, 
		     id, MPI_COMM_WORLD, &intercomm, errors));
  running = TRUE;

  meg(MPI_Intercomm_merge(intercomm, 0, &intracomm));
  meg(MPI_Comm_rank(intracomm, &id));

  FREEopds = o_2;
  oldfreevm = FREEvm;

 ERR:
  if (errors) free(errors);
  FREEvm = oldfreevm;
  return r;
}
