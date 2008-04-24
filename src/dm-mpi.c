#include <mpi.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>

#include "dm-mpi.h"
#include "error-local.h"

static MPI_Comm rook = MPI_COMM_NULL;
static MPI_Comm world = MPI_COMM_NULL;
static P rank = -1;
static P universe = 0;

MPI_Comm getworldcomm(void) {return world;}
P getworldsize(void) {return universe;}
P getworldrank(void) {return rank;}
MPI_Comm getparentcomm(void) {return rook;}

static MPI_Comm currComm;
static P currTag;
static P currRank;
static B* currBuff;
static P currSize;
static BOOLEAN currFlag;

typedef P (*MpiThreadFunc)(void);
static MpiThreadFunc mpithread_func = (MpiThreadFunc) -1;
static P mpithread_retc;

static pthread_mutex_t mpithread_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_t mpithread_id;
static pthread_cond_t mpithread_cond = PTHREAD_COND_INITIALIZER;

static void exithandler(void) {
  int eq;
  if (! MPI_Comm_compare(rook, MPI_COMM_NULL, &eq) && eq != MPI_IDENT)
    MPI_Abort(rook, 0);
  else
    MPI_Abort(MPI_COMM_WORLD, 0);
}

static void mpihandler(MPI_Comm* comm, int* err, ...) {
  static char string[MPI_MAX_ERROR_STRING];
  int len;
  int eq = 0;
  static BOOLEAN inhandler = 0;

  if (inhandler) return;
  inhandler = 1;

  if (MPI_Error_string(*err, string, &len))
    error(1, 0, "Unable to produce error string (%i)", *err);
  else if (MPI_Comm_compare(*comm, world, &eq))
    error(1, 0, "MPI Error: %s", string);
  else
    error(1, 0, "Error %s mpi: %s", 
	  (eq == MPI_IDENT) ? "speaking with pawns" : "speaking with rook",
	  string);

  exit(1);
}

static P mpithread_probe(void) {
  MPI_Status status;
  MPI_Probe(currRank, currTag, currComm, &status);
  currRank = status.MPI_SOURCE;
  currTag = status.MPI_TAG;
  currSize = (P) status.count;
  return OK;
}

static P mpithread_iprobe(void) {
  MPI_Status status;
  int flag;
  MPI_Iprobe(currRank, currTag, currComm, &flag, &status);
  if (flag) {
    currRank = status.MPI_SOURCE;
    currTag = status.MPI_TAG;
    currSize = (P) status.count;
  }
  currFlag = (BOOLEAN) currFlag;
  return OK;
}

static P mpithread_recv(void) {
  MPI_Recv(currBuff, currSize, MPI_UNSIGNED_CHAR, 
	   currRank, currTag, currComm, MPI_STATUS_IGNORE);
  return OK;
}

static P mpithread_send(void) {
  MPI_Send(currBuff, currSize, MPI_UNSIGNED_CHAR,
	   currRank, currTag, currComm);
  return OK;
}

static P mpithread_broadcast(void) {
  MPI_Bcast(currBuff, currSize, MPI_UNSIGNED_CHAR,
	    currRank, currComm);
  return OK;
}

static P mpithread_barrier(void) {
  MPI_Barrier(currComm);
  return OK;
}

DM_INLINE_STATIC void mpithread_lock(void) {
  if (pthread_mutex_lock(&mpithread_mutex)) 
    error(1, errno, "Failed lock");
}

DM_INLINE_STATIC void mpithread_recv_signal(void) {
  if (pthread_cond_wait(&mpithread_cond, &mpithread_mutex))
    error(1, errno, "Failed cond");
}

DM_INLINE_STATIC void mpithread_send_signal(void) {
  if (pthread_cond_signal(&mpithread_cond)) 
    error(1, errno, "Failed signal");
}

DM_INLINE_STATIC void mpithread_unlock(void) {
  if (pthread_mutex_unlock(&mpithread_mutex)) 
    error(1, errno, "Failed unlock");
}

DM_INLINE_STATIC P mpithread_exec(MpiThreadFunc func) {
  P retc;
  mpithread_func = func;
  mpithread_send_signal();

  while (!abortflag && mpithread_func) mpithread_recv_signal();
  retc = mpithread_retc;

  mpithread_unlock();  
  return abortflag ? ABORT : retc;
}

static void* mpifunc(void* unused) __attribute__ ((__noreturn__));

static void* mpifunc(void* unused __attribute__ ((__unused__)) ) {
  int argc =0;
  char* argv[] = {NULL};
  int threadtype;
  MPI_Errhandler mpierr;
  sigset_t new;
  int universe_;
  int rank_;

  mpithread_lock();

  if (sigfillset(&new) || pthread_sigmask(SIG_BLOCK, &new, NULL))
    error(1, errno, "Failed sigmask");

  MPI_Init_thread(&argc, (char***) &argv, MPI_THREAD_MULTIPLE, &threadtype);
  atexit(exithandler);
  if (threadtype < MPI_THREAD_MULTIPLE)
    error(1, 0, "MPI_Init_thread: Requested %i, received %i",
	  MPI_THREAD_MULTIPLE, threadtype);

  MPI_Comm_create_errhandler(mpihandler, &mpierr);
  MPI_Comm_dup(MPI_COMM_WORLD, &world);
  MPI_Comm_set_errhandler(world, mpierr);
  MPI_Comm_get_parent(&rook);  
  MPI_Comm_set_errhandler(rook, mpierr);
  MPI_Comm_size(world, &universe_);
  universe = universe_;
  MPI_Comm_rank(world, &rank_);
  rank = rank_;

  while (1) {
    mpithread_func = NULL;
    mpithread_send_signal();

    do {mpithread_recv_signal();} while (! mpithread_func);
    mpithread_retc = mpithread_func();
  }
}

P mpiiprobe(MPI_Comm comm, P* tag, P* rank, BOOLEAN* flag, P* count) {
  P retc;
  mpithread_lock();
  currComm = comm;
  currTag = *tag;
  currRank = *rank;
  if ((retc = mpithread_exec(mpithread_iprobe))) return retc;

  if (currFlag) {
    *tag = currTag;
    *rank = currRank;
    *count = currSize;
  }
  *flag = currFlag;
  return OK;
}

P mpiprobe(MPI_Comm comm, P* tag, P* rank, P* count) {
  P retc;

  mpithread_lock();
  currComm = comm;
  currTag = *tag;
  currRank = *rank;
  if ((retc = mpithread_exec(mpithread_probe))) return retc;
  
  *tag = currTag;
  *rank = currRank;
  *count = currSize;
  return OK;
}

P mpirecv(MPI_Comm comm, P tag, P rank, B* buffer, P size) {
  mpithread_lock();
  currComm = comm;
  currTag = tag;
  currRank = rank;
  currBuff = buffer;
  currSize = size;
  return mpithread_exec(mpithread_recv);
}

P mpisend(MPI_Comm comm, P tag, P rank, B* buffer, P size) {
  mpithread_lock();
  currComm = comm;
  currTag = tag;
  currRank = rank;
  currBuff = buffer;
  currSize = size;
  return mpithread_exec(mpithread_send);
}

P mpibroadcast(MPI_Comm comm, P rank, B* buffer, P size) {
  mpithread_lock();
  currComm = comm;
  currRank = rank;
  currBuff = buffer;
  currSize = size;
  return mpithread_exec(mpithread_broadcast);
}

P mpibarrier(MPI_Comm comm) {
  mpithread_lock();
  currComm = comm;
  return mpithread_exec(mpithread_barrier);
}

void initmpi(void) {
  mpithread_lock();
  if (pthread_create(&mpithread_id, NULL, mpifunc, NULL))
    error(1, errno, "Failed thread create");

  do {mpithread_recv_signal();} while (mpithread_func);
  
  currComm = rook;
  if (mpithread_exec(mpithread_barrier))
    error(1, 0, "Unable to barrier with rook");
}
