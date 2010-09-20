#include "dm-sem.h"

#if DM_ENABLE_SEM

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <semaphore.h>

#ifndef SEM_DNODE_NAME
#define SEM_DNODE_NAME "dnode"
#endif

static sem_t* lock = NULL;
static char SEM_NAME[] = SEM_DNODE_NAME;

P inter_lock_init(void) {
  char sem_name[sizeof(SEM_NAME)/sizeof(char) + 10];
  const size_t sz = sizeof(sem_name)/sizeof(char);

  if (lock) {
    sem_t* lock_ = lock;
    lock = NULL;
    while (sem_close(lock_)) {
      if (errno == EINTR) checkabort();
      else return -errno;
    }
  }

  if (snprintf(sem_name, sz, "/%s-%lu",
	       SEM_NAME, (unsigned long) getpid())
      >= sz)
    return MEM_OVF;
  
  
  while (! (lock = sem_open(sem_name, O_CREAT|O_EXCL, 0, 1))) {
    if (errno == EINTR) checkabort();
    else return -errno;
  }

  while (sem_unlink(sem_name)) {
    if (errno == EINTR) checkabort();
    else return -errno;
  }
  
  return OK;
}

static BOOLEAN use_locks = FALSE;
static BOOLEAN interlocked = FALSE;

DM_INLINE_STATIC P x_inter_lock(BOOLEAN force) {
  if (! interlocked && (force || use_locks) && lock)
    while (sem_wait(lock)) {
      if (errno == EINTR) checkabort();
      else return -errno;
    }
  
  interlocked = (force || use_locks) && lock;
  return OK;
}

DM_INLINE_STATIC P x_inter_unlock(void) {
  if (interlocked && lock)
    while (sem_post(lock)) {
      if (errno == EINTR) checkabort();
      else return -errno;
    }

  interlocked = FALSE;
  return OK;
}

P do_inter_lock(void) {
  return x_inter_lock(FALSE);
}

P do_inter_unlock(void) {
  return x_inter_unlock();
}

// use_interlocks_bool | --
P op_set_inter_lock(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) =! BOOL) return OPD_CLA;
  
  use_locks = BOOL_VAL(o_1);
  FREEopds = o_1;
  return OK;
}

static P x_op_inter_lock(void) {
  P retc;

  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != BOOL) return EXECS_COR;

  if (! BOOL_VAL(x_1) && (retc = x_inter_unlock())) 
    return retc;
  
  FREEexecs = x_1;
  return repush_stop();
}

// ~active | --
P op_inter_lock(void) {
  P retc;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;
  
  TAG(x1) = BOOL; 
  ATTR(x1) = 0;
  BOOL_VAL(x1) = interlocked;
  
  TAG(x2) = OP; 
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_inter_lock"; 
  OP_CODE(x2) = x_op_inter_lock;

  TAG(x3) = BOOL; 
  ATTR(x3) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;
  
  moveframe(o_1, x4);

  if ((retc = x_inter_lock(TRUE))) return retc;

  FREEexecs = x5;
  FREEopds = o_1;
  return OK;
}

static P x_op_inter_lock_implicit(void) {
  P retc;

  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != BOOL) return EXECS_COR;

  if (! BOOL_VAL(x_1) && (retc = x_inter_unlock()))
    return retc;
  
  FREEexecs = x_1;
  return repush_stop();
}

// ~active | --
P op_inter_lock_implicit(void) {
  P retc;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;
  
  TAG(x1) = BOOL; 
  ATTR(x1) = 0;
  BOOL_VAL(x1) = interlocked;
  
  TAG(x2) = OP; 
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_inter_lock_implicit"; 
  OP_CODE(x2) = x_op_inter_lock_implicit;

  TAG(x3) = BOOL; 
  ATTR(x3) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;
  
  moveframe(o_1, x4);

  if ((retc = x_inter_lock(FALSE))) return retc;

  FREEexecs = x5;
  FREEopds = o_1;
  return OK;
}

static P x_op_inter_unlock(void) {
  P retc;

  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != BOOL) return EXECS_COR;

  if (BOOL_VAL(x_1) && (retc = x_inter_lock(TRUE)))
    return retc;
  
  FREEexecs = x_1;
  return repush_stop();
}

// ~active | --
P op_inter_unlock(void) {
  P retc;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;
  
  TAG(x1) = BOOL; 
  ATTR(x1) = 0;
  BOOL_VAL(x1) = interlocked;
  
  TAG(x2) = OP; 
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_inter_unlock"; 
  OP_CODE(x2) = x_op_inter_unlock;

  TAG(x3) = BOOL; 
  ATTR(x3) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;
  
  moveframe(o_1, x4);
  if ((retc = x_inter_unlock())) return retc;

  FREEexecs = x5;
  FREEopds = o_1;
  return OK;
}

#else // ! ENABLE_SEM

P inter_lock_init(void) {
  return OK;
}

P reset_inter_lock(void) {
  return OK;
}

P do_inter_lock(void) {
  return OK;
}

P do_inter_unlock(void) {
  return OK;
}

P op_inter_lock(void) {
  return INTER_LOCK_NOT_DEF;
}

P op_inter_unlock(void) {
  return INTER_LOCK_NOT_DEF;
}

P op_inter_lock_implicit(void) {
  return INTER_LOCK_NOT_DEF;
}
  

P op_set_inter_lock(void) {
  return INTER_LOCK_NOT_DEF;
}

#endif //ENABLE_SEM
