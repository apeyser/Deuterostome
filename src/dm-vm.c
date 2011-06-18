#include "dm.h"

#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#include "dm-vm.h"
#include "error-local.h"
#include "pluginlib.h"
#include "dm2.h"
#include "dm-signals.h"
#include "dm-sem.h"

// original directory for vmresize
static char* original_dir;

B msf[FRAMEBYTES] = {}, cmsf[FRAMEBYTES] = {};
/*-- setup: opds, execs, dicts, VM/MB, userdict */
static LBIG setup[5];

static void setupbase(B* sysdict, B* userdict) {
  moveframe(sysdict-FRAMEBYTES,FREEdicts);
  FREEdicts += FRAMEBYTES;
  moveframe(userdict-FRAMEBYTES,FREEdicts); 
  FREEdicts += FRAMEBYTES;
  TAG(msf) = (ARRAY | BYTETYPE); 
  ATTR(msf) = READONLY;
  if (FREEvm + MSF_SIZE + FRAMEBYTES > CEILvm)
    error_local(EXIT_FAILURE, 0, "VM chosen too small");
  VALUE_BASE(msf) = (P)FREEvm + FRAMEBYTES; 
  ARRAY_SIZE(msf) = MSF_SIZE;
  moveframe(msf, FREEvm); 
  FREEvm += FRAMEBYTES + DALIGN(MSF_SIZE);
  moveframe(msf,cmsf);
}

/*------------------------------------------------maketinysetup
  - creates a 'tiny' memory, just enough to bootstrap
    vmresize.  Sysdict is at the bottom of vm, not top
*/
  
void maketinysetup(void) {
  B *sysdict, *userdict;
  LBIG tinysetup[5] = { 100, 50, 10, 1 , 100 };
  
  if (makeDmemory(tinysetup))
    error_local(1, errno, "Insufficient memory");
  if ((sysdict = makeopdict((B*) sysop,syserrc,syserrm)) == (B*) -1L)
    error_local(EXIT_FAILURE, 0, "Cannot make system dictionary");;
  if ((userdict = makedict(tinysetup[4])) == (B *)(-1L))
    error_local(EXIT_FAILURE, 0, "Cannot make user dictionary");
  tinymemory = TRUE;
  setupbase(sysdict, userdict);

  if (! (original_dir = getcwd(NULL, 0))) 
    error_local(EXIT_FAILURE,errno,"getcwd");  
}

/*-------------------------------------------- vmresize
    <X nopds ndicts nexecs nVM/MB userdictsize > | bool
                                            null | true
    
  - with NULL as operand, establishes the 'tiny' D workspace
  - with dimensions operand, establishes a new workspace for the
    given stack and VM dimensions (stack dimensions are in objects,
    VM dimension is in MB)
  - sets up startup dir & switches back to original working directory
  - puts sysdict at top
  - pushes true if memory allocation succeeded.
*/

static P VMRESIZE_ERR(P err, BOOLEAN bool) {
  TAG(o1) = BOOL; 
  ATTR(o1) = 0; 
  BOOL_VAL(o1) = bool;
  FREEopds = o2;
  return err;
}

P op_vmresize_(void)
{
  B *userdict, *sysdict;

  if (o_1 < FLOORopds) return VMRESIZE_ERR(OPDS_UNF, FALSE);
  FREEopds = o_1;

  if ((tinymemory ? 1 : 0) == ((CLASS(o1) == NULLOBJ) ? 1 : 0)) {
    op_abort();
    return VMRESIZE_ERR(VMR_STATE, FALSE);
  }

  if (CLASS(o1) == NULLOBJ) { 
    closealllibs();
    maketinysetup();
  }
  else { 
    if (TAG(o1) != (ARRAY | LONGBIGTYPE)) return VMRESIZE_ERR(OPD_ERR, FALSE);
    if (ARRAY_SIZE(o1) < 5) return VMRESIZE_ERR(RNG_CHK, FALSE);
    moveLBIG((LBIG *)VALUE_BASE(o1), setup, 5);
    if ((setup[0] < 1000) || (setup[1] < 100)
        || (setup[2] < 50) || (setup[3] < 1)
        || (setup[4] < 200))
      return VMRESIZE_ERR(RNG_CHK, FALSE);
    if ((setup[0] > MAX_NUM_OPDS) || (setup[1] > MAX_NUM_DICTS)
        || (setup[2] > MAX_NUM_EXECS) || (setup[3] > MAX_MEM_SIZE)
        || (setup[4] > MAX_USER_DICT_SIZE))
      return VMRESIZE_ERR(RNG_CHK, FALSE);
    
    if (makeDmemory(setup)) 
      return VMRESIZE_ERR(VMR_ERR, FALSE);

    if ((sysdict = makeopdictbase((B*) sysop, syserrc, syserrm, SYS_DICT_SIZE))
        == (B*) -1L)
      error_local(EXIT_FAILURE, 0, "systemdict > vm");
    if ((userdict = makedict(setup[4])) == (B *)(-1L))
      error_local(EXIT_FAILURE, 0, "userdict > vm");
    tinymemory = FALSE;

    setupbase(sysdict, userdict);
    initialize_plugins();
    setupdirs();
  }

  if (chdir(original_dir)) 
    error_local(EXIT_FAILURE, errno, "chdir");

  return VMRESIZE_ERR(do_inter_lock_init ? do_inter_lock_init() : OK, 
		      TRUE);
}

/*-------------------------- Dnode operators -------------------------*/

static P x_op_lock(void) {
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != BOOL) return EXECS_COR;
	
  locked = BOOL_VAL(x_1);
  FREEexecs = x_1;
  return repush_stop();
}

/* ~active | -- */
P op_lock(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;

  TAG(x1) = BOOL; 
  ATTR(x1) = 0;
  BOOL_VAL(x1) = locked;

  TAG(x2) = OP; 
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_lock"; 
  OP_CODE(x2) = x_op_lock;

  TAG(x3) = BOOL; 
  ATTR(x3) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_1, x4);
  FREEexecs = x5;
  FREEopds = o_1;
  locked = TRUE;

  return OK;
}

/* ~active | -- */
P op_unlock(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;

  TAG(x1) = BOOL; 
  ATTR(x1) = 0;
  BOOL_VAL(x1) = locked;

  TAG(x2) = OP; 
  ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_lock"; 
  OP_CODE(x2) = x_op_lock;

  TAG(x3) = BOOL; 
  ATTR(x3) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_1, x4);
  FREEexecs = x5;
  FREEopds = o_1;
  locked = FALSE;

  return OK;
}

/* -- | bool */
P op_locked(void) {
  if (o1 >= CEILopds) return OPDS_OVF;

  TAG(o1) = BOOL;
  ATTR(o1) = 0;
  BOOL_VAL(o1) = locked;

  FREEopds = o2;
  return OK;
}

static P x_op_serialize(void) {
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != BOOL) return EXECS_COR;
	
  serialized = BOOL_VAL(x_1);
  FREEexecs = x_1;
  return repush_stop();
}

/* ~active | -- */
P op_serialize(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CEILexecs < x5) return EXECS_OVF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;

  TAG(x1) = BOOL; ATTR(x1) = 0;
  BOOL_VAL(x1) = serialized;

  TAG(x2) = OP; ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_serialize"; 
  OP_CODE(x2) = x_op_serialize;

  TAG(x3) = BOOL; 
  ATTR(x3) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_1, x4);
  FREEexecs = x5;
  FREEopds = o_1;
  serialized = TRUE;

  return OK;
}

/*------------------------------------- 'halt' 
   - pushes 'x_halt' frame on the execution stack and directs phrases
     received from the console to the execution stack
   - 'x_halt' blocks execution of frames below it on the execution stack
     by pushing itself back on the stack until 'continue' is executed
   - frames pushed above 'x_halt' are executed normally
   - below x_halt is x_halt_stop, which intercepts stops (if necessary),
     resets locked and halt_flag values under the halt, and starts the
     stop again (if necessary).
*/

P x_op_halt(void)
{
  if (halt_flag) {
    FREEexecs = x2;
    return DONE; 
  }
  return OK;
}

static P x_op_halt_stop(void) {
  if (x_2 < FLOORexecs) return EXECS_UNF;
  if (TAG(x_1) != BOOL || TAG(x_2) != BOOL)
    return EXECS_COR;

  halt_flag = BOOL_VAL(x_1);
  locked = BOOL_VAL(x_2);

  FREEexecs = x_2;
  return repush_stop();
}

P op_halt(void)
{
  if (x5 >= CEILexecs) return EXECS_OVF;
  
  TAG(x1) = BOOL; 
  ATTR(x1) = 0;
  BOOL_VAL(x1) = locked;

  TAG(x2) = BOOL;
  ATTR(x2) = 0;
  BOOL_VAL(x2) = halt_flag;

  TAG(x3) = OP;
  ATTR(x3) = ACTIVE;
  OP_NAME(x3) = "x_halt_stop";
  OP_CODE(x3) = x_op_halt_stop;

  TAG(x4) = BOOL;
  ATTR(x4) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x4) = FALSE;

  TAG(x5) = OP;
  ATTR(x5) = ACTIVE;
  OP_NAME(x5) = "x_halt";
  OP_CODE(x5) = x_op_halt;


  FREEexecs = x6;
  halt_flag = TRUE;
  return DONE;
}

/*------------------------------------- 'continue'
   - enables removal of x_halt from the execution stack
*/

P op_continue(void) { 
  halt_flag = FALSE; 
  return OK; 
}

/*--------------------------------------- abort
   - clears the operand stack
   - clears the execution stack
   - drops the dictionary stack to 'userdict'

*/

P op_abort(void)
{
  B *frame;
  abortflag = FALSE;
  isstopping = FALSE;

  frame = FREEexecs;
  while ((frame -= FRAMEBYTES) >= FLOORexecs) {
    if (ATTR(frame) & ABORTMARK) {
      BOOL_VAL(frame) = TRUE;
      ATTR(frame) = (ABORTMARK | ACTIVE);
      FREEexecs = frame + FRAMEBYTES;
      return OK;
    }
  }

  FREEopds = FLOORopds;
  FREEexecs = FLOORexecs;
  FREEdicts = FLOORdicts + FRAMEBYTES + FRAMEBYTES;
  moveframe(msf,cmsf);
  return DONE;
}

