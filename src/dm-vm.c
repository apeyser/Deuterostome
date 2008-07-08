#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#include "dm-vm.h"
#include "error-local.h"
#include "pluginlib.h"
#include "dm2.h"
#include "dm-signals.h"

// original directory for vmresize
static char* original_dir;

B msf[FRAMEBYTES] = {}, cmsf[FRAMEBYTES] = {};
/*-- setup: opds, execs, dicts, VM/MB, userdict */
static LBIG setup[5];

/////////////////////////////////////////// signal handling code

/*--------- signal handler: SIGFPE */

static void SIGFPEhandler(int sig __attribute__ ((__unused__)) )
{
  numovf = TRUE;
}

/*---------- signal handler: SIGALRM */

static void SIGALRMhandler(int sig __attribute__ ((__unused__)) )
{
  timeout = TRUE;
}

/*---------- signal handler: SIGINT */

static void quithandler(int sig) __attribute__ ((__noreturn__));
static void quithandler(int sig __attribute__ ((__unused__)) ) {
  exit(0);
}

static void aborthandler(int sig) {
  fprintf(stderr, "Aborting on signal %i\n", sig);
  abortflag = TRUE;
}

static void makeaborthandler(void)
{
  int abortsigs[] = {SIGINT, 0};
  int* i;
  for (i = abortsigs; *i; i++) sethandler(*i, aborthandler);
}

static void makequithandler(void) 
{
  int quitsigs[] = {SIGQUIT, SIGTERM, SIGHUP, 0};
  int* i;
  for (i = quitsigs; *i; i++) sethandler(*i, quithandler);
}

static void makeignorehandler(void)
{
  int quitsigs[] = {SIGHUP, 0};
  int* i;
  for (i = quitsigs; *i; i++) sethandler(*i, SIG_IGN);
}

void setuphandlers(void) {
/*----------------- SIGNALS that we wish to handle */
/* FPU indigestion is recorded in the numovf flag;
   we do not wish to be killed by it
*/

  numovf = FALSE;
  sethandler(SIGFPE, SIGFPEhandler);

/* The broken pipe signal is ignored, so it cannot kill us;
   it will pop up in attempts to send on a broken connection
*/

  sethandler(SIGPIPE, SIG_IGN);

/* We use alarms to limit read/write operations on sockets  */

  timeout = FALSE;
  sethandler(SIGALRM, SIGALRMhandler);

// Switched the following to SIGABRT, produced by kill -ABRT
// rather than control-c, that normally terminates the job
//
/* The interrupt signal is produced by the control-c key of the
   console keyboard, it triggers the execution of 'abort'
*/
  abortflag = FALSE;

  makequithandler();
  makeaborthandler();
  makeignorehandler();
}

static void setupbase(B* sysdict, B* userdict) {
  moveframe(sysdict-FRAMEBYTES,FREEdicts);
  FREEdicts += FRAMEBYTES;
  moveframe(userdict-FRAMEBYTES,FREEdicts); 
  FREEdicts += FRAMEBYTES;
  TAG(msf) = (ARRAY | BYTETYPE); 
  ATTR(msf) = READONLY;
  if (FREEvm + MSF_SIZE + FRAMEBYTES > CEILvm)
    error(EXIT_FAILURE, 0, "VM chosen too small");
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
    error(1, errno, "Insufficient memory");
  if ((sysdict = makeopdict((B*) sysop,syserrc,syserrm)) == (B*) -1L)
    error(EXIT_FAILURE, 0, "Cannot make system dictionary");;
  if ((userdict = makedict(tinysetup[4])) == (B *)(-1L))
    error(EXIT_FAILURE, 0, "Cannot make user dictionary");
  tinymemory = TRUE;
  setupbase(sysdict, userdict);

  if (! (original_dir = getcwd(NULL, 0))) 
    error(EXIT_FAILURE,errno,"getcwd");  

  setuphandlers();
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
  if (CLASS(o1) == NULLOBJ) { 
    if (tinymemory) {
      op_abort(); 
      return VMRESIZE_ERR(VMR_STATE, FALSE);
    };
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
    
    if (!tinymemory) {
      closealllibs(); 
      maketinysetup();
    }
    
    if (makeDmemory(setup)) 
      return VMRESIZE_ERR(VMR_ERR, FALSE);

    if ((sysdict = makeopdictbase((B*) sysop,syserrc,syserrm,SYS_DICT_SIZE))
        == (B*) -1L)
      error(EXIT_FAILURE, 0, "systemdict > vm");
    if ((userdict = makedict(setup[4])) == (B *)(-1L))
      error(EXIT_FAILURE, 0, "userdict > vm");
    tinymemory = FALSE;

    setupbase(sysdict, userdict);    
    initialize_plugins();
    setupdirs();
  }

  if (chdir(original_dir)) error(EXIT_FAILURE,errno,"chdir");

  return VMRESIZE_ERR(OK, TRUE);
}

/*-------------------------- Dnode operators -------------------------*/

static P x_op_lock(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;
  if (TAG(x_1) != BOOL) return EXECS_COR;
	
  locked = BOOL_VAL(x_1);
  if (! BOOL_VAL(o_1)) FREEexecs = x_1;
  else {
    TAG(x_1) = OP;
    ATTR(x_1) = ACTIVE;
    OP_NAME(x_1) = "stop"; 
    OP_CODE(x_1) = op_stop;
  }
  FREEopds = o_1;
  return OK;
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
  ATTR(x3) = (STOPMARK | ACTIVE);
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
  ATTR(x3) = (STOPMARK | ACTIVE);
  BOOL_VAL(x3) = FALSE;

  moveframe(o_1, x4);
  FREEexecs = x5;
  FREEopds = o_1;
  locked = FALSE;

  return OK;
}


static P x_op_serialize(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (x_1 < FLOORexecs) return EXECS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;
  if (TAG(x_1) != BOOL) return EXECS_COR;
	
  serialized = BOOL_VAL(x_1);
  if (! BOOL_VAL(o_1)) FREEexecs = x_1;
  else {
    TAG(x_1) = OP; ATTR(x_1) = ACTIVE;
    OP_NAME(x_1) = "stop"; 
    OP_CODE(x_1) = op_stop;
  }
  FREEopds = o_1;
  return OK;
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

  TAG(x3) = BOOL; ATTR(x3) = (STOPMARK | ACTIVE);
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
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;
  if (TAG(x_1) != BOOL || TAG(x_2) != BOOL)
    return EXECS_COR;

  halt_flag = BOOL_VAL(x_1);
  locked = BOOL_VAL(x_2);
  if (! BOOL_VAL(o_1)) FREEexecs = x_2;
  else {
    TAG(x_2) = OP;
    ATTR(x_2) = ACTIVE;
    OP_NAME(x_2) = "stop";
    OP_CODE(x_2) = op_stop;
    FREEexecs = x_1;
  }
  FREEopds = o_1;
  return OK;
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
  ATTR(x4) = (STOPMARK|ACTIVE);
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


P op_tostderr(void)
{
  B *p;
  P nb, atmost;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

  p = VALUE_PTR(o_1);
  atmost = ARRAY_SIZE(o_1);
  while (atmost > 0) {
    while ((nb = write(2, p, atmost)) < 0
           && ((errno == EINTR) || (errno == EAGAIN)));
    if (nb < 0) return op_abort();
    atmost -= nb;
    p += nb;
  }

  FREEopds = o_1;
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

  frame = FREEexecs;
  while ((frame -= FRAMEBYTES) >= FLOORexecs) {
    if (ATTR(frame) & ABORTMARK) {
      BOOL_VAL(frame) = TRUE;
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

