/*====================== D machine Rev3.0 (dm5.c) ======================

       -  operators:
          - exec
          - if
          - ifelse
          - for
          - repeat
          - loop
          - forall
          - exit
          - stop
          - stopped
          - countexecstack
          - execstack
          - quit
	  - die
          - eq
          - ne
          - ge
          - gt
          - le
          - lt
          - and
          - not
          - or
          - xor
          - bitshift
*/

#define DEBUG_ACTIVE 0
#include "dm.h"

#include <stdio.h>
#include <limits.h>

#include "dm2.h"
#include "dm5.h"

#include "error-local.h"

/*----------------------------------------------- start
   any_active | --

   drops the currently executing object from the execution stack and
   pushes any_active on the execution stack.
*/

P op_start(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  moveframe(o_1,FREEexecs-FRAMEBYTES);
  FREEopds = o_1;
  return OK;
}

/*----------------------------------------------- exec
   any_active | --

   pushes any_active on the execution stack
*/

P op_exec(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (x1 >= CEILexecs) return EXECS_OVF;
  moveframe(o_1,x1);
  FREEopds = o_1; FREEexecs = x2;
  return OK;
}

/*----------------------------------------------- if
   bool proc | ---
*/

P op_if(void)
{
  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != BOOL)) return OPD_CLA;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (BOOL_VAL(o_2)) { 
    if (x1 >= CEILexecs) return EXECS_OVF;
    moveframe(o_1, x1); 
    FREEexecs = x2;
  } 
  FREEopds = o_2;
  return OK;
}

/*----------------------------------------------- ifelse
   bool proc1 proc2 | ---
*/

P op_ifelse(void)
{
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_3) != BOOL) return OPD_CLA;
  if (((ATTR(o_2) & ACTIVE) == 0) || ((ATTR(o_1) & ACTIVE) == 0))
    return OPD_ATR;
  if (x1 >= CEILexecs) return EXECS_OVF;
  if (BOOL_VAL(o_3)) moveframe(o_2, x1); 
  else moveframe(o_1, x1);
  FREEexecs = x2; 
  FREEopds = o_3;
  return OK;
}

/*----------------------------------------------- for
   first step limit proc | ---

                              proc
     x_for                    x_for
     proc                     proc
     limit                    limit
     step                     step
   * first                  * current      (* = exit mark)

*/
static P x_op_for(void)
{
  if (COMPARE(x_4,x_2) == TEST(x_3)) /* i.e. GT/GT, LT/LT, EQ/EQ */
    FREEexecs = x_4;
  else { 
    if (o1 >= CEILopds) return OPDS_OVF;
    moveframe(x_4, o1);
    ATTR(o1) &= ~XMARK;
    FREEopds = o2;
    moveframe(x_1,x2);
    ADD(x_4,x_3);
    FREEexecs = x3; 
  }
  return OK;
}

/*----------------------------------------- replace_active_name
   replaces a frame of an active name, with the object to which
   it points if that object is active (otherwise, null op)
*/
void replace_active_name(B* frame)
{
  B* dict = FREEdicts;
  B* newframe;
  while((dict -= FRAMEBYTES) > FLOORdicts)
    if ((newframe = lookup(frame, VALUE_PTR(dict))) != 0L) {
      if ((ATTR(newframe) & ACTIVE) == 0) return;
      moveframe(newframe, frame);
      return;
    }
}

P op_for(void)
{
  if (o_4 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_4) != NUM) 
      || (CLASS(o_3) != NUM) 
      || (CLASS(o_2) != NUM)) 
    return OPD_CLA;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if ((TEST(o_4) == UN) 
      || (TEST(o_3) == UN) 
      || (TEST(o_2) == UN)) 
    return UNDF_VAL; 
  if (x6 >= CEILexecs) return EXECS_OVF;

  if (CLASS(o_1) == NAME) replace_active_name(o_1);
 
  ATTR(o_4) |= EXITMARK;
  moveframes(o_4,x1,4L);
  TAG(x5) = OP; 
  ATTR(x5) = ACTIVE;
  OP_NAME(x5) = "x_for"; 
  OP_CODE(x5) = x_op_for;
  FREEexecs = x6; 
  FREEopds = o_4;

  return OK;
}

/*--------------------------------------------- repeat
    count proc | --

                    proc
     x_repeat       x_repeat
     proc           proc
   * count        * count          (exitmark)

*/

static P x_op_repeat(void)
{
  W t;

  t = TEST(x_2);
  if ((t == LT) || (t == EQ)) { 
    FREEexecs = x_2; 
    return OK; 
  }
  DECREMENT(x_2);
  moveframe(x_1, x2);
  ATTR(x2) &= ~XMARK;
  FREEexecs = x3;

  return OK;
}

P op_repeat(void)
{
  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM)) return OPD_CLA;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (TEST(o_2) == UN) return UNDF_VAL;
  if (x4 >= CEILexecs) return EXECS_OVF;
  if (CLASS(o_1) == NAME) replace_active_name(o_1); 

  ATTR(o_2) |= EXITMARK;
  moveframes(o_2, x1, 2L);
  TAG(x3) = OP; ATTR(x3) = ACTIVE;
  OP_NAME(x3) = "x_repeat"; 
  OP_CODE(x3) = x_op_repeat;
  FREEopds = o_2; 
  FREEexecs = x4;

  return OK;
}

/*------------------------------------------------ loop
   proc | --

                    proc
     x_loop         x_loop
   * proc         * proc           (exitmark)
*/

static P x_op_loop(void)
{
  moveframes(x_1,x2, 1L);
  ATTR(x2) &= (~XMARK);          /* no replication of exitmark! */
  FREEexecs = x3;
  return OK;
}

P op_loop(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (x4 >= CEILexecs) return EXECS_OVF;
  if (CLASS(o_1) == NAME) replace_active_name(o_1); 

  ATTR(o_1) |= EXITMARK; 
  moveframe(o_1, x1);
  TAG(x2) = OP; ATTR(x2) = ACTIVE;
  OP_NAME(x2) = "x_loop"; 
  OP_CODE(x2) = x_op_loop;
  FREEexecs = x3; 
  FREEopds = o_1;

  return OK;
}

/*---------------------------------------------- forall
     array proc | ---
      list proc | ---
      dict proc | ---

                                proc
      x_forall                  x_forall
      proc                      proc
   *  array/list/dict         * array/list/dict
*/

static P x_op_forall(void)
{
  B *dict, *entry;

  switch(CLASS(x_2)) {
    case ARRAY: 
      if (ARRAY_SIZE(x_2) <= 0) { 
        FREEexecs = x_2; 
        return OK; 
      }
      if (o1 >= CEILopds) return OPDS_OVF;
      TAG(o1) = NUM | TYPE(x_2);
      MOVE(x_2,o1); 
      ATTR(o1) = 0;
      VALUE_BASE(x_2) +=  VALUEBYTES(TYPE(x_2));
      ARRAY_SIZE(x_2)--; FREEopds = o2;
      break;

    case LIST:  
      if (VALUE_BASE(x_2) >= LIST_CEIL(x_2)) {
        FREEexecs = x_2; 
        return OK; 
      }
      if (o1 >= CEILopds) return OPDS_OVF;
      moveframe((B *)VALUE_BASE(x_2),o1);
      FREEopds = o2;
      VALUE_BASE(x_2) += FRAMEBYTES;
      break;

    case DICT:  
      dict = (B *)VALUE_BASE(x_2);
      if (DICT_CURR(x_2) >= DICT_FREE(dict)) { 
        FREEexecs = x_2; 
        return OK; 
      }
      if (o2 >= CEILopds) return OPDS_OVF;
      entry = (B *)DICT_CURR(x_2);
      DICT_CURR(x_2) += ENTRYBYTES;
      moveframe(ASSOC_NAME(entry),o1);
      ATTR(o1) = 0;
      moveframe(ASSOC_FRAME(entry),o2);
      FREEopds = o3; 
      break;

     default: 
       return EXECS_COR;
  }
  moveframe(x_1,x2); 
  FREEexecs = x3;
  return OK;
}

P op_forall(void)
{
  B *dict;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (x3 >= CEILexecs) return EXECS_OVF;
  switch(CLASS(o_2)) {
     case ARRAY: break;
     case LIST:  break;
     case DICT:  
       dict = VALUE_PTR(o_2);
       DICT_CURR(o_2) = DICT_ENTRIES(dict); 
       break;
     default: 
       return OPD_CLA;
  }

  if (CLASS(o_1) == NAME) replace_active_name(o_1); 
  moveframes(o_2,x1,2L); 
  ATTR(x1) |= EXITMARK;
  TAG(x3) = OP; 
  ATTR(x3) = ACTIVE;
  OP_NAME(x3) = "x_forall"; 
  OP_CODE(x3) = x_op_forall;
  FREEexecs = x4; 
  FREEopds = o_2;

  return OK;
}

/*---------------------------------------------- exit */
// -- | --
// exits inner loop
// if not in loop or invalid exit, errors with EXECS_UNF.
// if it jumps through another stop, abort errors with INV_EXT

P op_exit(void)
{
  B *frame; W m;
  isstopping = FALSE;

  frame = FREEexecs;
  while ((frame -= FRAMEBYTES) >= FLOORexecs) {
    if ((m = ATTR(frame) & XMARK))  {
      if (m & EXITMARK) {
        FREEexecs = frame; 
        return OK; 
      }
      else return INV_EXT;
    }     
  }

  return EXECS_UNF;
} 

/*----------------------------------------------- stop */
// -- | -- (leaves true on exec stack)
// exits stopped
// if not in stopped or invalid stop, errors with EXECS_UNF.
// if it jumps through another loop, abort errors with INV_STOP

P op_stop(void)
{
  B *frame; W m; 
  isstopping = FALSE;

  frame = FREEexecs;
  while ((frame -= FRAMEBYTES) >= FLOORexecs) {
    if ((m = ATTR(frame) & XMARK)) {
      if (m & STOPMARK) { 
        BOOL_VAL(frame) = TRUE;
	ATTR(frame) = (STOPMARK | ACTIVE);
        FREEexecs = frame + FRAMEBYTES; 
	isstopping = TRUE;
        return OK; 
      }
      else if (m & ABORTMARK) return INV_STOP;
    }
  }

  return EXECS_UNF;
}

/*----------------------------------------------- stopped
   any_active | bool

    any     
  * false 
*/

P op_stopped(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if ((ATTR(o_1) & ACTIVE) == 0) return OPD_ATR;
  if (x3 > CEILexecs) return EXECS_OVF;
  TAG(x1) = BOOL; 
  ATTR(x1) = (STOPMARK | ACTIVE);
  BOOL_VAL(x1) = FALSE;
  moveframe(o_1, x2); 
  FREEexecs = x3;
  FREEopds = o_1;
  return OK;
}

/*----------------------------------------------- countexecstack
    --- | int
  returns number of elements on execution stack
*/

P op_countexecstack(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (FREEexecs-FLOORexecs) / FRAMEBYTES;
  FREEopds = o2;
  return OK;
}

/*-------------------------------------- execstack
  list | sublist

  stores all elements of the execution stack into the list and
  returns the sublist of copied elements in the list.
*/

P op_execstack(void)
{
  B *eframe; 
  P n,nb;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != LIST) return OPD_CLA;
  nb = FREEexecs - FLOORexecs; 
  n = nb / FRAMEBYTES;
  if (nb > (LIST_CEIL(o_1) - VALUE_BASE(o_1))) return RNG_CHK;

  moveframes(FLOORexecs, (B *)VALUE_BASE(o_1), n);
  ATTR(o_1) &= (~PARENT);
  LIST_CEIL(o_1) = VALUE_BASE(o_1) + nb;
  for (eframe = VALUE_PTR(o_1); 
       eframe < LIST_CEIL_PTR(o_1);
       eframe += FRAMEBYTES) { 
    ATTR(eframe) &= (~XMARK);
    if (CLASS(eframe) == DICT)
      moveframe(VALUE_PTR(eframe)-FRAMEBYTES, eframe); 
    /* restore frame's DICT_NB */
  }
  return OK;
}

/*--------------------------------------------------- quit
   ---|---
*/

P op_quit(void)  
{
  DEBUG("%s", "quitting");
  recvd_quit = FALSE;
  exitval = 0;
  return TERM;
}

// num | exited
P op_die(void) {
  DEBUG("%s", "dieing");
  recvd_quit = FALSE;

  if (FLOORopds > o_1) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &exitval)) return UNDF_VAL;
  if (sizeof(P) > sizeof(int) && exitval > INT_MAX) 
    return RNG_CHK;

  return TERM;
}

/*---------------------------------------------------- eq
   any1 any2 | bool

simple objects:     equality of values (including undefined)
composite objects:  identity of values (i.e. shared VM) 

*/

P op_eq(void)
{
  BOOLEAN t;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) == CLASS(o_1)) { 
    switch(CLASS(o_2)) {
      case MARK: 
        t = TRUE; 
        break;

      case NULLOBJ: 
	if (TYPE(o_1) != TYPE(o_2)) t = FALSE;
	else switch (TYPE(o_1)) {
	  case SOCKETTYPE:
	    t = (SOCKET_VAL(o_1) == SOCKET_VAL(o_2)) ? TRUE : FALSE;
	    break;
	  case PIDTYPE:
	    t = (PID_VAL(o_1) == PID_VAL(o_2)) ? TRUE : FALSE;
	    break;
	  default: 
	    t = TRUE;
	    break;
	}
	break;

      case NUM: 
        t = COMPARE(o_2,o_1);
        if (t == UN) { 
          t = ((TEST(o_2) == UN) && (TEST(o_1) == UN));
          break; 
        }
        if (t == EQ) t = TRUE; 
        else t = FALSE; 
        break;

      case BOOL: 
        if (BOOL_VAL(o_2) == BOOL_VAL(o_1)) t = TRUE;
        else t = FALSE; 
        break;

      case OP: 
        if ((OP_NAME(o_2) == OP_NAME(o_1)) 
            && (OP_CODE(o_2) == OP_CODE(o_1))) 
          t = TRUE;
        else t = FALSE; 
        break;

      case NAME: 
        if ((NAME_KEY(o_2) == NAME_KEY(o_1)) 
            && matchname(o_2,o_1)) 
          t = TRUE; 
        else t = FALSE; 
        break;

      case ARRAY: 
      case LIST:
      case DICT:
      case BOX:  
        t = ((VALUE_BASE(o_2) == VALUE_BASE(o_1)) 
             && (ARRAY_SIZE(o_2) == ARRAY_SIZE(o_1)));
        break;

      case STREAM:
	t = ((STREAM_FD(VALUE_PTR(o_2)) == STREAM_FD(VALUE_PTR(o_1))));
	break;

      default: return OPD_CLA; // never happen
    } 
  }
  else t = FALSE;

  TAG(o_2) = BOOL; 
  ATTR(o_2) = 0; 
  BOOL_VAL(o_2) = t;
  FREEopds = o_1;

  return OK;
}

/*---------------------------------------------------- ne
   any1 any2 | bool

simple objects:     equality of values
composite objects:  identity of values (shared VM)
*/

P op_ne(void)
{
  BOOLEAN t;

  if (o_2 < FLOORopds) return OPDS_UNF;

  if (CLASS(o_2) == CLASS(o_1)) { 
    switch(CLASS(o_2)) {
      case MARK: 
        t=FALSE; 
        break;
        
      case NULLOBJ: 
        if (TYPE(o_1) != TYPE(o_2)) 
          t = TRUE;
	else switch (TYPE(o_1)) {
	  case SOCKETTYPE:
	    t = (SOCKET_VAL(o_1) != SOCKET_VAL(o_2)) ? TRUE : FALSE;
	    break;
	  case PIDTYPE:
	    t = (PID_VAL(o_1) != PID_VAL(o_2)) ? TRUE : FALSE;
	    break;
	  default:
	    t = FALSE;
	    break;
	};
	break;

      case NUM: 
        t = COMPARE(o_2,o_1);
        if (t == UN) {
          t = ((TEST(o_2) != UN) || (TEST(o_1) != UN));
          break; 
        }
        if (t != EQ) t = TRUE; 
        else t = FALSE; 
        break;

      case BOOL: 
        if (BOOL_VAL(o_2) != BOOL_VAL(o_1)) t = TRUE;
        else t = FALSE; 
        break;

      case OP: 
        if ((OP_NAME(o_2) != OP_NAME(o_1)) 
            || (OP_CODE(o_2) != OP_CODE(o_1))) 
          t = TRUE;
        else t = FALSE; 
        break;

      case NAME: 
        if ((NAME_KEY(o_2) == NAME_KEY(o_1)) 
            && matchname(o_2,o_1)) 
          t = FALSE; 
        else t = TRUE; 
        break;

      case ARRAY: 
      case LIST:
      case DICT:
      case BOX:  
        t = ((VALUE_BASE(o_2) != VALUE_BASE(o_1)) 
             || (ARRAY_SIZE(o_2) != ARRAY_SIZE(o_1)));
        break;

      case STREAM:
	t = (STREAM_FD(VALUE_PTR(o_2)) != STREAM_FD(VALUE_PTR(o_1)));
	break;
	
      default: 
        return OPD_CLA; //never happen
    } 
  }
  else t = TRUE;

  TAG(o_2) = BOOL; 
  ATTR(o_2) = 0; 
  BOOL_VAL(o_2) = t;
  FREEopds = o_1;

  return OK;
}

/*---------------------------------------------------- ge
   num1 num2 | bool
*/

P op_ge(void)
{
  W t;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return OPD_CLA;
  t = COMPARE(o_2,o_1);  
  if (t == UN) return UNDF_VAL;

  BOOL_VAL(o_2) = ((t == EQ) || (t == GT));
  TAG(o_2) = BOOL; ATTR(o_2) = 0;
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- gt
   num1 num2 | bool
*/

P op_gt(void)
{
  W t;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return OPD_CLA;
  t = COMPARE(o_2,o_1);  
  if (t == UN) return UNDF_VAL;

  BOOL_VAL(o_2) = (t == GT); 
  TAG(o_2) = BOOL; 
  ATTR(o_2) = 0;
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- le
   num1 num2 | bool
*/

P op_le(void)
{
  W t;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return OPD_CLA;
  t = COMPARE(o_2,o_1);  
  if (t == UN) return UNDF_VAL;

  BOOL_VAL(o_2) = ((t == EQ) || (t == LT));
  TAG(o_2) = BOOL; 
  ATTR(o_2) = 0;
  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- lt
   num1 num2 | bool
*/

P op_lt(void)
{
  W t;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return OPD_CLA;
  t = COMPARE(o_2,o_1); 
  if (t == UN) return UNDF_VAL;

  BOOL_VAL(o_2) = (t == LT); 
  TAG(o_2) = BOOL; 
  ATTR(o_2) = 0;
  FREEopds = o_1;
  return OK;
}

DM_INLINE_STATIC ULBIG getbitpattern(B* frame) {
  switch (TYPE(frame)) {
    case BYTETYPE: 
      return (ULBIG) *(UB*) NUM_VAL(frame); 
    case WORDTYPE: 
      return (ULBIG) *(UW*) NUM_VAL(frame); 
    case LONG32TYPE: case SINGLETYPE:
      return (ULBIG) *(UL32*) NUM_VAL(frame); 
    case LONG64TYPE: case DOUBLETYPE:
      return (ULBIG) *(UL64*) NUM_VAL(frame);
  };
  
  return 0; //should never happen
}

DM_INLINE_STATIC void setbitpattern(B* frame, ULBIG n) {
  switch (TYPE(frame)) {
    case BYTETYPE:
      *(UB*) NUM_VAL(frame) = (UB) n;
      break;
    case WORDTYPE:
      *(UW*) NUM_VAL(frame) = (UW) n;
      break;
    case LONG32TYPE: case SINGLETYPE:
      *(UL32*) NUM_VAL(frame) = (UL32) n;
      break;
    case LONG64TYPE: case DOUBLETYPE:
      *(UL64*) NUM_VAL(frame) = (UL64) n;
      break;
  };
}

/*---------------------------------------------------- and
     num1 num2 | num     (result is of type of num1)
   bool1 bool2 | bool
*/

P op_and(void)
{
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != CLASS(o_1)) return OPD_ERR;

  switch (CLASS(o_2)) {
    case NUM:
      setbitpattern(o_2, getbitpattern(o_1) & getbitpattern(o_2));
      break;

    case BOOL:
      BOOL_VAL(o_2) = (BOOL_VAL(o_1) && BOOL_VAL(o_2)) ? TRUE : FALSE;
      break;
      
    default: 
      return OPD_CLA;
  }

  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- not
     num | num
    bool | bool
*/

P op_not(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  switch (CLASS(o_1)) {
    case NUM:
      LONGBIG_VAL(o_1) = ~(LONGBIG_VAL(o_1));
      break;

    case BOOL: 
      BOOL_VAL(o_1) = BOOL_VAL(o_1) ? FALSE : TRUE; 
      break;

    default: 
      return OPD_CLA;
  }

  return OK;
}

/*---------------------------------------------------- or
     num1 num2 | num  (result is of first operand type)
   bool1 bool2 | bool
*/

P op_or(void)
{
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != CLASS(o_1)) return OPD_ERR;

  switch (CLASS(o_2)) {
    case NUM: 
      setbitpattern(o_2, getbitpattern(o_1) | getbitpattern(o_2));
      break;

    case BOOL: 
      BOOL_VAL(o_2) = (BOOL_VAL(o_2) || BOOL_VAL(o_1)) ? TRUE : FALSE; 
      break;

    default: 
      return OPD_CLA;
  }

  FREEopds = o_1;
  return OK;
}
/*---------------------------------------------------- xor
     num1 num2 | num   (result is of first operand type)
   bool1 bool2 | bool
*/

P op_xor(void)
{
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != CLASS(o_1)) return OPD_ERR;

  switch (CLASS(o_2)) {
    case NUM:
      setbitpattern(o_2, getbitpattern(o_1) ^ getbitpattern(o_2));
      break;

   case BOOL: 
     BOOL_VAL(o_2) = (BOOL_VAL(o_2) != BOOL_VAL(o_1)) ? TRUE : FALSE;
     break;

   default: 
     return OPD_CLA;
  }

  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- bitshift
     num count | num
*/

P op_bitshift(void)
{
  LBIG c;
  ULBIG n;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return OPD_CLA;
  if (!VALUE(o_1, &c)) return UNDF_VAL;

  n = getbitpattern(o_2);
  setbitpattern(o_2, (c < 0) ? (n >> -c) : (n << c));
  FREEopds = o_1;
  return OK;
}

