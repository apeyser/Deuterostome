
/*---------------- D machine 3.0 (Linux) dvt_1.c -------------------------

 This is an 'include' module for dvt.c and contains dvt-specific
 operators plus their support:

     - error
     - aborted
     - abort
     - toconsole
     - nextevent
     - send
     - getsocket
     - connect
     - disconnect

*/

/*-------------------- operator support ------------------------------*/

#include "dm.h"

#include <string.h>

#include "dm2.h"

P toconsole(B *p, P atmost)
{
  P nb;
  if (atmost == -1) atmost = strlen((char*)p);
  while (atmost > 0) {
  tc1:
    checkabort();
    if ((nb = write(1, p, atmost)) < 0) { 
      if ((errno == EINTR) || (errno == EAGAIN)) goto tc1;
      else return(-errno);
    }
    atmost -= nb; p += nb;
  }
  return OK;
}

/*--------------------------- read a line from the console keyboard
    Tries to read a full line (terminated by '\n') from the console
    into the provided string buffer object. On success, the substring
    representing the line minus the '\n' character is inserted into
    the string buffer frame supplied to 'fromconsole'. Several abnormal
    returns can occur.
 */

P op_fromconsole(void)
{
  P nb, nsbuf;
  B *sbuf;
  BOOLEAN eof = FALSE;

  if (feof(stdin)) return QUIT;
  if (CEILexecs <= x2) return EXECS_OVF;
  
  moveframe(inputframe, x1);
  nsbuf = ARRAY_SIZE(inputframe);
  sbuf = VALUE_PTR(inputframe);

/* we read until we have a \n-terminated string */
  if (!fgets((char*)sbuf, nsbuf, stdin)) {
    if (ferror(stdin)) return -errno;
    eof = feof(stdin);
  }
  nb = strlen((char*)sbuf);
  /* we trim the buffer string object on the operand stack */
  ARRAY_SIZE(x1) = eof ? nb : nb - 1;
  FREEexecs = x2;
  return OK;
}

/*-------------------- DVT-specific operators -------------------------*/

/*-------------------------------------- 'error'
   use: instance_string error_numeral | (->abort)

  - Clib error numerals are negative errno of Clib
  - decodes the error numeral and writes message
  - executes 'abort'
  - NOTE: you do not want to report errors of D nodes this way
    because 'error' aborts; use 'nodeerror' instead, which simply reports
*/

P op_error(void)
{
  P e; B *m;
  B *p, strb[256];
  P nb, atmost; 

  if (o_2 < FLOORopds) goto baderror;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_1) != NUM) goto baderror;
  if (!PVALUE(o_1,&e)) goto baderror;
  
  p = strb; 
  atmost = 255;
  nb = dm_snprintf((char*)p,atmost,"\033[31m");
  p += nb; 
  atmost -= nb;
 
  if (e < 0) { /*Clib error */
    nb = dm_snprintf((char*)p,atmost,"%s",(char*)strerror(-e));
  }
  else { /* one of our error codes: decode */
    m = geterror(e);
    nb = dm_snprintf((char*)p,atmost,"%s",(char*)m);
  }

  p += nb; 
  atmost -= nb;
  nb = dm_snprintf((char*)p,atmost," in %s\033[0m\n", (B*)VALUE_BASE(o_2));
  nb += (P) (p - strb);
  toconsole(strb, nb);
  FREEopds = o_2;
  return op_abort();
  //return ABORT;

 baderror: 
  toconsole((B*)"Error with corrupted error info on operand stack!\n", -1L);
  return op_abort();
  //return ABORT;
}

/*-------------------------------------- 'errormessage'
  use: instance_string error-numeral stringbuf | substring_of_stringbuf

  - composes an error message and returns it in a subarray of string buffer
*/

P op_errormessage(void)
{
  P e, nb, tnb; B *m, *s;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (!PVALUE(o_2,&e)) return UNDF_VAL;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  s = (B *)VALUE_BASE(o_1); tnb = ARRAY_SIZE(o_1);
  if (e < 0) { /*Clib error */
    nb = dm_snprintf((char*)s,tnb,"%s",(char*)strerror(-e));
  } else { /* one of our error codes: decode */
    m = geterror(e);
    nb = strlen((char*)m);
    if (nb > tnb) nb = tnb;
    moveB(m,s,nb);
  }
  s += nb; 
  tnb -= nb;
  nb = snprintf((char*)s,tnb," in %s\n", (B *)VALUE_BASE(o_3));
  if (nb > tnb) nb = tnb;
  ARRAY_SIZE(o_1) = (P)(s + nb) - VALUE_BASE(o_1);
  moveframe(o_1,o_3);
  FREEopds = o_2;
  return OK;
}
    


/*--------------------------------------- abort
   - drops execution stack to level above nearest ABORTMARK object (a BOOL)
   - sets the boolean object that carries ABORTMARK to TRUE
*/

P x_op_abort(void) {return ABORT;}
static BOOLEAN rabort = FALSE;
P op_abort(void)
{
  fprintf(stderr, "Aborting..\n");
  if (rabort) {
    fprintf(stderr, "Recursive abort\n");
    return ABORT;
  }
  if (x2 > CEILexecs) {
    fprintf(stderr, "exec ovf abort\n");
    return ABORT;
  }    
  if (FREEvm+2 >= CEILvm) {
    fprintf(stderr, "mem ovf abort\n");
    return ABORT;
  }
    
  TAG(x1) = OP; ATTR(x1) = ACTIVE;
  OP_NAME(x1) = "x_op_abort"; 
  OP_CODE(x1) = x_op_abort;
  TAG(x2) = ARRAY | BYTETYPE; 
  ATTR(x2) = ACTIVE;
  VALUE_PTR(x2) = FREEvm;
  strncpy((char*)FREEvm, "a__", 3);
  ARRAY_SIZE(x2) = 3;
  FREEvm += 3;
  
  FREEexecs = x3;
  rabort = TRUE;
  return OK;
}

/*---------------------------------------------------- toconsole
     string | ---

  - prints string operand on console
*/
P op_toconsole(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  FREEopds = o_1;
  return toconsole((B *)VALUE_BASE(o1), ARRAY_SIZE(o1));
}

/* we push the errsource string followed by the error code on
   the operand stack, and 'error' on the execution stack 
*/

void makeerror(P retc, B* error_source) {
  if (retc == OPDS_OVF) FREEopds = FLOORopds;
  if (retc == EXECS_OVF) FREEexecs = FLOORexecs;
  if (o2 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;
  TAG(o1) = ARRAY | BYTETYPE; 
  ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)error_source; 
  ARRAY_SIZE(o1) = strlen((char*)error_source);
  TAG(o2) = NUM | LONGBIGTYPE; ATTR(o2) = 0;
  LONGBIG_VAL(o2) = retc;
  moveframe(errorframe,x1);
  FREEopds = o3; FREEexecs = x2;
}
