#include <mpi.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "dm-dpawn.h"
#include "dm-vm.h"
#include "dm2.h"
#include "dm-mpi.h"

static P rank;
static MPI_Comm comm;

static P wrap_mpirecv_1(B* buffer, P size) {
  return mpirecv(comm, 1, rank, buffer, size);
}

static P wrap_mpirecv_2(B* buffer, P size) {
  return mpirecv(comm, 2, rank, buffer, size);
}

static P wrap_mpisend_1(B* buffer, P size) {
  return mpisend(comm, 1, rank, buffer, size);
}

static P wrap_mpisend_2(B* buffer, P size) {
  return mpisend(comm, 2, rank, buffer, size);
}

static P handle_error(P retc) {
  static B buf[FRAMEBYTES+1024];
  if (! retc) return OK;

  if (o4 >= CEILopds) goto baderr;

  TAG(o1) = (NUM|LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = getworldrank();
  TAG(o2) = (ARRAY|BYTETYPE);
  ATTR(o2) = 0;
  VALUE_PTR(o2) = (B*) "mpi error";
  ARRAY_SIZE(o2) = strlen((char*) VALUE_PTR(o2));
  TAG(o3) = (NUM|LONGBIGTYPE);
  ATTR(o3) = 0;
  LONGBIG_VAL(o3) = retc;
  TAG(buf) = (ARRAY|BYTETYPE);
  ATTR(buf) = 0;
  VALUE_PTR(buf) = buf+FRAMEBYTES;
  ARRAY_SIZE(buf) = sizeof(buf)-FRAMEBYTES;
  moveframe(buf, o4);
  FREEopds = o4;
  if (op_errormessage()) goto baderr;  
  error(1, 0, "%.*s", (int) ARRAY_SIZE(o_1), VALUE_PTR(o_1));
  
 baderr:
  error(1, 0, "Unable to compose error for %lli\n", (long long) retc);
  return retc;
}

static P frommpi(B* bufferf, MPI_Comm parent, P src) {
  rank = src;
  comm = parent;
  return handle_error(fromsource(bufferf, wrap_mpirecv_1, wrap_mpirecv_2));
}

static P tompi(B* rootf, MPI_Comm parent, BOOLEAN mksave, P dest) {
  rank = dest;
  comm = parent;
  return handle_error(tosource(rootf, mksave, wrap_mpisend_1, wrap_mpisend_2));
}

static P wrap_mpibroadcast_1(B* buffer, P size) {
  return mpibroadcast(comm, rank, buffer, size);
}

static P broadcastmpi(B* rootf, MPI_Comm parent, P root) {
  rank = root;
  comm = parent;
  if (rank == getworldrank())
    return fromsource(NULL, wrap_mpibroadcast_1, wrap_mpibroadcast_1);
  else
    return tosource(rootf, FALSE, wrap_mpibroadcast_1, wrap_mpibroadcast_1);
}

P op_mpirank(void) {
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = (NUM | LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = getworldrank();
  FREEopds = o2;
  return OK;
}

P op_mpisize(void) {
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = (NUM | LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = getworldsize();
  FREEopds = o2;
  return OK;
}

P op_mpibarrier(void) {
  return mpibarrier(getworldcomm());
}

P op_rsend(void) {
  static B xrootf[FRAMEBYTES];
  if (o_1 < FLOORopds) return OPDS_UNF;
  moveframe(o_1, xrootf);
  FREEopds = o_1;
  return tompi(xrootf, getparentcomm(), TRUE, 0);
}

P op_mpibroadcast(void) {
  static B xrootf[FRAMEBYTES];
  P root;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &root) || root < 0 || root >= getworldsize())
    return RNG_CHK;

  moveframe(o_1, xrootf);
  FREEopds = o_2;
  return broadcastmpi(xrootf, getworldcomm(), root);
}

P op_mpisend(void) {
  static B xrootf[FRAMEBYTES];
  P dest;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &dest) || dest < 0 || dest >= getworldsize())
    return RNG_CHK;

  moveframe(o_1, xrootf);
  FREEopds = o_2;
  return tompi(xrootf, getworldcomm(), FALSE, dest);
}

P op_mpirecv(void) {
  P src;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &src) || src < 0 || src >= getworldsize())
    return RNG_CHK;

  return frommpi(NULL, getworldcomm(), src);
}

P op_mpiprobe(void) {
  P src, tag, retc, count;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (o1 >= CEILopds) return OPDS_OVF;

  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &src)) src = MPI_ANY_SOURCE;
  else if (src < 0 || src >= getworldsize()) return RNG_CHK;
  if (! PVALUE(o_1, &tag)) tag = MPI_ANY_TAG;
  else if (tag < 0 || tag > 32376) return RNG_CHK;

  if ((retc = mpiprobe(getworldcomm(), &tag, &src, &count)))
    return retc;
  
  TAG(o_2) = (NUM|LONGBIGTYPE);
  LONGBIG_VAL(o_2) = src;
  TAG(o_1) = (NUM|LONGBIGTYPE);
  LONGBIG_VAL(o_1) = tag;
  TAG(o1) = (NUM|LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = count;
  FREEopds = o2;

  return OK;
}
  
P op_mpiiprobe(void) {
  P src, tag, retc, count;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (o2 >= CEILopds) return OPDS_OVF;

  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &src)) src = MPI_ANY_SOURCE;
  else if (src < 0 || src >= getworldsize()) return RNG_CHK;
  if (! PVALUE(o_1, &tag)) tag = MPI_ANY_TAG;
  else if (tag < 0 || tag > 32376) return RNG_CHK;

  switch (mpiiprobe(getworldcomm(), &tag, &src, &count)) {
    case OK:
      TAG(o_2) = (NUM|LONGBIGTYPE);
      LONGBIG_VAL(o_2) = src;
      TAG(o_1) = (NUM|LONGBIGTYPE);
      LONGBIG_VAL(o_1) = tag;
      TAG(o1) = (NUM|LONGBIGTYPE);
      ATTR(o1) = 0;
      LONGBIG_VAL(o1) = count;
      TAG(o2) = BOOL;
      ATTR(o2) = 0;
      BOOL_VAL(o2) = TRUE;
      FREEopds = o3;
      break;
    case MPI_NOMSG:
      TAG(o_2) = BOOL;
      BOOL_VAL(o_2) = FALSE;
      FREEopds = o_1;
      break;

    default:
      return retc;
  }
  return OK;
}

P op_vmresize(void) {
  return op_vmresize_();
}

static void quithandler(int sig __attribute__ ((__unused__))) {
  exit(0);
}

/* push on operand stack:
   error code    (top)
   errsource string
   rank#
   and push active name 'error' on execution stack
*/
void makeerror(P retc, B* error_source) {   
  if (o3 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;
  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = rank; 
  TAG(o2) = ARRAY | BYTETYPE; 
  ATTR(o2) = READONLY;
  VALUE_PTR(o2) = error_source; 
  ARRAY_SIZE(o2) = strlen((char*)error_source);
  TAG(o3) = NUM | LONGBIGTYPE; 
  ATTR(o3) = 0; 
  LONGBIG_VAL(o3) = retc;
  moveframe(errorframe,x1);
  FREEopds = o4; 
  FREEexecs = x2;
}

static BOOLEAN pending(void) {
  if (halt_flag) {
    if (x_1 >= FLOORexecs 
	&& TAG(x_1) == OP 
	&& OP_CODE(x_1) == (P) x_op_halt)
      return FALSE;
  }

  return (FREEexecs != FLOORexecs);
}

static P clientinput(B* bufferf, MPI_Comm parent) {
  P retc;
  if ((retc = frommpi(bufferf, parent, 0))) return retc;

  if (x1 >= CEILexecs) return EXECS_OVF;
  if (o_1 < FLOORopds) return OPDS_UNF;
  moveframe(o_1, x1);
  FREEopds = o_1;
  FREEexecs = x2;

  return OK;
}

static P nextevent(B* bufferf) {
  P src ;
  P tag;
  P count;
  P retc;
  MPI_Comm parent = getparentcomm();

  do {
    if (abortflag) return ABORT;
    
    src = 0;
    tag = 1;
    switch (retc = pending() ? mpiiprobe(parent, &tag, &src, &count)
	                     : mpiprobe(parent, &tag, &src, &count)) {
      case OK: break;
      case MPI_NOMSG: retc = OK; continue;
      default: continue;
    }
    
    retc = clientinput(bufferf, parent);
  } while (! retc && ! pending());

  return retc;
}

/*-------------------------------------- 'error'
  - expects on operand stack:
     error code    (top)
     errsource string
     rank#
  - prints message on current console or startup
    terminal (default)
  - aborts on corrupted error info
  - halts after uncorrupted error
*/

P op_error(void)
{
  LBIG e, r;
  P nb, atmost; 
  B *m, strb[256], *p;
  P ret;

  p = strb; 
  atmost = 255;
  if (o_3 < FLOORopds) goto baderror;
  if (CLASS(o_3) != NUM) goto baderror;
  if (! VALUE(o_3, &r)) goto baderror;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_1) != NUM) goto baderror;
  if (!VALUE(o_1,&e)) goto baderror;

  nb = dm_snprintf((char*)p,atmost,"\033[31mOn rank %lld: ",
                   (long long) r);

  p += nb; atmost -= nb;
  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*)p,atmost,(char*)strerror((P)-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = dm_snprintf((char*)p,atmost,(char*)m);
  }
  p += nb; atmost -= nb;
  nb = dm_snprintf((char*)p,atmost," in %s\033[0m\n", (char*)VALUE_BASE(o_2));
  nb += (P)(p - strb);
  TAG(o_3) = ARRAY | BYTETYPE; 
  ATTR(o_3) = READONLY;
  VALUE_BASE(o_3) = (P)strb; 
  ARRAY_SIZE(o_3) = nb;
  FREEopds = o_2;
  op_toconsole();
  if ((ret = op_halt()) == DONE) return DONE;

  nb = dm_snprintf((char*)p, atmost, "** Error in internal halt!\n");
  goto baderror2;

 baderror: 
  nb = dm_snprintf((char*)p,atmost,
                   "**Error with corrupted error info on operand stack!\n");
 baderror2:
  op_abort();
  nb += (P)(p - strb);
  TAG(o1) = ARRAY | BYTETYPE; 
  ATTR(o1) = READONLY;
  VALUE_BASE(o1) = (P)strb; 
  ARRAY_SIZE(o1) = nb;
  FREEopds = o2;
  return op_toconsole();
}

/*-------------------------------------- 'errormessage'
  - expects on operand stack:
     string buffer (top)
     error code
     errsource string
     rank#
  - composes an error message and returns it in a subarray of string buffer
*/

P op_errormessage(void)
{
  LBIG e, r;
  P nb, tnb; 
  B *m, *s;

  if (o_4 < FLOORopds) goto baderror;
  if (CLASS(o_4) != NUM) goto baderror;
  if (! VALUE(o_4, &r)) goto baderror;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_2) != NUM) goto baderror;
  if (! VALUE(o_2,&e)) goto baderror;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) goto baderror;

  s = (B *)VALUE_BASE(o_1); 
  tnb = ARRAY_SIZE(o_1);
  nb = dm_snprintf((char*)s,
		   tnb,"On rank %lld: ", (long long) r);
  s += nb; tnb -= nb;

  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*)s,tnb,(char*)strerror(-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = strlen((char*)m);
    if (nb > tnb) nb = tnb;
    moveB(m,s,nb);
  }
  s += nb; tnb -= nb;
  nb = dm_snprintf((char*)s,tnb," in %s\n", (char*)VALUE_BASE(o_3));
  ARRAY_SIZE(o_1) = (P)(s + nb) - VALUE_BASE(o_1);
  moveframe(o_1,o_4);
  FREEopds = o_3;
  return OK;

 baderror:
  printf("**Error with corrupted error info on operand stack!\n");
  return op_halt();
}

/*-------------------------------------- 'toconsole'
   (message) | -

  - sends a command to print the message string to the current
    console node
*/

P op_toconsole(void)
{
  static B stringf[FRAMEBYTES];
  B *p; 
  B *p_;
  B *oldFREEvm;
  P retc;
  const char saves[] = "save {(";
  const char rests[] = ") toconsole restore} lock";

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

  B* max_ = VALUE_PTR(o_1) + ARRAY_SIZE(o_1);
  if ((FREEvm + ARRAY_SIZE(o_1) + (sizeof(saves)-1) + (sizeof(rests)-1)) 
      > CEILvm)
    return VM_OVF;
  p_ = VALUE_PTR(o_1);
  do {
    B* max = FREEvm + MSF_SIZE - (sizeof(saves)-1) - (sizeof(rests)-1) - 1;
    if (max > CEILvm) max = CEILvm;
    
    p = FREEvm; 
    moveB((B*)saves, p, sizeof(saves)-1); 
    p += sizeof(saves)-1;
    for (; p_ <  max_ && p < max; p_++) {
      switch (*p_) {
	case ')': case '\\': 
	  p += dm_snprintf((char*)p, max - p, "\\%c", (unsigned int) *p_);
	  break;
          
	case 0: case 1: case 2: case 3: case 4: case 5: 
	case 6: case 7: case 8: case 9: case 10: case 11: 
	case 12: case 13: case 14: case 15: case 16: case 17: 
	case 18: case 19: case 20: case 21: case 22: case 23: 
	case 24: case 25: case 26: case 27: case 28: case 29: 
	case 30: case 31: case 127:
	  p += dm_snprintf((char*)p, max - p, "\\%.3o", (unsigned int) *p_);
	  break;

	default:
	  *(p++) = *p_;
	  break;
      }
      if (p == CEILvm) return VM_OVF;
    }
    if (p + sizeof(saves)-1 > CEILvm) return VM_OVF;
    moveB((B*)rests, p, sizeof(rests)-1); 
    p += sizeof(rests)-1;
    TAG(stringf) = ARRAY | BYTETYPE; 
    ATTR(stringf) = 0;
    VALUE_PTR(stringf) = FREEvm; 
    ARRAY_SIZE(stringf) = p - FREEvm;
    oldFREEvm = FREEvm; 
    FREEvm = (B*)DALIGN(p);
    if ((retc = tompi(stringf, getparentcomm(), TRUE, 0))) {
      FREEvm = oldFREEvm;
      return retc;
    }
    FREEvm = oldFREEvm;
  } while (p_ < max_);

  FREEopds = o_1;
  return OK;
}

P op_quit(void) {exit(0);}

void run_dpawn_mill(void) {
  P retc;

  initmpi();
  maketinysetup(quithandler);

/*----------------- construct frames for use in execution of D code */
  makename((B*)"error", errorframe); 
  ATTR(errorframe) = ACTIVE;

/*-------------- you are entering the scheduler -------------------*/\
/* We start with no D code on the execution stack, so we doze
   while waiting for source (or console) activity.

   We scan sources always in round-robin fashion across snapshots;
   so we maintain a rotating source index.
*/

  moveframe(msf, cmsf);
  locked = FALSE;
  serialized = FALSE;
  while (1) {
    switch (retc = exec(100)) {
      case MORE: 
	if (locked) continue;
	retc = nextevent(cmsf);
	break;

      case DONE: 
	locked = FALSE; 
	serialized = FALSE;
	if (FREEexecs == FLOORexecs) {
	  moveframe(msf,cmsf);
	  halt_flag = FALSE;
	}
	retc = nextevent(cmsf);
	break;
	
      default:
	break;
    }
    switch (retc) {
      case OK: continue;
      case ABORT: op_abort(); continue;
      default: break;
    }

    /*------------------------------------ report an error */
    makeerror(retc, errsource);
  }  /* we never return */
}
