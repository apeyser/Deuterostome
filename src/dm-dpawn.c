#include "dm.h"

#include <mpi.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "dm-dpawn.h"
#include "dm-vm.h"
#include "dm2.h"
#include "dm-mpi.h"
#include "dm5.h"
#include "error-local.h"

static P rank;
static MPI_Comm comm;
static unsigned char stype;

static P wrap_mpirook_1(B* buffer, P size) {
  mpirecv(getparentcomm(), 1, 0, (B*) &stype, sizeof(stype));
  if (stype) return mpibroadcast(getparentcomm(), 0, buffer, size);
  return mpirecv(getparentcomm(), 2, 0, buffer, size);
}

static P wrap_mpirook_2(B* buffer, P size) {
  if (stype) return mpibroadcast(getparentcomm(), 0, buffer, size);
  return mpirecv(getparentcomm(), 3, 0, buffer, size);
}

static P wrap_mpirecv_1(B* buffer, P size) {
  return mpirecv(getworldcomm(), 1, rank, buffer, size);
}

static P wrap_mpirecv_2(B* buffer, P size) {
  return mpirecv(getworldcomm(), 2, rank, buffer, size);
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

  if (o5 >= CEILopds) goto baderr;

  TAG(o1) = (NUM|LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = getpid();

  TAG(o2) = (NUM|LONGBIGTYPE);
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = getworldrank();

  TAG(o3) = (ARRAY|BYTETYPE);
  ATTR(o3) = 0;
  VALUE_PTR(o3) = (B*) "mpi error";
  ARRAY_SIZE(o3) = strlen((char*) VALUE_PTR(o3));

  TAG(o4) = (NUM|LONGBIGTYPE);
  ATTR(o4) = 0;
  LONGBIG_VAL(o4) = retc;

  TAG(buf) = (ARRAY|BYTETYPE);
  ATTR(buf) = 0;
  VALUE_PTR(buf) = buf+FRAMEBYTES;
  ARRAY_SIZE(buf) = sizeof(buf)-FRAMEBYTES;
  moveframe(buf, o5);

  FREEopds = o6;
  if (op_errormessage()) goto baderr;
  dm_error(0, "%.*s", (int) ARRAY_SIZE(o_1), VALUE_PTR(o_1));
  
 baderr:
  dm_error(0, "Unable to compose error for %lli\n", (long long) retc);
}

static P fromrook(B* bufferf) {
  return handle_error(fromsource(bufferf, wrap_mpirook_1, wrap_mpirook_2));
}

static P frommpi(P src) {
  rank = src;
  return handle_error(fromsource(NULL, wrap_mpirecv_1, wrap_mpirecv_2));
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

// -- | current-pawn-id
P op_mpirank(void) {
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = (NUM | LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = getworldrank();
  FREEopds = o2;
  return OK;
}

// -- | number-of-pawns
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

// object | --
P op_rsend(void) {
  static B xrootf[FRAMEBYTES];
  if (o_1 < FLOORopds) return OPDS_UNF;
  moveframe(o_1, xrootf);
  FREEopds = o_1;
  return tompi(xrootf, getparentcomm(), TRUE, 0);
}

// rootid object | object
// collective
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

// destid object | --
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

// srcid | object
P op_mpirecv(void) {
  P src;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &src) || src < 0 || src >= getworldsize())
    return RNG_CHK;

  return frommpi(src);
}

// srcid/* | src
P op_mpiprobe(void) {
  P src, tag, retc, count;
  if (o_1 < FLOORopds) return OPDS_UNF;

  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &src)) src = MPI_ANY_SOURCE;
  else if (src < 0 || src >= getworldsize()) return RNG_CHK;
  tag = MPI_ANY_TAG;

  if ((retc = mpiprobe(getworldcomm(), &tag, &src, &count)))
    return retc;
  
  TAG(o_1) = (NUM|LONGBIGTYPE);
  LONGBIG_VAL(o_1) = src;
  return OK;
}

// srcid/* | false/src true  
P op_mpiiprobe(void) {
  P src, tag, retc, count;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (o1 >= CEILopds) return OPDS_OVF;

  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &src)) src = MPI_ANY_SOURCE;
  else if (src < 0 || src >= getworldsize()) return RNG_CHK;
  tag = MPI_ANY_TAG;

  switch ((retc = mpiiprobe(getworldcomm(), &tag, &src, &count))) {
    case OK:
      TAG(o_1) = (NUM|LONGBIGTYPE);
      LONGBIG_VAL(o_1) = src;
      TAG(o1) = BOOL;
      ATTR(o1) = 0;
      BOOL_VAL(o1) = TRUE;
      FREEopds = o2;
      break;

    case MPI_NOMSG:
      TAG(o_1) = BOOL;
      BOOL_VAL(o_1) = FALSE;
      FREEopds = o1;
      break;

    default:
      return retc;
  }
  return OK;
}

P op_vmresize(void) {
  return op_vmresize_();
}

/* push on operand stack:
   error code    (top)
   errsource string
   rank#
   pid
   and push active name 'error' on execution stack
*/
void makeerror(P retc, B* error_source) {   
  if (o4 >= CEILopds) FREEopds = FLOORopds;
  if (x1 >= CEILexecs) FREEexecs = FLOORexecs;

  TAG(o1) = (NUM | LONGBIGTYPE);
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (LBIG) getpid();

  TAG(o2) = (NUM | LONGBIGTYPE);
  ATTR(o2) = 0;
  LONGBIG_VAL(o2) = getworldrank();

  TAG(o3) = (ARRAY | BYTETYPE);
  ATTR(o3) = READONLY;
  VALUE_PTR(o3) = error_source; 
  ARRAY_SIZE(o3) = strlen((char*)error_source);

  TAG(o4) = (NUM | LONGBIGTYPE);
  ATTR(o4) = 0;
  LONGBIG_VAL(o4) = retc;

  moveframe(errorframe, x1);
  FREEopds = o5;
  FREEexecs = x2;
}

static BOOLEAN pending(void) {
  if (halt_flag) {
    if (x_1 >= FLOORexecs
	&& TAG(x_1) == OP
	&& OP_CODE(x_1) == x_op_halt)
      return FALSE;
  }

  return (recvd_quit || FREEexecs != FLOORexecs);
}

static P clientinput(B* bufferf) {
  P retc;
  if ((retc = fromrook(bufferf))) return retc;

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
    checkabort();
    
    src = 0;
    tag = 1;
    switch (retc = pending() ? mpiiprobe(parent, &tag, &src, &count)
	                     : mpiprobe(parent, &tag, &src, &count)) {
      case OK: break;
      case MPI_NOMSG: retc = OK; continue;
      default: continue;
    }
    
    retc = clientinput(bufferf);
  } while (! retc && ! pending());

  return retc;
}

static BOOLEAN groupconsole = FALSE;
static B groupconsole_buf[MSF_SIZE];
static P groupconsole_len = 0;
static const char saves[] = "save {(";
static const char rests[] = ") toconsole restore} lock";

DM_INLINE_STATIC P clear_toconsole(void) {
  static B stringf[FRAMEBYTES];
  P retc;

  if (! groupconsole_len) return OK;

  if (groupconsole_len + sizeof(rests) - 1 > sizeof(groupconsole_buf)) 
    return BUF_OVF;
  moveB((B*) rests, groupconsole_buf + groupconsole_len, sizeof(rests)-1);
  groupconsole_len += sizeof(rests)-1;

  TAG(stringf) = ARRAY | BYTETYPE; 
  ATTR(stringf) = 0;
  VALUE_PTR(stringf) = groupconsole_buf; 
  ARRAY_SIZE(stringf) = groupconsole_len;
  groupconsole_len = 0;
  if ((retc = tompi(stringf, getparentcomm(), TRUE, 0))) {
    dm_error_msg(0, "Unable to propagate: %s", stringf);
    return retc;
  }

  return OK;
}

/*-------------------------------------- 'error'
  - expects on operand stack:
     error code    (top)
     errsource string
     rank#
     pid
  - prints message on current console or startup
    terminal (default)
  - aborts on corrupted error info
  - halts after uncorrupted error
*/

P op_error(void)
{
  LBIG e, r, pid;
  P nb, atmost; 
  B *m, strb[256], *p;
  P ret;
  BOOLEAN groupconsole_ = groupconsole;

  p = strb; 
  atmost = 255;
  if (o_4 < FLOORopds) goto baderror;
  if (CLASS(o_4) != NUM) goto baderror;
  if (! VALUE(o_4, &pid)) goto baderror;
  if (CLASS(o_3) != NUM) goto baderror;
  if (! VALUE(o_3, &r)) goto baderror;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_1) != NUM) goto baderror;
  if (!VALUE(o_1, &e)) goto baderror;
      
  nb = dm_snprintf((char*)p, atmost, "\033[31mOn rank %llu, pid %llu: ",
                   (unsigned long long) r,
		   (unsigned long long) pid);
      
  p += nb;
  atmost -= nb;

  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*)p,atmost,"%s",(char*)strerror((P)-e));
  else { /* one of our error codes: decode */
    m = geterror((P)e);
    nb = dm_snprintf((char*)p,atmost,"%s",(char*)m);
  }
  p += nb;
  atmost -= nb;

  nb = dm_snprintf((char*)p, atmost, " in %*s\033[0m\n",
		   (int) ARRAY_SIZE(o_2),
		   (char*) VALUE_BASE(o_2));
  nb += (P)(p - strb);

  TAG(o_4) = ARRAY | BYTETYPE; 
  ATTR(o_4) = READONLY;
  VALUE_BASE(o_4) = (P) strb; 
  ARRAY_SIZE(o_4) = nb;
  FREEopds = o_3;

  clear_toconsole();
  groupconsole = FALSE;
  op_toconsole();
  groupconsole = groupconsole_;
  if ((ret = op_halt()) == DONE) return DONE;

  nb = dm_snprintf((char*) p, atmost, "%s",
		   "** Error in internal halt!\n");
  goto baderror2;

 baderror: 
  nb = dm_snprintf((char*) p, atmost, "%s",
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
     rank# int
     pid int
  - composes an error message and returns it in a subarray of string buffer
*/

P op_errormessage(void)
{
  LBIG e, r, pid;
  P nb, tnb;
  B *m, *s;

  if (o_5 < FLOORopds) goto baderror;
  if (CLASS(o_5) != NUM) goto baderror;
  if (! VALUE(o_5, &pid)) goto baderror;
  if (CLASS(o_4) != NUM) goto baderror;
  if (! VALUE(o_4, &r)) goto baderror;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) goto baderror;
  if (CLASS(o_2) != NUM) goto baderror;
  if (! VALUE(o_2,&e)) goto baderror;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) goto baderror;

  s = (B *)VALUE_BASE(o_1); 
  tnb = ARRAY_SIZE(o_1);
  nb = dm_snprintf((char*) s, tnb, "On rank %llu, pid %llu: ",
		   (unsigned long long) r,
		   (unsigned long long) pid);
  s += nb;
  tnb -= nb;

  if ((P)e < 0) /*Clib error */
    nb = dm_snprintf((char*) s, tnb, "%s",
		     (char*) strerror(-e));
  else { /* one of our error codes: decode */
    m = geterror((P) e);
    nb = strlen((char*) m);
    if (nb > tnb) nb = tnb;
    moveB(m, s, nb);
  }
  s += nb;
  tnb -= nb;

  nb = dm_snprintf((char*) s, tnb, " in %*s\n",
		   (int) ARRAY_SIZE(o_3),
		   (char*) VALUE_BASE(o_3));

  ARRAY_SIZE(o_1) = (P)(s + nb) - VALUE_BASE(o_1);
  moveframe(o_1, o_5);
  FREEopds = o_4;
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
  B *p; 
  B *p_;
  P retc;
  B* max_;
  B* max;
  static BOOLEAN nl = TRUE;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

  if (groupconsole_len
      && (groupconsole_len + ARRAY_SIZE(o_1) + sizeof(rests) - 1 
	  > sizeof(groupconsole_buf))
      && ((retc = clear_toconsole()))) return retc;

  if ((sizeof(saves) - 1 + ARRAY_SIZE(o_1) +  sizeof(rests) - 1 
       > sizeof(groupconsole_buf)))
    return BUF_OVF;
  
  p_ = VALUE_PTR(o_1);
  max_ = VALUE_PTR(o_1) + ARRAY_SIZE(o_1);
  max = groupconsole_buf + sizeof(groupconsole_buf) - (sizeof(rests)-1);
  do {
    p = groupconsole_buf + groupconsole_len; 
    if (! groupconsole_len) {
      moveB((B*)saves, p, sizeof(saves)-1); 
      p += sizeof(saves)-1;
    }
    for (; p_ <  max_ && p < max;) {
      if (nl) {
	p += dm_snprintf((char*) p, max - p, "%lli: ", 
			 (long long) getworldrank());
	nl = FALSE;
	continue;
      }
      switch (*p_) {
	case ')': case '\\': 
	  p += dm_snprintf((char*)p, max - p, "\\%c", (unsigned int) *(p_++));
	  break;
          
	case 10: nl = TRUE;
	  //intentional fall through
	case 0: case 1: case 2: case 3: case 4: case 5: 
	case 6: case 7: case 8: case 9: case 11: 
	case 12: case 13: case 14: case 15: case 16: case 17: 
	case 18: case 19: case 20: case 21: case 22: case 23: 
	case 24: case 25: case 26: case 27: case 28: case 29: 
	case 30: case 31: case 127:
	  p += dm_snprintf((char*)p, max - p, "\\%.3o", (unsigned int) *(p_++));
	  break;

	default:
	  *(p++) = *(p_++);
	  break;
      }
    }

    groupconsole_len = p - groupconsole_buf;
    if ((p >= max || ! groupconsole)  && ((retc = clear_toconsole())))
      return retc;
  } while (p_ < max_);

  FREEopds = o_1;
  return OK;
}

static P x_op_groupconsole(void) {
  groupconsole = FALSE;

  clear_toconsole();  
  return repush_stop();
}

// ~active | -- 
P op_groupconsole(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;
  if (groupconsole) return op_exec();

  if (CEILexecs < x4) return EXECS_OVF;

  TAG(x1) = OP;
  ATTR(x1) = ACTIVE;
  OP_NAME(x1) = "x_groupconsole";
  OP_CODE(x1) = x_op_groupconsole;
  
  TAG(x2) = BOOL;
  ATTR(x2) = (STOPMARK | ABORTMARK | ACTIVE);
  BOOL_VAL(x2) = FALSE;

  moveframe(o_1, x3);
  FREEexecs = x4;
  FREEopds = o_1;
  groupconsole = TRUE;

  return OK;
}

void run_dpawn_mill(void) {
  P retc;
  B abortframe[FRAMEBYTES];

  setuphandlers();
  initmpi();
  maketinysetup();

/*----------------- construct frames for use in execution of D code */
  makename((B*) "error", errorframe); 
  ATTR(errorframe) = ACTIVE;

  makename((B*) "abort", abortframe); 
  ATTR(abortframe) = ACTIVE;

/*-------------- you are entering the scheduler -------------------*/\
/* We start with no D code on the execution stack, so we doze
   while waiting for source (or console) activity.

   We scan sources always in round-robin fashion across snapshots;
   so we maintain a rotating source index.
*/

  moveframe(msf, cmsf);
  locked = FALSE;
  serialized = FALSE;
  groupconsole = FALSE;
  while (1) {
    switch (retc = exec(100)) {
      case DONE: 
	locked = FALSE; 
	serialized = FALSE;
	if (FREEexecs == FLOORexecs) {
	  moveframe(msf,cmsf);
	  halt_flag = FALSE;
	  groupconsole = FALSE;
	}

	//intentional fall-throught
      case MORE:
	retc = nextevent(cmsf);
	break;

      case TERM: die();
	
      default: break;
    };

    switch (retc) {
      case OK: continue;

      case ABORT:
	abortflag = FALSE;
	if (x1 >= CEILexecs) {
	  retc = EXECS_OVF;
	  errsource = (B*) "supervisor";
	  break;
	}

	moveframe(abortframe, x1);
	FREEexecs = x2;
	continue;

      case QUIT:
	if ((retc = quit())) {
	  errsource = (B*) "supervisor";
	  break;
	}
	continue;

      default: break;
    }

    /*------------------------------------ report an error */
    makeerror(retc, errsource);
  }  /* we never return */
}

static P x_op_done(void) {
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != BOOL) return OPD_CLA;

  FREEopds = o_1;
  return BOOL_VAL(o1) ? ABORT : DONE;
}

P recur_dpawn_mill(B* execf) {
  P retc;
  B* done;

  if (CEILexecs < x4) return EXECS_OVF;
  
  done = x1;
  TAG(x1) = OP;
  ATTR(x1) = ACTIVE;
  OP_NAME(x1) = "x_done";
  OP_CODE(x1) = x_op_done;

  TAG(x2) = BOOL;
  ATTR(x2) = (ABORTMARK | ACTIVE);
  BOOL_VAL(x2) = FALSE;

  moveframe(execf, x3);
  FREEexecs = x4;

  while (1) {
    switch (retc = exec(100)) {
      case DONE:
	return OK;

      case MORE:
	if (locked) continue;
	if (! (retc = nextevent(cmsf))) break;
	//else fall through to default
      
      default:
	if (FREEexecs > done) FREEexecs = done;
	return retc;
    }
  }
}

