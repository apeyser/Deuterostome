/*=================== D machine Rev3.0 =============================

     -  dictionary functions: - make empty dictionary
                              - make dictionary of operators
     -  name object assembly, disassembly, and comparison
     -  fast moves for frames/blocks
     -  dictionary lookup and insertion
     -  mill
     -  getstartupdir

*/
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>


#include "dm.h"
#include "paths.h"
#include "dm-swapbytes.h"
#include "dm2.h"

static char sys_hi[] = "System Operators V" PACKAGE_VERSION;
P op_syshi(void)   {return wrap_hi((B*)sys_hi);}
P op_syslibnum(void) {return wrap_libnum(0);}

P wrap_libnum(UP libnum)
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = (NUM | LONGBIGTYPE);
    ATTR(o1) = 0;
    LONGBIG_VAL(o1) = libnum >> 16;

    FREEopds = o2;
    return OK;
}

P wrap_hi(B* hival)
{
    P hilen;
    LBIG index;

    if (FLOORopds > o_2) return OPDS_UNF;
    if (! VALUE(o_1, &index)) return UNDF_VAL;
    hilen = strlen((char*)hival);
    if (index + hilen > ARRAY_SIZE(o_2)) return RNG_CHK;

    strncpy((char*)VALUE_PTR(o_2) + index, (char*)hival, hilen);
    LONGBIG_VAL(o_1) = index + hilen;
    TAG(o_1) = (NUM | LONGBIGTYPE);
    ATTR(o_1) = 0;
    
    return OK;
}

B* nextlib(B* frame)
{
    if (frame == NULL)
        frame = CEILvm;
    else
        frame += FRAMEBYTES + DICT_NB(frame) + LIBBYTES;
    
  ll_nextdict:
    if (frame > TOPvm) return NULL;
    
    switch (CLASS(frame))
    {
        case DICT:
            if (TYPE(frame) == OPLIBTYPE)
              return frame;
            
            frame += FRAMEBYTES + DICT_NB(frame);
            goto ll_nextdict;

        case BOX:
            frame += FRAMEBYTES + BOX_NB(frame);
            goto ll_nextdict;
            
        case ARRAY:
            frame += DALIGN(ARRAY_SIZE(frame) * VALUEBYTES(TYPE(frame)));
            goto ll_nextdict;

        case LIST:
            frame = (B*) LIST_CEIL(frame);
            goto ll_nextdict;

        default:
          return NULL;
    };
}

static B unknown[] = "** Unknown error";
B* geterror(P e)
{
    B* frame = NULL;
    P type = e >> 16;
    P i;
    P* errc;
    B** errm;
    B* m = unknown;

    e &= 0x0000FFFFl;
    do {
      if (! (frame = nextlib(frame))) return (B*) "Lib VM corrupted";
    } while (LIB_TYPE(frame) != type);

    errc = LIB_ERRC(frame);
    errm = LIB_ERRM(frame);
    for (i = 0; errc[i]; ++i)
        if (e == errc[i]) {m = errm[i]; break;}

    return m;
}

/*----------------------------------- make empty dictionary 

  this function serves to create a new dictionary for a given
  number of entries. It automatically designs a hashtable size
  and constant and primes the hash table entries to be undefined.

  The hash constant is a prime equal or larger than twice the dictionary
  size up to a limit, then equal to the limit. The hash
  index will be the identifier key modulo the hash constant.

  The dictionary is stored in free VM space. A master frame is created
  below the dictionary value.
*/

B *makedict(L32 n)
{
  static W lastprime = 9973;
  static W primes[] = 
    { 7,13,17,19,23,31,41,53,61,79,
      101,127,157,199,251,317,397,503,631,797,
      997,1259,1579,2003,2503,3163,3989,4999,6311,7937,
      9973,
    };
  P k,nb,ne; W hcon; B *dict,*mframe;

  k = 0;
  if ((n+n) >= lastprime) hcon = lastprime;
  else while ((hcon = primes[k]) < (n+n)) k++;
  ne = n * ENTRYBYTES;
  nb = (P)(DALIGN(DICTBYTES + ne + (((P)hcon)*sizeof(LBIG))));
  if ((FREEvm + nb + FRAMEBYTES) > CEILvm) return((B *)(-1L));
  dict = FREEvm + FRAMEBYTES;
  mframe = FREEvm;  FREEvm += nb + FRAMEBYTES;

  DICT_ENTRIES(dict) = DICT_FREE(dict) = (P)dict + DICTBYTES;
  DICT_TABHASH(dict) = DICT_ENTRIES(dict) + ne;
  DICT_CONHASH(dict) = hcon;
  for (k=0; k < hcon; k++) DICT_TABHASH_ARR(dict, k) = -1L;
  TAG(mframe) = DICT; ATTR(mframe) = 0; 
  VALUE_BASE(mframe) = (P)dict; 
  DICT_NB(mframe) = nb;
  return(dict);
}

/*----------------------------------- clear associations of dictionary */

void cleardict(B *dict)
{
  P k;

  DICT_FREE(dict) = DICT_ENTRIES(dict);
  for (k=0; k< DICT_CONHASH(dict); k++)
    DICT_TABHASH_ARR(dict, k) = -1L;
}

/*----------------------------------- make dictionary of operators 

  creates a read-only dictionary that interfaces operators to D.
  Returns the address of the dictionary body created or -1L after
  a VM overflow. The dictionary is inserted in the permanent space of
  the VM, preceded by a master frame and followed by an OPLIB
  extension, which receives the addresses of lists of error codes and
  message strings that relate to the operators.

  NOTE: DICT_NB gives the dimension of the operator dictionary
  excluding the operator library extension appended to it.

  NOTE: operator dictionaries are currently the only objects kept
  in the permanent VM space; nevertheless, they are specially marked
  as containing operators such that they can be distinguished from
  other dictionary objects there (and can be scanned considering their
  OPLIB extension).

  makeopdict just redirects to makeopdictbase, which is the original
  makeoptdict, with a dictionary length added (for sysdict).
*/

B *makeopdict(B *opdefs, P *errc, B **errm) {
  return makeopdictbase(opdefs, errc, errm, 0);
}

B *makeopdictbase(B *opdefs, P *errc, B **errm, L32 n1)
{
  L32 n;
  B *opdef;
  B *dict, *frame, 
    framebuf[FRAMEBYTES], nameframe[FRAMEBYTES],
    *oldFREEvm, *newdict;

  oldFREEvm = FREEvm;
  n = 0; opdef = opdefs;
  while (OPDEF_CODE(opdef) != 0) { n++; opdef += OPDEFBYTES; };
  if (! n1) n1 = n;

  if ((dict = makedict(n1))== (B *)(-1L)) return((B *)(-1L));
  if (FREEvm + LIBBYTES > CEILvm) return((B*) -1L);
  frame = framebuf;
  TAG(frame) = OP; ATTR(frame) = ACTIVE | READONLY;
  for (opdef = opdefs; n; opdef += OPDEFBYTES, n--) {
    OP_NAME(frame) = OPDEF_NAME(opdef);
    OP_CODE(frame) = OPDEF_CODE(opdef);
    makename(((B *)OP_NAME(frame)),nameframe);
    if (!insert(nameframe,dict,frame)) return((B *)(-1L));
  }
  FREEvm = oldFREEvm; 
  CEILvm -= FRAMEBYTES + LIBBYTES + DICT_NB(oldFREEvm);
  moveD((D*) oldFREEvm, (D*) CEILvm, (DICT_NB(oldFREEvm) + FRAMEBYTES)>>3);
  newdict = CEILvm + FRAMEBYTES;
  d_reloc(newdict, (P) dict, (P) newdict);
  VALUE_BASE(CEILvm) = (P) newdict;
  TAG(CEILvm) |= OPLIBTYPE; ATTR(CEILvm) |= READONLY;
  LIB_TYPE(CEILvm) = 0;
  LIB_HANDLE(CEILvm) = 0;
  LIB_ERRC(CEILvm) = errc;
  LIB_ERRM(CEILvm) = errm;
  return(newdict);
}

/*------------------------ dictionary relocators ----------------------*/

/*-- relocate dictionary from virtual to actual addresses:
 
   d_reloc(dictbase,oldbase,newbase)

- changes all pointers in dict by newbase-oldbase
- sets up pointers into dict items before using them!
*/

void d_reloc(B *dict, P oldl, P newl)
{
  P offs, k; B *entry;

  offs = newl - oldl;
  DICT_ENTRIES(dict) += offs;
  DICT_FREE(dict) += offs;
  DICT_TABHASH(dict) += offs;
  for (k = 0; k < DICT_CONHASH(dict); k++)
    if (DICT_TABHASH_ARR(dict, k) != (-1L))
      DICT_TABHASH_ARR(dict, k) += offs;

  for (entry = (B *)DICT_ENTRIES(dict); 
       entry < (B *)DICT_FREE(dict);
       entry += ENTRYBYTES)
    if (ASSOC_NEXT(entry) != (-1L)) 
      ASSOC_NEXT(entry) += offs;
}

/*-- relocate dictionary from actual to virtual addresses:
 
   d_rreloc(dictbase,oldbase,newbase)

- changes all pointers in dict by newbase-oldbase
- uses pointers into dict items before relocating them!
*/

void d_rreloc(B *dict, P oldl, P newl)
{
  P offs, k; B *entry;

  offs = newl - oldl;
  for (k = 0; k <  DICT_CONHASH(dict); k++)
    if (DICT_TABHASH_ARR(dict, k) != -1L)
      DICT_TABHASH_ARR(dict, k) += offs;

  for (entry = (B *)DICT_ENTRIES(dict); 
       entry < (B *)DICT_FREE(dict);
       entry += ENTRYBYTES)
    if (ASSOC_NEXT(entry) != (-1L)) 
      ASSOC_NEXT(entry) += offs;

  DICT_ENTRIES(dict) += offs;
  DICT_FREE(dict) += offs;
  DICT_TABHASH(dict) += offs;
}

/* ======================== name object functions =====================

 The internal makeup of the name object is hidden inside functions
 because of its akwardness:

   ccccccccaaaaaaaa 0000001111112222
   2233333344444455 5555666666777777
   888888999999AAAA AABBBBBBCCCCCCDD
   DDDDEEEEEEFFFFFF ----GGGGGGHHHHHH

 c:    class specifier
 a:    attribute
 0..H: name string characters, encoded in six bits
 -: zeroes
   after tiling in the string, the first word is replaced by the
   name key, which is the exclusive-or checksum of the string fields
   of all words
 
 Name string characters are 0..9, A..Z, a..z, and '_'. In addition
 the name ']' is encoded by a zero in all character positions. A
 zero in positions 1-D concludes the string.

  makename(->namestring, ->nameframe) only first NAMEBYTES characters incl.
  pullname(->nameframe, ->namestring) namestring must be NAMEBYTES+1 long
  BOOLEAN matchname(->nameframe1, ->nameframe2)
  L compname(->nameframe1, ->nameframe2) n1 < n2 : <0, n1 = n2 : 0, else >0

 Name strings need to be able to hold 15 bytes.
*/

static UB tosix[] = {
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     1,2,3,4,5,6,7,8,9,10,0,0,0,0,0,0,
     0,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
     26,27,28,29,30,31,32,33,34,35,36,0,0,0,0,37,
     0,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,
     53,54,55,56,57,58,59,60,61,62,63,0,0,0,0,0
   };

static UB fromsix[] =
   "\0000123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz";

static B sb[NAMEBYTES];

void makename(B *namestring, B *nameframe)
{
  UW i; B c;
  UW* n; B *j;
  /* clear string buffer, copy string converting it to six-bit code,
     truncate after NAMEBYTES bytes
  */
  for (i = 0; (i < NAMEBYTES) && (c = namestring[i]); ++i) 
    sb[i] = tosix[c];
  for (; i<NAMEBYTES ; i++) sb[i] = 0;

  TAG(nameframe) = NAME;
  ATTR(nameframe) = 0;

/* tile nameframe longwords with 6-bit characters */
  n = ((UW*)nameframe)+1;
   for (i=0, j=sb; i < (FRAMEBYTES/sizeof(UW)-1)/3; ++i, j+=8) {
     n[i*3] = (((j[0] << 6) | j[1]) << 4) | (j[2] >> 2);
     n[i*3+1] = (((((j[2] << 6) | j[3]) << 6) | j[4]) << 2) | (j[5] >> 4);
     n[i*3+2] = (((j[5] << 6) | j[6]) << 6) | j[7];
   }
   
   switch ((FRAMEBYTES/sizeof(UW)-1)%3) {
   case 0: break;
   case 1:
     n[3*i] = (j[0] << 6) | j[1];
     break;
   case 2:
     n[3*i] = (((j[0] << 6) | j[1]) << 4) | (j[2] >> 2);
     n[3*i+1] = ((((j[2] << 6) | j[3]) << 6) | j[4]) << 2;
     break;
   };

   for (i=1; i<FRAMEBYTES/sizeof(UW)-1; ++i) n[0] ^= n[i];
}

// namestring must be NAMEBYTES+1 long
void pullname(B *nameframe, B *namestring)
{
  UW w0, w1;
  UW* n=((UW*)nameframe)+1;
  B* j;
  UW i;

  /* recover string under checksum, merge into top longword */
  w0 = 0;
  for (i=0; i < (FRAMEBYTES/sizeof(UW)-1); ++i) w0 ^= n[i];
  for (i=0, j=namestring; i < (FRAMEBYTES/sizeof(UW)-1)/3; ++i, j+=8) {
    if (i != 0) w0 = n[i*3];
    
    if (! (j[0] = fromsix[w0 >> 10])) {
      if (i == 0) {
	j[0] = ']';
	j[1] = 0;
      }
      return;
    }

    if (! (j[1] = fromsix[(w0 >> 4) & 0x3F])) return;
    w1 = n[i*3+1];
    if (! (j[2] = fromsix[((w0 << 2) | (w1 >> 14)) & 0x3F])) return;
    if (! (j[3] = fromsix[(w1 >> 8) & 0x3F])) return;
    if (! (j[4] = fromsix[(w1 >> 2) & 0x3F])) return;
    w0 = n[i*3+2];
    if (! (j[5] = fromsix[((w1 << 4) | (w0 >> 12)) & 0x3F])) return;
    if (! (j[6] = fromsix[(w0 >> 6) & 0x3F])) return;
    if (! (j[7] = fromsix[(w0 & 0x3F)])) return;
  }

  switch ((FRAMEBYTES/sizeof(UW)-1)%3) {
  case 0: break;
  case 1:
    w0 = n[i*3];
    if (! (j[0] = fromsix[(w0 >> 6) & 0x3F])) return;
    if (! (j[1] = fromsix[w0 & 0x3F])) return;
    break;
  case 2:
    w0 = n[i*3];
    if (! (j[0] = fromsix[(w0 >> 10) & 0x3F])) return;
    if (! (j[1] = fromsix[(w0 >> 4) & 0x3F])) return;
    w1 = n[i*3+1];
    if (! (j[2] = fromsix[((w0 << 2) | (w1 >> 14)) & 0x3F])) return;
    if (! (j[3] = fromsix[(w1 >> 8) & 0x3F])) return;
    if (! (j[4] = fromsix[(w1 >> 2) & 0x3F])) return;
    break;
  }

  namestring[NAMEBYTES] = 0;
}

P compname(B* nameframe1, B* nameframe2) 
{
  UW i; W r; W* n1; W* n2;
  n1 = ((W*) nameframe1)+1; n2 = ((W*) nameframe2)+1;
  for (i = 0; i < (FRAMEBYTES/sizeof(UW)-1)/3; ++i) 
    if ((r = n1[3*i] - n2[3*i])
        || (r = n1[3*i+1] - n2[3*i+1])
        || (r = n1[3*i+2] - n2[3*i+2])) 
      return r;

  switch ((FRAMEBYTES/sizeof(UW)-1)%3) {
    case 1: return n1[3*i] - n2[3*i];
    case 2: return (n1[3*i] - n2[3*i]) || (n1[3*i+1] - n2[3*i+1]);
    default: return 0;
  }
}

BOOLEAN matchname(B *nameframe1, B *nameframe2)
{
  return compname(nameframe1, nameframe2) == 0;
}

/* ===================== dictionary services ======================== */

/*--------------------------- lookup --------------------------------
returns pointer to associated frame in dictionary, or 0L if the name is
not represented.
*/

B *lookup(B *nameframe, B *dict)
{
  B* link; W h, key;

  key = NAME_KEY(nameframe);
  h = DICT_CONHASH(dict); 
  h = (key & 0x7FFF) % h;
  if ((link = (B*) DICT_TABHASH_ARR(dict, h)) == (B*) -1L)
    return NULL;

  do { 
    if (key == NAME_KEY(ASSOC_NAME(link)))
      if (matchname(nameframe, ASSOC_NAME(link)))
        return(ASSOC_FRAME(link));
  } while ((link = (B*) ASSOC_NEXT(link)) != (B*) -1L);

  return 0L;
}

/* merge entries in socket into sink */
/* return false if not enough space in sink */
BOOLEAN mergedict(B *socket, B* sink) {
  B *entry;
	size_t i;
	static B excludes_ = TRUE;
	static B *_excludes[] = {
	  (B*)"hi", (B*)"libnum", (B*)"INIT_", (B*)"FINI_"
	};
	static B excludes[sizeof(_excludes)/sizeof(_excludes[0])][FRAMEBYTES];

	if (excludes_) {
		for (i = 0; i < sizeof(_excludes)/sizeof(_excludes[0]); i++) {
			makename(_excludes[i], excludes[i]);
		}
		excludes_ = FALSE;
	}

  for (entry = (B*) DICT_ENTRIES(socket);
       entry < (B*) DICT_FREE(socket);
       entry += ENTRYBYTES) {
		for (i = 0; i < sizeof(_excludes)/sizeof(_excludes[0]); i++) {
			if (matchname(ASSOC_NAME(entry), excludes[i]))
				goto nextentry;
		}
		if (! insert(ASSOC_NAME(entry), sink, ASSOC_FRAME(entry)))
			return FALSE;

	  nextentry: ;
  }
  return TRUE;
}

/*------------------------- insertion ----------------------------------
returns TRUE if insertion was done, or FALSE on failure.
*/

BOOLEAN insert(B *nameframe, B *dict, B *framedef)
{
  W h, key; B *link, *nextlink;

  key = NAME_KEY(nameframe);
  h = DICT_CONHASH(dict); 
  h = (key & 0x7FFF) % h;

  /* prime a chain */
  if ((nextlink = (B*) DICT_TABHASH_ARR(dict, h)) == (B *)(-1L)) {
    if (DICT_FREE(dict) >= DICT_TABHASH(dict)) return FALSE;
    link = (B*) (DICT_TABHASH_ARR(dict, h) = DICT_FREE(dict));
    goto ins_name;
  }

  do {
    link = nextlink;
    if (key == NAME_KEY(ASSOC_NAME(link))
        && (matchname(nameframe, ASSOC_NAME(link))))
      goto ins_fra;
  } while ((nextlink = (B *)(ASSOC_NEXT(link))) != (B *)(-1L));

  /* reserve entry for new name; link and fill it */

  if (DICT_FREE(dict) >= DICT_TABHASH(dict)) return FALSE;
  link =  (B *)(ASSOC_NEXT(link) = DICT_FREE(dict)) ;

 ins_name:
  DICT_FREE(dict) += ENTRYBYTES;
  moveframe(nameframe, ASSOC_NAME(link)); ASSOC_NEXT(link) = -1L;

 ins_fra:
  moveframe(framedef, ASSOC_FRAME(link));
  return(TRUE);
}

/* ======================== executive ================================ 

 Executes the objects on the execution stack until one of:
    - the execution stack is empty (DONE)
    - the 'quit' operator has been executed (QUIT)
    - an error has been encountered (error code)
    - 'turns' number of objects has been executed (MORE)

 'turns' is passed by address, so it can be reset asynchronously to
 stop the mill.

 The returned longvalue is indicated in parentheses. In the case
 of an error, '_errsource' points to a string identifying the instance.
*/

P exec(L32 turns)
{
static B fetch_err[] = "fetch phase\n";
static B transl_err[] = "translation phase\n";
static B exec_err[] = "execution phase\n";
static B undfn_buf[NAMEBYTES+1];
 B *f, *af, *dict; P  retc; UB fclass;
OPER tmis;

/* ------------------------------------------- test phase */

x_t:
if ( FREEexecs <= FLOORexecs) return(DONE);
 if (turns-- <= 0) return(MORE);
 if (abortflag) { abortflag = FALSE; return(ABORT); }

/* ---------------------------------------- fetch phase */
 
fclass = CLASS(x_1);
if (fclass == LIST) goto f_list; 
if (fclass == ARRAY) goto f_arr;
if (fclass < BOX) { f = x_1; FREEexecs = x_1; goto x_e; }
errsource = fetch_err; return(CORR_OBJ);

f_arr:
if (TAG(x_1) == (ARRAY | BYTETYPE))
   { 
     if ((retc = tokenize(x_1)) != OK)
      { if (retc == DONE) { FREEexecs = x_1; goto x_t; }
        errsource = transl_err; return(retc);
      }
     f = FREEopds = o_1;
   } else f = x_1;
goto x_e;

f_list:
if (VALUE_BASE(x_1) >= LIST_CEIL(x_1))
   { FREEexecs = x_1; goto x_t; }
f = (B *)VALUE_BASE(x_1);
if ((VALUE_BASE(x_1) += FRAMEBYTES) >= LIST_CEIL(x_1))
   FREEexecs = x_1;

/* -----------------------------------------  execution phase */
x_e:
if ((ATTR(f) & ACTIVE) == 0) goto e_opd;
if ((fclass = CLASS(f)) == OP) goto e_op;
if (fclass == NAME) goto e_name;
if (fclass == NULLOBJ) goto x_t;
if (fclass > BOX) { retc = CORR_OBJ; goto e_er_1; }

e_opd:                               /* push object on operand stack */
if (FREEopds >= CEILopds) { retc = OPDS_OVF; goto e_er_1; }
moveframe(f,o1);
ATTR(o1) &= ~XMARK; // leave active alone -- might be procedure
if ((CLASS(o1)== NAME) && ((ATTR(o1) & TILDE) != 0)) {
		ATTR(o1) |= ACTIVE;
		ATTR(o1) &= ~TILDE;
}

FREEopds = o2;
goto x_t;

e_op:                                /* only C operators for the time! */
tmis = (OPER)OP_CODE(f);
if ((retc = (*tmis)()) != OK)
   { errsource = (B *)OP_NAME(f); return(retc); }
goto x_t;

e_name:
dict = FREEdicts;
while ((dict -= FRAMEBYTES) >= FLOORdicts)
   { if ((af = lookup(f, (B *)(VALUE_BASE(dict)))) != 0L)
       { f = af;
         if (ATTR(af) & ACTIVE) 
          { if (FREEexecs >= CEILexecs) { retc = EXECS_OVF; goto e_er_1; }
							moveframe(f,x1); FREEexecs = x2; goto x_t;
          } else { goto e_opd; } 
        }
   }
pullname(f,undfn_buf);  errsource = undfn_buf; return(UNDF);

e_er_1:   errsource = exec_err; return(retc);
}

/*-------------------- tree handling support --------------------------*/

DM_INLINE_STATIC void movehead(B* frame) {
  if (frame != VALUE_PTR(frame) - FRAMEBYTES)
    moveframe(frame, VALUE_PTR(frame) - FRAMEBYTES);
}


DM_INLINE_STATIC P deendian_array(B* frame, B isnonnative) {
  if (! isnonnative) return OK;
  if (HASNATIVEENDIAN(isnonnative)) return OK;

  switch (TYPE(frame)) {
    case BYTETYPE:
      return OK;

    case WORDTYPE: {
      W* w;
      for (w = (W*)VALUE_BASE(frame); 
           w < ((W*)VALUE_BASE(frame)) + ARRAY_SIZE(frame);
           ++w) {
        swap2bytes((B*) w);
      };
      return OK;
    };

    case LONG32TYPE: {
      L32* w;
      for (w = (L32*)VALUE_BASE(frame); 
           w < ((L32*)VALUE_BASE(frame)) + ARRAY_SIZE(frame);
           ++w) {
        swap4bytes((B*) w);
      };
      return OK;
    };

    case LONG64TYPE: {
      L64* l;
      for (l = (L64*)VALUE_PTR(frame); 
           l < ((L64*)VALUE_PTR(frame)) + ARRAY_SIZE(frame);
           ++l) {
        swap8bytes((B*)l);
      };
      return OK;
    };
    
    case SINGLETYPE: {
      S* l;
      for (l = (S*)VALUE_BASE(frame); 
           l < ((S*)VALUE_BASE(frame)) + ARRAY_SIZE(frame);
           ++l) {
        swap4bytes((B*) l);
      };
      return OK;
    };

    case DOUBLETYPE: {
      D* d;
      for (d = (D*)VALUE_BASE(frame); 
           d < ((D*)VALUE_BASE(frame)) + ARRAY_SIZE(frame);
           ++d) {
        swap8bytes((B*) d);
      };
      return OK;
    };

    default:
      return OPD_TYP;
  }
}

DM_INLINE_STATIC P deendian_list(B* frame, B isnonnative) {
  B* lframe;
  P retc;
  if (! isnonnative) return OK;

  for (lframe = (B*)VALUE_BASE(frame);
       lframe < (B*)LIST_CEIL(frame);
       lframe += FRAMEBYTES)
    if ((retc = deendian_frame(lframe, isnonnative)) != OK)
      return retc;
  
  return OK;
}
    

DM_INLINE_STATIC P deendian_dict(B* dict, B isnonnative) {
  if (! isnonnative) return OK;

  swaplongbytes(&DICT_ENTRIES(dict), isnonnative);
  swaplongbytes(&DICT_FREE(dict), isnonnative);
  swaplongbytes(&DICT_TABHASH(dict), isnonnative);
  if (! HASNATIVEENDIAN(isnonnative)) {
    swap2bytes((B*) &DICT_CONHASH(dict));
  }
  return OK;
}

DM_INLINE_STATIC P deendian_entries(B* dict, B isnonnative) {
  P retc, i;
  B* entry;
  if (! isnonnative) return OK;
  
  for (i = 0; i < DICT_CONHASH(dict); i++)
    swaplongbytes(&DICT_TABHASH_ARR(dict, i), isnonnative);

  for (entry = (B*) DICT_ENTRIES(dict);
       entry < (B*) DICT_FREE(dict);
       entry += ENTRYBYTES) {
    if ((retc = deendian_frame(ASSOC_NAME(entry), isnonnative)) != OK) 
      return retc;
    swaplongbytes(&ASSOC_NEXT(entry), isnonnative);
    if ((retc = deendian_frame(ASSOC_FRAME(entry), isnonnative)) != OK)
      return retc;
  }

  return OK;
}

/*----------------------------- foldobj

  - receives the frame of an internal composite object and a box base
    address (of master frame of first box value object)
  - copies the composite object value into VM freespace
  - scans all objects belonging to the copied composite object value:
      - leaves simple or external objects as unchanged
      - replaces operator objects by their name and earmarks them
      - applies itself recursively to an internal list or dict object
      - replaces internal box objects, sockets and handles 
			  referenced in dictionaries or
        contained in lists by null objects
  - returns a frame whose internal addresses are relative to base (side
    effect on VM freespace: objects belonging to lower nodes of tree)

NB: a dict is relocated in 2 steps: 1 - to the new physical mem loc
                                    2 - to the box base
*/
#define MAXDEPTH 50  /* counts depth of object nesting (<= 20) */

DM_INLINE_STATIC BOOLEAN foldsubframe(B* lframe) {
	switch (CLASS(lframe)) {
		case OP:
			makename((B *)OP_NAME(lframe),lframe);
			ATTR(lframe) |= (BIND | ACTIVE); 
			return FALSE;
		case BOX: case NULLOBJ:
			TAG(lframe) = NULLOBJ; ATTR(lframe) = 0;
			return FALSE;
	    case DICT:
		  if (TYPE(lframe) == OPAQUETYPE) {
			TAG(lframe) = NULLOBJ; ATTR(lframe) = 0;
			return FALSE;
		  }
		  return TRUE;
		default:
			return COMPOSITE(lframe);
	}
}

static B** freemem = NULL;
static B** ceilmem = NULL;
static B* vmalloc = NULL;
DM_INLINE_STATIC P foldobj_int(B *frame, P base, W *depth);

P foldobj(B *frame, P base, W *depth) 
{
		int retc;
		
		foldobj_free();
		freemem = &FREEvm;
		ceilmem = &CEILvm;
		retc = foldobj_int(frame, base, depth);
		foldobj_free();

		return retc;
}

P foldobj_ext(B* frame) 
{
		static B* freemem_ = NULL;
		static B* ceilmem_ = NULL;
		W depth = 0;
		int retc;
		static B frame_[FRAMEBYTES];

		freemem_ = FREEvm;
		moveframe(frame, frame_);		
		if ((retc = foldobj(frame, (P)FREEvm, &depth)) != VM_OVF)
				return retc;

		freemem = &freemem_;
		ceilmem = &ceilmem_;
		FREEvm = freemem_;

		moveframe(frame_, frame);
		depth = 0;
		if (! (vmalloc = (B*) malloc((FREEvm - FLOORvm))))
				return -errno;
		
		freemem_ = vmalloc;
		ceilmem_ = freemem_ + (FREEvm - FLOORvm);
		if ((retc = foldobj_int(frame, (P)freemem_, &depth)) == OK)
				return OK;

		foldobj_free();
		if (retc != VM_OVF) return retc;

		moveframe(frame_, frame);
		depth = 0;
		if (! (vmalloc = (B*) malloc((CEILvm - FLOORvm))))
				return -errno;
		
		freemem_ = vmalloc;
		ceilmem_ = freemem_ + (CEILvm - FLOORvm);
		if ((retc = foldobj_int(frame, (P)freemem_, &depth)) == OK)
				return OK;

		foldobj_free();
		return retc;
}

BOOLEAN foldobj_mem(B** base, B** top) 
{
		if (! vmalloc) return FALSE;

		*base = vmalloc;
		*top = *freemem;
		return TRUE;
}

void foldobj_free(void) 
{
		if (vmalloc) {
				free(vmalloc);
				vmalloc = NULL;
		}
		
		freemem = NULL;
		ceilmem = NULL;
}

DM_INLINE_STATIC P foldobj_int(B *frame, P base, W *depth)
{
  B *tframe, *tvalue, *value, *lframe, *entry;
  P k, retc, nb, offset;

  if ((++(*depth)) > MAXDEPTH) return(RNG_CHK);

  switch(CLASS(frame)) {
    case ARRAY: 
      tframe = *freemem; 
      tvalue = tframe + FRAMEBYTES;
      nb = (P)(DALIGN(ARRAY_SIZE(frame) * VALUEBYTES(TYPE(frame))));
      if ((*freemem+nb+FRAMEBYTES) > *ceilmem) return(VM_OVF);
      *freemem += nb + FRAMEBYTES;
      value = (B*)VALUE_BASE(frame);
      VALUE_BASE(frame) = (P)tvalue - base;
      moveframe(frame,tframe); 
      moveLBIG((LBIG*)value,(LBIG*)tvalue, nb/sizeof(LBIG));
      break;

    case LIST:  
      tframe = *freemem; 
      tvalue = tframe + FRAMEBYTES;
      nb = LIST_CEIL(frame) - VALUE_BASE(frame);
      if ((*freemem+nb+FRAMEBYTES) > *ceilmem) return(VM_OVF);
      *freemem += nb + FRAMEBYTES;
      value = VALUE_PTR(frame);
      VALUE_PTR(frame) = tvalue - base;
      LIST_CEIL(frame) = VALUE_BASE(frame) + nb;
      moveframe(frame,tframe); 
      moveframes(value, tvalue, nb/FRAMEBYTES);
      for (lframe = tvalue; 
           lframe < (tvalue + nb); 
           lframe += FRAMEBYTES) { 
        if (foldsubframe(lframe) 
            && (retc = foldobj_int(lframe,base,depth)) != OK)
          return(retc);
      }
      break;

    case DICT:  
      if (TYPE(frame) == OPAQUETYPE) return FOLD_OPAQUE;

      tframe = *freemem; 
      tvalue = tframe + FRAMEBYTES;
      value = VALUE_PTR(frame);
      nb = DICT_NB(frame); 
      if ((*freemem+nb+FRAMEBYTES) > *ceilmem) return(VM_OVF);

      *freemem += nb + FRAMEBYTES;
      VALUE_BASE(frame) = (P)tvalue - base;
      moveframe(frame,tframe); 
      moveLBIG((LBIG*)value,(LBIG*)tvalue, nb/sizeof(LBIG));
      offset = ((P)tvalue) - ((P)value);
      DICT_ENTRIES(tvalue) += offset; 
      DICT_FREE(tvalue) += offset;
      DICT_TABHASH(tvalue) += offset;
      offset -= base;
      for (k = 0; k < DICT_CONHASH(tvalue); k++)
        if (DICT_TABHASH_ARR(tvalue, k) != (-1L)) 
          DICT_TABHASH_ARR(tvalue, k) += offset;
      
      for (entry = (B *)DICT_ENTRIES(tvalue);
           entry < (B *)DICT_FREE(tvalue);
           entry += ENTRYBYTES) { 
        if (ASSOC_NEXT(entry) != (-1L))
          ASSOC_NEXT(entry) += offset;
        lframe = ASSOC_FRAME(entry);
        if (foldsubframe(lframe) 
            && (retc = foldobj_int(lframe,base,depth)) != OK) 
          return(retc);
      }
      DICT_ENTRIES(tvalue) -= base; 
      DICT_FREE(tvalue) -= base;
      DICT_TABHASH(tvalue) -= base;
      break;

    default: 
      return(CORR_OBJ);
  }

  (*depth)--;
  return(OK);
}

/*----------------------------------- unfoldobj
   - accepts frame of internal composite VM object whose in-frame and
     in-value addresses are relative to base (not usually the master
     frame)
   - adds base to the in-frame addresses
   - scans objects contained in the object value:
      - leaves simple or external objects unchanged
      - binds earmarked operator names (or leaves names if undefined)
      - applies itself recursively to internal list or dict objects
   - copies the frame into the master frame of the object
*/
  

P unfoldobj(B *frame, P base, B isnonnative)
{
  B *lframe, *dict, *entry, *dframe, *xframe, *ldict;
  P retc, k;

  switch(CLASS(frame)) {
    case ARRAY: 
      VALUE_BASE(frame) += base; 
      if ((retc = deendian_array(frame, isnonnative)) != OK)
        return retc;
      break;

    case LIST: 
      VALUE_BASE(frame) += base; 
      LIST_CEIL(frame) += base;
      if ((retc = deendian_list(frame, isnonnative)) != OK)
        return retc;

      for (lframe = (B *)VALUE_BASE(frame);
           lframe < (B *)LIST_CEIL(frame); 
           lframe += FRAMEBYTES) { 
        if (ATTR(lframe) & BIND) { 
          dframe = FREEdicts - FRAMEBYTES; xframe = 0L;
          while ((dframe >= FLOORdicts) && (xframe == 0L)) { 
            ldict = (B *)VALUE_BASE(dframe);
            xframe = lookup(lframe,ldict);//lframe not frame
            dframe -= FRAMEBYTES;
          }
          if ((P)xframe > 0) moveframe(xframe,lframe); 
          else ATTR(lframe) &= (~BIND); 
          continue;
        }   
        if (!COMPOSITE(lframe)) continue;
        if ((retc = unfoldobj(lframe, base, isnonnative)) != OK) return(retc);
      }
      break;
   
    case DICT: 
      VALUE_BASE(frame) += base;
   
      dict = (B *)VALUE_BASE(frame);
      if ((retc = deendian_dict(dict, isnonnative)) != OK) 
        return retc;

      DICT_ENTRIES(dict) += base; 
      DICT_FREE(dict) += base;
      DICT_TABHASH(dict) += base;
      if ((retc = deendian_entries(dict, isnonnative)) != OK)
        return retc;

      for (k = 0; k < DICT_CONHASH(dict); k++)
        if (DICT_TABHASH_ARR(dict, k) != (-1L)) 
          DICT_TABHASH_ARR(dict, k) += base; 

      for (entry = (B *)DICT_ENTRIES(dict); 
           entry < (B *)DICT_FREE(dict);
           entry += ENTRYBYTES) {
        if (ASSOC_NEXT(entry) != (-1L)) 
          ASSOC_NEXT(entry) += base;

        lframe = ASSOC_FRAME(entry);
        if (ATTR(lframe) & BIND) { 
          dframe = FREEdicts - FRAMEBYTES; xframe = 0L;
          while ((dframe >= FLOORdicts) && (xframe == 0L)) { 
            ldict = (B *)VALUE_BASE(dframe);
            xframe = lookup(lframe,ldict);//lframe, not frame
            dframe -= FRAMEBYTES;
          }

          if ((P)xframe > 0) moveframe(xframe,lframe); 
          else ATTR(lframe) &= (~BIND); 
          continue;
        } 
        if (!COMPOSITE(lframe)) continue;
        if ((retc = unfoldobj(lframe,base,isnonnative)) != OK) return(retc);
      }
      break;
	 
    default: 
      return(CORR_OBJ);
  }

  movehead(frame);
  return(OK);
}

/*--------------------------------------------getstartupdir
 *  --- | (directory)
 *  returns the hardcoded path (hidden at bottom of vm)
 *  to the startup directory for the node
 */
P op_getstartupdir(void)
{    
	if (CEILopds < o2) return OPDS_OVF;
	if (!startup_dir_frame) return CORR_OBJ;
	moveframe(startup_dir_frame, o1);
	FREEopds = o2;
	return OK;
}

/*---------------------------------------------------- getplugindir
  -- | string
 *  returns the hardcoded path (hidden at bottom of vm)
 *  to the plugin directory for the node
 */
P op_getplugindir(void)
{    
	if (CEILopds < o2) return OPDS_OVF;
	if (!plugin_dir_frame) return CORR_OBJ;
	moveframe(plugin_dir_frame, o1);
	FREEopds = o2;
	return OK;
}

/*--------------------------------------------------- getconfdir
 * -- | string
 * returns the hardcoded path to the per-host configuration directory
 */
P op_getconfdir(void) 
{
    if (CEILopds < o2) return OPDS_OVF;
    if (! conf_dir_frame) return CORR_OBJ;
    moveframe(conf_dir_frame, o1);
    FREEopds = o2;
    return OK;
}

/*---------------------------------------------------- gethomedir
   -- | string
   - returns $HOME
*/

P op_gethomedir(void) {
  if (CEILopds < o2) return OPDS_OVF;
  if (!home_dir_frame) return CORR_OBJ;
  moveframe(home_dir_frame, o1);
  FREEopds = o2;
  return OK;
}

DM_INLINE_STATIC void setupname(B** frame, const char* string, BOOLEAN app) {
  P len = strlen(string);
  P lenapp = len;
  if (app && (len == 0 || string[len-1] != '/')) lenapp++;

  if (FREEvm + FRAMEBYTES + DALIGN(len) > CEILvm)
    error(EXIT_FAILURE,0,"VM overflow");

  *frame = FREEvm;
  FREEvm += FRAMEBYTES + DALIGN(lenapp);
  TAG(*frame) = (ARRAY | BYTETYPE);
  ATTR(*frame) = READONLY;
  ARRAY_SIZE(*frame) = lenapp;
  VALUE_PTR(*frame) = *frame + FRAMEBYTES;
  strncpy((char*) *frame + FRAMEBYTES, string, len);
  if (app) (*frame)[FRAMEBYTES+lenapp-1] = '/';
}

#ifndef PLUGIN_DIR
#define PLUGIN_DIR ""
#endif

#ifndef CONF_DIR
#define CONF_DIR ""
#endif

void setupdirs(void) {
  const char* home_env = getenv("HOME");
  const char* home_dir = "/";
  const char* plugin_env = getenv("DNODEPLUGINPATH");
  const char* plugin_dir = PLUGIN_DIR;
  const char* startup_env = getenv("DVTSCRIPTPATH");
  const char* conf_dir = CONF_DIR;
  const char* conf_env = getenv("DMCONFDIR");
  char  myname[1024];
  char  myxname[1024];
  struct hostent* h;

  if (home_env && *home_env) home_dir = home_env;
  if (plugin_env && *plugin_env) plugin_dir = plugin_env;
  if (startup_env && *startup_env) startup_dir = startup_env;
  else startup_dir = STARTUP_DIR;
  if (conf_env && *conf_env) conf_dir = conf_env;

  if (gethostname(myname, sizeof(myname)))
    error(1, errno, "gethostname failure");

  if (! (h = gethostbyname(myname)))
    error(1, h_errno, "gethostbyname failure");
  
  memset(myxname, '*', sizeof(myxname)-1);

  setupname(&home_dir_frame, home_dir, TRUE);
  setupname(&startup_dir_frame, startup_dir, TRUE);
  setupname(&plugin_dir_frame, plugin_dir, TRUE);
  setupname(&conf_dir_frame, conf_dir, TRUE);
  setupname(&myname_frame, myname, FALSE);
  setupname(&myfqdn_frame, h->h_name, FALSE);
  setupname(&myxname_frame, myxname, FALSE);
}

void makeDmemory(B *em, LBIG specs[5])
{
  FREEopds = FLOORopds = (B*)(((((size_t) em) >> 3) + 1) << 3);
  CEILopds = FLOORopds + specs[0] * FRAMEBYTES;

  FLOORexecs = FREEexecs = CEILopds;
  CEILexecs = FLOORexecs + specs[1] * FRAMEBYTES;

  FLOORdicts = FREEdicts = CEILexecs;
  CEILdicts = FLOORdicts + specs[2] * FRAMEBYTES;

  FLOORvm = FREEvm = CEILdicts;
  TOPvm = CEILvm  = FLOORvm + specs[3] * 1000000;
}

P tosource(B* rootf, BOOLEAN mksave, SourceFunc w1, SourceFunc w2) {
  P retc = OK;
  P nb;
  B* oldFREEvm = FREEvm;

  if (FREEvm + FRAMEBYTES >= CEILvm) return VM_OVF;
  TAG(FREEvm) = BOX;
  ATTR(FREEvm) = 0;
  VALUE_PTR(FREEvm) = 0;
  FREEvm += FRAMEBYTES;

  switch (CLASS(rootf)) {
    case ARRAY:
      if (mksave && TYPE(rootf) == BYTETYPE) {
	if (FREEvm + FRAMEBYTES + ARRAY_SIZE(rootf) >= CEILvm)
	  return VM_OVF;
      
	moveframe(rootf, FREEvm);
	moveB(VALUE_PTR(rootf), FREEvm+FRAMEBYTES, ARRAY_SIZE(rootf));
	VALUE_PTR(FREEvm) = NULL;
	FREEvm += FRAMEBYTES + ARRAY_SIZE(rootf);
	break;
      };
      // otherwise fall throught

    case LIST: case DICT: {
      W d = 0;
      retc = foldobj(rootf, (P) FREEvm, &d);
    };
    break;

    case OP:
      makename((B*) OP_NAME(rootf), rootf);
      ATTR(rootf) |= (BIND|ACTIVE);
      //intentional fall through
    case NULLOBJ: case NUM: case BOOL: case MARK: case NAME:
      moveframe(rootf, FREEvm);
      FREEvm += FRAMEBYTES;
      break;

    default:
      return BAD_FMT;
  };

/*----- we give ourselves SOCK_TIMEOUT secs to get this out */
  nb = FREEvm - oldFREEvm;
  FREEvm = oldFREEvm;
  BOX_NB(FREEvm) = nb-FRAMEBYTES;
  SETNATIVE(FREEvm);
  
  if (retc) return retc;
  if ((retc = w1(FREEvm, FRAMEBYTES)) 
      || (retc = w2(FREEvm + FRAMEBYTES, nb - FRAMEBYTES)))
    return retc;

  return OK;
}

P fromsource(B* bufferf, SourceFunc r1, SourceFunc r2) {
  B isnonnative;
  P retc;
  static B xboxf[FRAMEBYTES*2];
  static B* const xrootf = xboxf+FRAMEBYTES;
  B* irootf;
  B* oldfreevm = FREEvm;

  /*----- get the root frame and evaluate */
  /*----- we give ourselves SOCK_TIMEOUT secs */
  if ((retc = r1(xboxf, FRAMEBYTES))) return retc;

  if (! GETNATIVEFORMAT(xboxf) || ! GETNATIVEUNDEF(xboxf))
    return BAD_FMT;

  isnonnative = GETNONNATIVE(xboxf);
  if ((retc = deendian_frame(xboxf, isnonnative)) != OK) return retc;
  if ((retc = deendian_frame(xrootf, isnonnative)) != OK) return retc;

  switch (CLASS(xrootf)) {
    case ARRAY:
      if (bufferf && TYPE(xrootf) == BYTETYPE) {
	if (VALUE_BASE(xrootf) != 0) return BAD_MSG;
	if (ARRAY_SIZE(xrootf) <= 0) return BAD_MSG;
	if (ARRAY_SIZE(xrootf) > ARRAY_SIZE(bufferf)) return RNG_CHK;

	// reserve this space in the passed in buffer object
	VALUE_PTR(xrootf) = VALUE_PTR(bufferf);
	VALUE_PTR(bufferf) += ARRAY_SIZE(xrootf);
	ARRAY_SIZE(bufferf) -= ARRAY_SIZE(xrootf);

	if ((retc = r2(VALUE_PTR(xrootf), ARRAY_SIZE(xrootf))))
	  return retc;

	irootf = xrootf;
	break;
      };
      // else fall through
    case LIST: case DICT: {
      B* irootf;
      B* iboxf;
      
      if (bufferf) {
	if (FREEvm + FRAMEBYTES + SBOXBYTES + BOX_NB(xboxf) >= CEILvm)
	  return VM_OVF;

	iboxf = FREEvm;
	TAG(iboxf) = BOX;
	ATTR(iboxf) = PARENT;
	VALUE_PTR(iboxf) = FREEvm + FRAMEBYTES;
	BOX_NB(iboxf) = SBOXBYTES;
	FREEvm += FRAMEBYTES;
	SBOX_CAP(FREEvm) = NULL;
	FREEvm += SBOXBYTES;
      }
      
      irootf = FREEvm;
      moveframe(xrootf, irootf);
      ATTR(irootf) = PARENT;
      FREEvm += FRAMEBYTES;

      if ((retc = r2(FREEvm, BOX_NB(xboxf)-FRAMEBYTES))) {
	FREEvm = oldfreevm;
	return retc;
      }
      FREEvm += BOX_NB(xboxf)-FRAMEBYTES;

      if ((bufferf && o2 >= CEILopds) || o1 >= CEILopds) {
	FREEvm = oldfreevm;
	return OPDS_OVF;
      }
 
/*----- relocate root object*/
      if ((retc = unfoldobj(irootf, (P) irootf, isnonnative))) {
	FREEvm = oldfreevm;
	return retc;
      }

      if (bufferf) {
	moveframe(iboxf, o1);
	FREEopds = o2;
      }

      break;
    };

    case NAME:
      if ((ATTR(xrootf) & (BIND|ACTIVE)) == (BIND|ACTIVE)) {
	B* dframe;
	B* xframe = NULL;
	for (dframe = FREEdicts - FRAMEBYTES; 
	     dframe >= FLOORdicts; 
	     dframe -= FRAMEBYTES) {
	  if ((xframe = lookup(xrootf, VALUE_PTR(dframe)))) break;
	}
	if (! xframe) ATTR(xrootf) &= ~BIND;
	else moveframe(xframe, xrootf);
      }
      // intentional fall through
    case NULLOBJ: case NUM: case BOOL: case MARK:
      irootf = xrootf;
      break;

      
    default: 
      return BAD_MSG;
  };


  if (o1 >= CEILopds) return OPDS_OVF;
  moveframe(irootf, o1);
  FREEopds = o2;
  return OK;
}
