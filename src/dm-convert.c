#include "dm.h"
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

#include "dm-types.h"

#if DM_HOST_IS_32_BIT
typedef int32_t L;
typedef uint32_t UL;

#define LONGTYPE                       ((UB) 0x02)
#define Z32_SINGLETYPE                 ((UB) 0x03)
#define Z32_DOUBLETYPE                 ((UB) 0x04)      

#ifndef DM_WORDS_BIGENDIAN
#define Z32_GETNATIVEENDIAN(frame)  \
  ((BOOLEAN) ((FORMAT(frame) & ENDIANMASK) == LITTLEENDIAN))
#define Z32_SETNATIVEENDIAN(frame)  FORMAT(frame) |= LITTLEENDIAN
#else //! DM_WORDS_BIGENDIAN
#define Z32_GETNATIVEENDIAN(frame)  \
  ((BOOLEAN) ((FORMAT(frame) & ENDIANMASK) == BIGENDIAN))
#define Z32_SETNATIVEENDIAN(frame)  FORMAT(frame) |= BIGENDIAN
#endif //DM_WORDS_BIGENDIAN

#define Z32_GETNATIVEFORMAT(frame) \
  ((BOOLEAN) ((FORMAT(frame) & FORMATMASK) == FORMAT32))
#define Z32_SETNATIVEFORMAT(frame) FORMAT(frame) |= FORMAT32

#define Z32_GETNATIVE(frame) (Z32_GETNATIVEFORMAT(frame) && Z32_GETNATIVEENDIAN(frame))
#define Z32_SETNATIVE(frame) \
  FORMAT(frame) = 0; \
  Z32_SETNATIVEFORMAT(frame); \
  Z32_SETNATIVEENDIAN(frame)

#define Z32_GETNATIVEUNDEF(frame) \
  ((BOOLEAN) ! (FORMAT(frame) & ~(FORMATMASK | ENDIANMASK)))

#define Z32_NUM_VAL(frame)             ( ((B *)(((B*)(frame))+4)))
#define Z32_LONG_VAL(frame)            (*((L *)((frame)+4)))
#define Z32_BOOL_VAL(frame)            (*((BOOLEAN *)((frame)+2)))
#define Z32_NAME_KEY(frame)            (*((W *)((frame)+2)))

#define Z32_OP_CODE(frame)             (*((L *)(((B*)(frame))+4)))
#define Z32_OP_NAME(frame)             (*((L *)(((B*)(frame))+8)))
#define Z32_VALUE_BASE(frame)          (*((L *)(((B*)(frame))+4)))
#define Z32_ARRAY_SIZE(frame)          (*((L *)(((B*)(frame))+8)))
#define Z32_LIST_CEIL(frame)           (*((L *)(((B*)(frame))+8)))
#define Z32_DICT_NB(frame)             (*((L *)(((B*)(frame))+8)))
#define Z32_DICT_CURR(frame)           (*((L *)(((B*)(frame))+8)))
#define Z32_BOX_NB(frame)              (*((L *)(((B*)(frame))+8)))
#define Z32_VALUE_PTR(frame)           (*((B**)(((B*)(frame))+4)))
#define Z32_LIST_CEIL_PTR(frame)       (*((B**)(((B*)(frame))+8)))

/* NB: Attention to moveframe & moveframes in dm2.c whenever
   framebytes is changed */
#define Z32_FRAMEBYTES                 16L

// NAMEBYTES is defined in ../config.h
// To change, update ../configure.ac
#define Z32_NAMEBYTES                  18L //not including a terminating 0

/*-------------------------------------------- dictionary */
 
#define Z32_ASSOC_NAME(entry)          ( ((B *)(entry)))
#define Z32_ASSOC_NEXT(entry)          (*((L *)(((B*)(entry))+Z32_FRAMEBYTES)))
#define Z32_ASSOC_FRAME(entry)         (((B*)(entry))+Z32_ENTRYBYTES-Z32_FRAMEBYTES)

// keep frame on 64 bit boundaries, so that doubles will be so.
#define Z32_ENTRYBYTES  (8+2*Z32_FRAMEBYTES)

#define Z32_DICT_ENTRIES(dict)         (*((L *)(dict)))
#define Z32_DICT_FREE(dict)            (*((L *)(((B*)(dict))+4)))
#define Z32_DICT_CEIL(dict)            (*((L *)(((B*)(dict))+8)))
#define Z32_DICT_CONHASH(dict)         (*((W *)(((B*)(dict))+12)))
#define Z32_DICT_TABHASH(dict)         (*((L *)(((B*)(dict))+8)))

#define Z32_DICTBYTES                  16L

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

static UB fromsix[] =
   "\0000123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz";

// namestring must be Z32_NAMEBYTES+1 long
DM_INLINE_STATIC void Z32_pullname(B *nameframe, B *namestring)
{
  UW w0, w1;
  
  /* recover string under checksum, merge into top longword */
  w0 = *(UW *)(nameframe+2) 
    ^ *(UW *)(nameframe+4)
    ^ *(UW *)(nameframe+6) 
    ^ *(UW *)(nameframe+8)
    ^ *(UW *)(nameframe+10) 
    ^ *(UW *)(nameframe+12)
    ^ *(UW *)(nameframe+14);
  w1 = *(UW *)(nameframe+4);

  if (! (namestring[0] = fromsix[(w0 >> 10)])) {
    namestring[0] = ']'; 
    namestring[1] = 0; 
    return;
  };
  if (! (namestring[1] = fromsix[(w0 >> 4) & 0x3F])) return;
  if (! (namestring[2] = fromsix[((w0 << 2) | (w1 >> 14)) & 0x3F])) return;

  w0 = *(UW *)(nameframe+6);
  if (! (namestring[3] = fromsix[(w1 >> 8) & 0x3F])) return;
  if (! (namestring[4] = fromsix[(w1 >> 2) & 0x3F])) return;
  if (! (namestring[5] = fromsix[((w1 << 4) | (w0 >> 12)) & 0x3F])) return;
  
  w1 = *(UW *)(nameframe+8);
  if (! (namestring[6] = fromsix[(w0 >> 6) & 0x3F])) return;
  if (! (namestring[7] = fromsix[w0 & 0x3F])) return;
  
  w0 = *(UW *)(nameframe+10);
  if (! (namestring[8] = fromsix[(w1 >> 10)])) return;
  if (! (namestring[9] = fromsix[(w1 >> 4) & 0x3F])) return;
  if (! (namestring[10] = fromsix[((w1 << 2) | (w0 >> 14)) & 0x3F])) return;

  w1 = *(UW *)(nameframe+12);
  if (! (namestring[11] = fromsix[(w0 >> 8) & 0x3F])) return;
  if (! (namestring[12] = fromsix[(w0 >> 2) & 0x3F])) return;
  if (! (namestring[13] = fromsix[((w0 << 4) | (w1 >> 12)) & 0x3F])) return;
  
  w0 = *(UW *)(nameframe+14);
  if (! (namestring[14] = fromsix[(w1 >> 6) & 0x3F])) return;
  if (! (namestring[15] = fromsix[w1 & 0x3F])) return;
  if (! (namestring[16] = fromsix[(w0 >> 6) & 0x3F])) return;
  if (! (namestring[17] = fromsix[w0 & 0x3F])) return;

  namestring[Z32_NAMEBYTES] = 0;
}

/* ======================== move frame(s) =============================
  NOTA BENE: this implies FRAMEBYTES = 16 for sake of speed!
*/

DM_INLINE_STATIC void Z32_moveframe(B *socket, B *dest)
{
D *s,*d;

s = (D *)socket; d = (D *)dest;
*(d++) = *(s++); *(d++) = *(s++);

}

/* ===================== dictionary services ======================== */

/*-------------------- tree handling support --------------------------*/

DM_INLINE_STATIC void swapbytes(B* arr, B n1, B n2) {
  B temp = arr[n2];
  arr[n2] = arr[n1];
  arr[n1] = temp;
}

DM_INLINE_STATIC void swap2bytes(B* arr) {swapbytes(arr, 0, 1);}

DM_INLINE_STATIC void swap4bytes(B* arr) {
  swapbytes(arr, 0, 3);
  swapbytes(arr, 1 ,2);
}

DM_INLINE_STATIC void swap8bytes(B* arr) {
  swapbytes(arr, 0, 7);
  swapbytes(arr, 1, 6);
  swapbytes(arr, 2, 5);
  swapbytes(arr, 3, 4);
}

DM_INLINE_STATIC void movehead(B* frame) {
  if (frame != Z32_VALUE_PTR(frame) - Z32_FRAMEBYTES)
    Z32_moveframe(frame, Z32_VALUE_PTR(frame) - Z32_FRAMEBYTES);
}

DM_INLINE_STATIC L Z32_deendian_frame(B *frame) {
  switch (CLASS(frame)) {
    case NULLOBJ: case BOOL: case MARK:
      return OK;

    case NUM:
      switch (TYPE(frame)) {
	case BYTETYPE:
	  return OK;
	case WORDTYPE:
	  swap2bytes(Z32_NUM_VAL(frame));
	  return OK;
	case LONGTYPE: case Z32_SINGLETYPE:
	  swap4bytes(Z32_NUM_VAL(frame));
	  return OK;
	case Z32_DOUBLETYPE:
	  swap8bytes(Z32_NUM_VAL(frame));
	  return OK;
      };
      return OPD_TYP;

    case OP:
      return OPD_CLA;

    case NAME:
      swap2bytes(frame+2);
      swap2bytes(frame+4);
      swap2bytes(frame+6);
      swap2bytes(frame+8);
      swap2bytes(frame+10);
      swap2bytes(frame+12);
      swap2bytes(frame+14);
      return OK;

    case ARRAY: case LIST:
      swap4bytes((B*) &Z32_VALUE_BASE(frame));
      swap4bytes((B*) &Z32_ARRAY_SIZE(frame));
      //movehead(frame);
      return OK;

    case DICT:
      swap4bytes((B*) &Z32_VALUE_BASE(frame));
      swap4bytes((B*) &Z32_DICT_NB(frame));
      //CURR=NB
      //movehead(frame);
      return OK;

    case BOX:
      swap4bytes((B*) &Z32_VALUE_BASE(frame));
      swap4bytes((B*) &Z32_BOX_NB(frame));
      //movehead(frame);
      return OK;

    default:
      return OPD_CLA;
  };
}

DM_INLINE_STATIC L deendian_array(B* frame) {
  switch (TYPE(frame)) {
    case BYTETYPE:
      return OK;

    case WORDTYPE: {
      W* w;
      for (w = (W*)Z32_VALUE_BASE(frame); 
	   w < ((W*)Z32_VALUE_BASE(frame)) + Z32_ARRAY_SIZE(frame);
	   ++w) {
	swap2bytes((B*) w);
      };
      return OK;
    };

    case LONGTYPE: case Z32_SINGLETYPE: {
      L* l;
      for (l = (L*)Z32_VALUE_BASE(frame); 
	   l < ((L*)Z32_VALUE_BASE(frame)) + Z32_ARRAY_SIZE(frame);
	   ++l) {
	swap4bytes((B*) l);
      };
      return OK;
    };

    case Z32_DOUBLETYPE: {
      D* d;
      for (d = (D*)Z32_VALUE_BASE(frame); 
	   d < ((D*)Z32_VALUE_BASE(frame)) + Z32_ARRAY_SIZE(frame);
	   ++d) {
	swap8bytes((B*) d);
      };
      return OK;
    };

    default:
      return OPD_TYP;
  }
}

DM_INLINE_STATIC L deendian_list(B* frame) {
  B* lframe;
  L retc;

  for (lframe = (B*)Z32_VALUE_BASE(frame);
       lframe < (B*)Z32_LIST_CEIL(frame);
       lframe += Z32_FRAMEBYTES)
    if ((retc = Z32_deendian_frame(lframe)) != OK)
      return retc;

  return OK;
}
    

DM_INLINE_STATIC L deendian_dict(B* dict) {
  swap4bytes((B*) &Z32_DICT_ENTRIES(dict));
  swap4bytes((B*) &Z32_DICT_FREE(dict));
  swap4bytes((B*) &Z32_DICT_CEIL(dict));
  swap2bytes((B*) &Z32_DICT_CONHASH(dict));
  //TABHASH==CEIL
  return OK;
}

DM_INLINE_STATIC L deendian_entries(B* dict) {
  L retc, i, *link;
  B* entry;

  for (i = 0, link = (L*)Z32_DICT_TABHASH(dict);
       i < Z32_DICT_CONHASH(dict); 
       i++, link++)
    swap4bytes((B*) link);

  for (entry = (B*) Z32_DICT_ENTRIES(dict);
       entry < (B*) Z32_DICT_FREE(dict);
       entry += Z32_ENTRYBYTES) {
    if ((retc = Z32_deendian_frame(ASSOC_NAME(entry))) != OK) 
      return retc;
    swap4bytes((B*) &Z32_ASSOC_NEXT(entry));
    if ((retc = Z32_deendian_frame(ASSOC_FRAME(entry))) != OK)
      return retc;
  }

  return OK;
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
  

DM_INLINE_STATIC L Z32_unfoldobj(B *frame, L base, BOOLEAN isnative)
{
B *lframe, *dict, *entry;
L retc, k, *link;

switch(CLASS(frame)) {
 case ARRAY: 
   Z32_VALUE_BASE(frame) += base; 
   if (! isnative && ((retc = deendian_array(frame)) != OK))
     return retc;
   break;

 case LIST: 
   Z32_VALUE_BASE(frame) += base; 
   Z32_LIST_CEIL(frame) += base;
   if (! isnative && ((retc = deendian_list(frame)) != OK))
     return retc;

   for (lframe = (B *)Z32_VALUE_BASE(frame);
				lframe < (B *)Z32_LIST_CEIL(frame); 
				lframe += Z32_FRAMEBYTES) { 
     if (!COMPOSITE(lframe)) continue;
     if ((retc = Z32_unfoldobj(lframe, base, isnative)) != OK) return(retc);
   }
   break;
   
 case DICT: 
   Z32_VALUE_BASE(frame) += base;
   
   dict = (B *)Z32_VALUE_BASE(frame);
   if (! isnative && ((retc = deendian_dict(dict)) != OK)) 
     return retc;

   Z32_DICT_ENTRIES(dict) += base; Z32_DICT_FREE(dict) += base;
   Z32_DICT_CEIL(dict) += base;
   if (! isnative && ((retc = deendian_entries(dict)) != OK))
     return retc;

   for (k = 0, link = (L *)Z32_DICT_TABHASH(dict); 
				k < Z32_DICT_CONHASH(dict); 
				k++, link++)  { 
     if (*link != (-1L)) *link += base; 
   }
   for (entry = (B *)Z32_DICT_ENTRIES(dict); 
				entry < (B *)Z32_DICT_FREE(dict);
				entry += Z32_ENTRYBYTES) {
     if (Z32_ASSOC_NEXT(entry) != (-1L)) Z32_ASSOC_NEXT(entry) += base;
     lframe = Z32_ASSOC_FRAME(entry);   
     if (!COMPOSITE(lframe)) continue;
     if ((retc = Z32_unfoldobj(lframe,base,isnative)) != OK) return(retc);
   }
   break;
	 
	default: 
		return(CORR_OBJ);
 }
 movehead(frame);
 return(OK);
}

DM_INLINE_STATIC P Z32_convert(B* s, B* f) {
  B* df; B* sf; B* ldict; 
  B namestring[Z32_NAMEBYTES+1];
  B xf[FRAMEBYTES], xf2[FRAMEBYTES];
  P retc;

  TAG(f) = TAG(s);
  ATTR(f) = ATTR(s);

  switch (CLASS(s)) {
    case NULLOBJ:
      break;

    case NAME:
      Z32_pullname(s, namestring);
      makename(namestring, f);
      if ((ATTR(f) = ATTR(s)) & BIND) {
	df = FREEdicts - FRAMEBYTES;
	sf = 0;
	while ((df >= FLOORdicts) && ! sf) {
	  ldict = VALUE_PTR(df);
	  sf = lookup(f, ldict);
	  df -= FRAMEBYTES;
	}
	if (sf) moveframe(sf, f);
	else ATTR(f) &= ~BIND;
      }
      break;
      
    case NUM:
      switch (TYPE(f)) {
	case Z32_SINGLETYPE: TAG(f) = NUM | SINGLETYPE; break;
	case Z32_DOUBLETYPE: TAG(f) = NUM | DOUBLETYPE; break;
	case LONGTYPE: TAG(f) = NUM | LONG32TYPE; break;
      }
      switch (TYPE(f)) {
	case BYTETYPE: *(B*)NUM_VAL(f) = *(B*)Z32_NUM_VAL(s); break;
	case WORDTYPE: *(W*)NUM_VAL(f) = *(W*)Z32_NUM_VAL(s); break;
	case LONG32TYPE: *(L32*)NUM_VAL(f) = *(L*)Z32_NUM_VAL(s); break;
	case SINGLETYPE: *(S*)NUM_VAL(f) = *(S*)Z32_NUM_VAL(s); break;
	case DOUBLETYPE: *(D*)NUM_VAL(f) = *(D*)Z32_NUM_VAL(s); break;
      };
      break;
      
    case BOOL: 
      BOOL_VAL(f) = BOOL_VAL(s);
      break;

    case MARK:
      break;

    case ARRAY:
      switch (TYPE(f)) {
	case Z32_SINGLETYPE: TAG(f) = ARRAY | SINGLETYPE; break;
	case Z32_DOUBLETYPE: TAG(f) = ARRAY | DOUBLETYPE; break;
	case LONGTYPE: TAG(f) = ARRAY | LONG32TYPE; break;
      }

      df = FREEvm;
      VALUE_PTR(f) = FREEvm + FRAMEBYTES;
      ARRAY_SIZE(f) = Z32_ARRAY_SIZE(s);
      FREEvm += DALIGN(FRAMEBYTES+ARRAY_SIZE(f)*VALUEBYTES(TYPE(f)));
      if (FREEvm >= CEILvm) return VM_OVF;
      moveframe(f, df);
      moveB(Z32_VALUE_PTR(s), VALUE_PTR(f), ARRAY_SIZE(f)*VALUEBYTES(TYPE(f)));
      break;

    case LIST:
      df = FREEvm;
      VALUE_PTR(f) = FREEvm + FRAMEBYTES;
      LIST_CEIL_PTR(f) = VALUE_PTR(f) 
	+ FRAMEBYTES*((Z32_LIST_CEIL_PTR(s)-Z32_VALUE_PTR(s))/Z32_FRAMEBYTES);
      FREEvm = LIST_CEIL_PTR(f);
      if (FREEvm >= CEILvm) return VM_OVF;

      moveframe(f, df);
      for (df = VALUE_PTR(f), sf = Z32_VALUE_PTR(s); 
           df < LIST_CEIL_PTR(f); 
           df += FRAMEBYTES, sf += Z32_FRAMEBYTES)
        if ((retc = Z32_convert(sf, df)) != OK) return retc;
      break;

    case DICT:
      sf = Z32_VALUE_PTR(s);
      if ((VALUE_PTR(f) 
           = makedict((Z32_DICT_TABHASH(sf) - Z32_DICT_ENTRIES(sf))
                      /Z32_ENTRYBYTES)) == (B*)-1)
          return VM_OVF;
      moveframe(VALUE_PTR(f)-FRAMEBYTES, f);

      for (ldict = (B*)Z32_DICT_ENTRIES(sf);
           ldict < (B*)Z32_DICT_FREE(sf);
           ldict += Z32_ENTRYBYTES) {
        Z32_pullname(Z32_ASSOC_NAME(ldict), namestring);
        makename(namestring, xf);
        if ((retc = Z32_convert(Z32_ASSOC_FRAME(ldict), xf2)) != OK)
          return retc;
        if (! insert(xf, VALUE_PTR(f), xf2)) return DICT_OVF;
      }
      break;
      
      default:
          return CORR_OBJ;
  }
  return OK;
}

static clock_t endclock;
static L chunk_size;

DM_INLINE_STATIC void START_ALARM(void) {
		endclock = clock() + 180*CLOCKS_PER_SEC;
		timeout = FALSE;
}

#define MAX_CHUNK (32000)
//100mbit/s*1/8mbyte/mbit*1024byte/mbyte*5s*1/2minrate*/

DM_INLINE_STATIC L CHECK_ALARM(void) {
  int timeout_;
  alarm(0);
  
  timeout_ = timeout;
  timeout = FALSE;
  if (clock() > endclock || timeout_) return TIMER;
  checkabort();
  
  alarm(10);
  return OK;
}

DM_INLINE_STATIC void END_ALARM(void) {
  alarm(0);
  timeout = FALSE;
}

/*---------------------------------------------------- readboxfile
   dir filename | root

  - reads the contents of the file specified by the strings 'dir' and
    'filename' into VM
  - unfolds the tree of objects in the box
  - pushes root object of the tree on operand stack
*/

P op_readf32(void)
{
int fd;
P nb, atmost, npath, retc;
B *p;
B f[FRAMEBYTES];

if (o_2 < FLOORopds) return(OPDS_UNF);
if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
npath = ARRAY_SIZE(o_1) + ARRAY_SIZE(o_2) + 1;
if (FREEvm + npath > CEILvm) return(VM_OVF);
moveB((B *)VALUE_BASE(o_2), FREEvm, ARRAY_SIZE(o_2));
moveB((B *)VALUE_BASE(o_1), FREEvm + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
FREEvm[npath-1] = '\000';
atmost = CEILvm - FREEvm;
   
START_ALARM();
rb1:
  if ((retc = CHECK_ALARM()) != OK) return retc;
  fd = open(FREEvm, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    if ((errno == EINTR) || (errno == EAGAIN)) goto rb1; 
    else {retc = -errno; END_ALARM(); return retc;};
  }
  p = FREEvm; 

rb2:
 if ((retc = CHECK_ALARM()) != OK) return retc;
 chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost;
 nb = read(fd, p, chunk_size);
 if (nb == -1) {if ((errno == EAGAIN) || (errno == EINTR)) goto rb2;
 else {retc = -errno; END_ALARM(); return retc;};}
 if (nb == 0) goto rb3;
 p += nb; atmost -= nb;
 if (atmost == 0) {END_ALARM(); return(VM_OVF);};
 goto rb2;
 
rb3:
 if ((retc = CHECK_ALARM()) != OK) return retc;
 if (close(fd) == -1) {if ((errno == EINTR) || (errno == EAGAIN)) goto rb3;
   else return(-errno);}
 END_ALARM();
 
 nb = DALIGN(p - FREEvm);
 if (! Z32_GETNATIVEFORMAT(FREEvm) || ! Z32_GETNATIVEUNDEF(FREEvm)) return BAD_FMT;
 if (! Z32_GETNATIVEENDIAN(FREEvm) && ((retc = Z32_deendian_frame(FREEvm)) != OK))
     return retc;
 if ((retc = Z32_unfoldobj(FREEvm,(L)FREEvm, Z32_GETNATIVE(FREEvm))) != OK) 
   return(retc);

 FORMAT(FREEvm) = 0;
 p = FREEvm;
 if (nb < FRAMEBYTES) nb = FRAMEBYTES;
 FREEvm += nb;

 if ((retc = Z32_convert(p, f)) != OK) {
   FREEvm = p;
   return retc;
 }
 TAG(p) = ARRAY | BYTETYPE;
 ATTR(p) = 0;
 VALUE_PTR(p) = p + FRAMEBYTES;
 ARRAY_SIZE(p) = nb - FRAMEBYTES;

 moveframe(f, o_2);
 FREEopds = o_1;
 return OK;
}
#endif //DM_HOST_IS_32_BIT
