/*=================== D machine Rev3.0 =============================

     -  dictionary functions: - make empty dictionary
                              - make dictionary of operators
     -  name object assembly, disassembly, and comparison
     -  fast moves for frames/blocks
     -  dictionary lookup and insertion
     -  mill

*/

#include "dm.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern BOOLEAN abortflag;

static char sys_hi[] = "System Operators V3.1";
L op_hi(void)   {return wrap_hi(sys_hi);}
L op_libnum(void) {return wrap_libnum(0);}

L wrap_libnum(UL libnum)
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = (NUM | LONGTYPE);
    ATTR(o1) = 0;
    LONG_VAL(o1) = libnum >> 16;

    FREEopds = o2;
    return OK;
}

L wrap_hi(B* hival)
{
    L hilen;
    L index;

    if (FLOORopds > o_2) return OPDS_UNF;
    if (! VALUE(o_1, &index)) return UNDF_VAL;
    hilen = strlen(hival);
    if (index + hilen > ARRAY_SIZE(o_2)) return RNG_CHK;

    strncpy(VALUE_PTR(o_2) + index, hival, hilen);
    LONG_VAL(o_1) = index + hilen;
    TAG(o_1) = (NUM | LONGTYPE);
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
B* geterror(L e)
{
    B* frame = NULL;
    L type = e >> 16;
    UL i;
    L* errc;
    B** errm;
    B* m = unknown;

    e &= 0x0000FFFFl;
    do {
        if (! (frame = nextlib(frame))) return "Lib VM corrupted";
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

B *makedict(L n)
{
static W lastprime = 9973;
static W primes[] = 
    { 7,13,17,19,23,31,41,53,61,79,
      101,127,157,199,251,317,397,503,631,797,
      997,1259,1579,2003,2503,3163,3989,4999,6311,7937,
      9973,
    };
L k,nb,ne, *hashtable; W hcon; B *dict,*mframe;

k = 0;
if ((n+n) >= lastprime) hcon = lastprime;
   else while ((hcon = primes[k]) < (n+n)) k++;
ne = n * ENTRYBYTES;
nb = (L)(DALIGN(DICTBYTES + ne + (((L)hcon)<<2)));
if ((FREEvm + nb + FRAMEBYTES) > CEILvm) return((B *)(-1L));
dict = FREEvm + FRAMEBYTES;
mframe = FREEvm;  FREEvm += nb + FRAMEBYTES;

DICT_ENTRIES(dict) = DICT_FREE(dict) = (L)dict + DICTBYTES;
hashtable = (L *)(DICT_CEIL(dict) =  (L)(DICT_ENTRIES(dict) + ne));
DICT_CONHASH(dict) = hcon;
for (k=0; k<hcon; k++) hashtable[k] = -1L;
TAG(mframe) = DICT; ATTR(mframe) = 0;
VALUE_BASE(mframe) = (L)dict; DICT_NB(mframe) = nb;
return(dict);
}

/*----------------------------------- clear associations of dictionary */

void cleardict(B *dict)
{
L k;

DICT_FREE(dict) = DICT_ENTRIES(dict);
for (k=0; k<DICT_CONHASH(dict); k++) ((L *)DICT_CEIL(dict))[k] = -1L;
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

B *makeopdict(B *opdefs, L *errc, B **errm) {
  return makeopdictbase(opdefs, errc, errm, 0);
}

B *makeopdictbase(B *opdefs, L *errc, B **errm, L n1)
{
L n;
B *opdef, *dict, *frame, framebuf[FRAMEBYTES], nameframe[FRAMEBYTES],
    *oldFREEvm, *newdict;

 oldFREEvm = FREEvm;
 n = 0; opdef = opdefs;
 while ( OPDEF_CODE(opdef) != 0) { n++; opdef += OPDEFBYTES; };
 if (! n1) n1 = n;

 if ((dict = makedict(n1))== (B *)(-1L)) return((B *)(-1L));
 if (FREEvm + LIBBYTES > CEILvm) return((B*) -1L);
 frame = framebuf;
 TAG(frame) = OP; ATTR(frame) = ACTIVE | READONLY;
 for (opdef = opdefs; n; opdef += OPDEFBYTES, n--)
 {
   OP_NAME(frame) = OPDEF_NAME(opdef);
   OP_CODE(frame) = OPDEF_CODE(opdef);
   makename(((B *)OP_NAME(frame)),nameframe);
   if (!insert(nameframe,dict,frame)) return((B *)(-1L));
 }
 FREEvm = oldFREEvm; 
 CEILvm -= FRAMEBYTES + LIBBYTES + DICT_NB(oldFREEvm);
 moveD((D*) oldFREEvm, (D*) CEILvm, (DICT_NB(oldFREEvm) + FRAMEBYTES)>>3);
 newdict = CEILvm + FRAMEBYTES;
 d_reloc(newdict, (L) dict, (L) newdict);
 VALUE_BASE(CEILvm) = (L) newdict;
 TAG(CEILvm) |= OPLIBTYPE; ATTR(CEILvm) = READONLY;
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

void d_reloc(B *dict, L oldl, L newl)
{
L offs, *link, k; B *entry;

offs = newl - oldl;
DICT_ENTRIES(dict) += offs;
DICT_FREE(dict) += offs;
DICT_CEIL(dict) += offs;
 for (k = 0, link = (L *)DICT_TABHASH(dict);
      k < DICT_CONHASH(dict); k++, link++)
  if ( *link != (-1L)) *link += offs;
for (entry = (B *)DICT_ENTRIES(dict); entry < (B *)DICT_FREE(dict);
     entry += ENTRYBYTES)
    if (ASSOC_NEXT(entry) != (-1L)) ASSOC_NEXT(entry) += offs;
}

/*-- relocate dictionary from actual to virtual addresses:
 
   d_rreloc(dictbase,oldbase,newbase)

- changes all pointers in dict by newbase-oldbase
- uses pointers into dict items before relocating them!
*/

void d_rreloc(B *dict, L oldl, L newl)
{
L offs, *link, k; B *entry;

offs = newl - oldl;
for (k = 0, link = (L *)DICT_TABHASH(dict); k <  DICT_CONHASH(dict); k++, link++)
  if ( *link != (-1L)) *link += offs;
for (entry = (B *)DICT_ENTRIES(dict); entry < (B *)DICT_FREE(dict);
     entry += ENTRYBYTES)
    if (ASSOC_NEXT(entry) != (-1L)) ASSOC_NEXT(entry) += offs;
DICT_ENTRIES(dict) += offs;
DICT_FREE(dict) += offs;
DICT_CEIL(dict) += offs;
}

/* ======================== name object functions =====================

 The internal makeup of the name object is hidden inside functions
 because of its akwardness:

   cccc0000aaaaaaaa 0011111122222233
   3333444444555555 6666667777778888
   88999999AAAAAABB BBBBCCCCCCDDDDDD (kkkkkkkkkkkkkkkk)

 c:    class specifier
 a:    attribute
 0..D: name string characters, encoded in six bits
 k:    after tiling in the string, the last word is replaced by the
       name key, which is the exclusive-or checksum of the string fields
       of all preceding words
 
 Name string characters are 0..9, A..Z, a..z, and '_'. In addition
 the name ']' is encoded by a zero in all character positions. A
 zero in positions 1-D concludes the string.

  makename(->namestring, ->nameframe)
  pullname(->nameframe, ->namestring)
  BOOLEAN matchname(->nameframe1, ->nameframe2)

 Name strings need to be able to hold 15 bytes.
*/

static B tosix[] = {
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     1,2,3,4,5,6,7,8,9,10,0,0,0,0,0,0,
     0,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,
     26,27,28,29,30,31,32,33,34,35,36,0,0,0,0,37,
     0,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,
     53,54,55,56,57,58,59,60,61,62,63,0,0,0,0,0
   };

static B fromsix[] = 
   "\0000123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz";

static B sb[16] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

void makename(B *namestring, B *nameframe)
{
W i; B c;
/* clear string buffer, copy string converting it to six-bit code,
   truncate after 14 bytes
*/
   i = 0;
   while ((c = namestring[i])) { sb[i] = tosix[c]; if (++i >= 14) break; }
   for (;i<14;i++) sb[i] = 0;

/* tile nameframe longwords with 6-bit characters */
     *(L *)nameframe = ((((((((0x00000030L | (sb[0]>>2))<<10)
              | (sb[0] & 0x3))<<6) | sb[1])<<6) | sb[2])<<2) | (sb[3]>>4);
     *(L *)(nameframe+4) = (((((((((0x0000000FL & sb[3])<<6)
              | sb[4])<<6) | sb[5])<<6) | sb[6])<<6 | sb[7])<<4) | (sb[8]>>2);
     *(L *)(nameframe+8) = ((((((((((0x00000003L & sb[8])<<6)
              | sb[9])<<6) | sb[10])<<6) | sb[11])<<6) | sb[12])<<6)
              | sb[13]; 

/* key = exclusive or of all nameframe words except CLASS/ATTR */
     *(W *)(nameframe+10) ^=  *(W *)(nameframe+8) ^ 
                              *(W *)(nameframe+6) ^
                              *(W *)(nameframe+4) ^
                              *(W *)(nameframe+2) ^
                             (*(W *)nameframe & 0xF00);
     fprintf(stderr, "Name: %s, %lx:%lx:%hx\n", 
	     namestring,
	     *(UL*) nameframe, 
	     *(UL*)(nameframe+4),
	     *(UW*) (nameframe+8));
} 

void pullname(B *nameframe, B *namestring)
{
L lw; W w;
  
namestring[14] = '\000';
/* recover string under checksum, merge into top longword */
w =  *(W *)(nameframe+10) ^ *(W *)(nameframe+8) ^ 
       *(W *)(nameframe+6) ^  *(W *)(nameframe+4) ^
       *(W *)(nameframe+2) ^ (*(W *)nameframe & 0xF00);
lw = (*(L *)(nameframe+8) & 0xFFFF0000) | (((L)w) & 0x0000FFFF);

namestring[13] = fromsix[lw & 0x3F]; lw >>= 6;
namestring[12] = fromsix[lw & 0x3F]; lw >>= 6;
namestring[11] = fromsix[lw & 0x3F]; lw >>= 6;
namestring[10] = fromsix[lw & 0x3F]; lw >>= 6;
namestring[9]   = fromsix[lw & 0x3F]; lw >>= 6;
namestring[8]  = lw & 0x3;
lw =   *(L *)(nameframe+4);
namestring[8] = fromsix[namestring[8] | ((lw & 0xF)<<2)]; lw >>= 4;
namestring[7]  = fromsix[lw & 0x3F]; lw >>= 6;
namestring[6]  = fromsix[lw & 0x3F]; lw >>= 6;
namestring[5]  = fromsix[lw & 0x3F]; lw >>= 6;
namestring[4]  = fromsix[lw & 0x3F]; lw >>= 6;
namestring[3]  = lw & 0xF;
lw =   *(L *)nameframe;
namestring[3] = fromsix[namestring[3] | ((lw & 0x3)<<4)]; lw >>= 2;
namestring[2]  = fromsix[lw & 0x3F]; lw >>= 6;
namestring[1]  = fromsix[lw & 0x3F]; lw >>= 6;
namestring[0] = fromsix[(lw & 0x3) | (((lw>>10) & 0xF)<<2)];
if (namestring[0] == 0) namestring[0] = ']';
}

BOOLEAN matchname(B *nameframe1, B *nameframe2)
{  
/*  this expects that the namekeys already have been matched */
return ( (*(L *)(nameframe1+2) == *(L *)(nameframe2+2)) &&
         (*(L *)(nameframe1+6) == *(L *)(nameframe2+6)) &&
         (((*(W *)nameframe1 ^ *(W *)nameframe2) & 0x0F00) == 0)
       );
}

/* ======================== move frame(s) =============================
  NOTA BENE: this implies FRAMEBYTES = 16 for sake of speed!
*/

void moveframe(B *source, B *dest)
{
D *s,*d;

s = (D *)source; d = (D *)dest;
*(d++) = *(s++); *(d++) = *(s++);

}

void moveframes(B *source, B *dest, L n)
{
D *s,*d;

s = (D *)source; d = (D *)dest;
for (; n>0; n--) { *(d++) = *(s++); *(d++) = *(s++); }
}

/* ========================== move block ==============================

These move blocks of different data sizes among aligned locations     */

void moveB(B *source, B *dest, L n)
{ 
for (; n>0; n--) *(dest++) = *(source++);
}

void moveW(W *source, W *dest, L n)
{ 
for (; n>0; n--) *(dest++) = *(source++);
}

void moveL(L *source, L *dest, L n)
{ 
for (; n>0; n--) *(dest++) = *(source++);
}

void moveS(S *source, S *dest, L n)
{ 
for (; n>0; n--) *(dest++) = *(source++);
}

void moveD(D *source, D *dest, L n)
{ 
for (; n>0; n--) *(dest++) = *(source++);
}

/* ===================== dictionary services ======================== */

/*--------------------------- lookup --------------------------------
returns pointer to associated frame in dictionary, or 0L if the name is
not represented.
*/

B *lookup(B *nameframe, B *dict)
{

B  *link; W h, key; L *hashtable;

key = NAME_KEY(nameframe);
h = DICT_CONHASH(dict); h = (key & 0x7FFF) % h;
hashtable = (L *)(DICT_TABHASH(dict));
if ( (link = (B *)(hashtable[h])) == (B *)(-1L)) return((B *)0L);
do { if (key == NAME_KEY(ASSOC_NAME(link)))
        if (matchname(nameframe,ASSOC_NAME(link))) 
            return(ASSOC_FRAME(link));
   } while ( (link = (B *)(ASSOC_NEXT(link))) != (B *)(-1L));
return(0L);
}

/* merge entries in source into sink */
/* return false if not enough space in sink */
BOOLEAN mergedict(B *source, B* sink) {
  B *entry;
  B hiname[FRAMEBYTES];
  B libnumname[FRAMEBYTES];
  makename("hi", hiname);
  makename("libnum", libnumname);

  for (entry = (B*) DICT_ENTRIES(source);
       entry < (B*) DICT_FREE(source);
       entry += ENTRYBYTES) {
    if (! matchname(ASSOC_NAME(entry), hiname)
	&& ! matchname(ASSOC_NAME(entry), libnumname)
	&& ! insert(ASSOC_NAME(entry), sink, ASSOC_FRAME(entry)))
      return FALSE;
  }
  return TRUE;
}

/*------------------------- insertion ----------------------------------
returns TRUE if insertion was done, or FALSE on failure.
*/

BOOLEAN insert(B *nameframe, B *dict, B *framedef)
{
L *hashtable; W h, key; B *link, *newlink;

key = NAME_KEY(nameframe);
h = DICT_CONHASH(dict); h = (key & 0x7FFF) % h;
hashtable = (L *)(DICT_TABHASH(dict));

if ((newlink = (B *)(hashtable[h])) == (B *)(-1L))    /* prime a chain */
   { if (DICT_FREE(dict) >= DICT_CEIL(dict)) return(FALSE);
     link = (B *)( hashtable[h] = DICT_FREE(dict) );
     goto ins_name;
   }
do { link = newlink;
     if (key == NAME_KEY(ASSOC_NAME(link)))
        if ( matchname(nameframe,ASSOC_NAME(link))) goto ins_fra;
   } while ( (newlink = (B *)(ASSOC_NEXT(link))) != (B *)(-1L));

/* reserve entry for new name; link and fill it */

if (DICT_FREE(dict) >= DICT_CEIL(dict)) return(FALSE);
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

L exec(L turns)
{
static B fetch_err[] = "fetch phase\n";
static B transl_err[] = "translation phase\n";
static B exec_err[] = "execution phase\n";
static B undfn_buf[15];
 B *f, *af, *dict; L  retc; UB fclass;
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
 if ((CLASS(o1)== NAME) && ((ATTR(o1) & TILDE) != 0)) ATTR(o1) = ACTIVE;
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

/*----------------------------- foldobj

  - receives the frame of an internal composite object and a box base
    address (of master frame of first box value object)
  - copies the composite object value into VM freespace
  - scans all objects belonging to the copied composite object value:
      - leaves simple or external objects as unchanged
      - replaces operator objects by their name and earmarks them
      - applies itself recursively to an internal list or dict object
      - replaces internal box objects referenced in dictionaries or
        contained in lists by null objects
  - returns a frame whose internal addresses are relative to base (side
    effect on VM freespace: objects belonging to lower nodes of tree)

NB: a dict is relocated in 2 steps: 1 - to the new physical mem loc
                                    2 - to the box base
*/
#define MAXDEPTH 50  /* counts depth of object nesting (<= 20) */

L foldobj(B *frame, L base, W *depth)
{
B *tframe, *tvalue, *value, *lframe, *entry;
L k, retc, *link, nb, offset;

if ((++(*depth)) > MAXDEPTH) return(RNG_CHK);

switch(CLASS(frame))
  {
  case ARRAY: tframe = FREEvm; tvalue = tframe + FRAMEBYTES;
              nb = (L)(DALIGN(ARRAY_SIZE(frame) * VALUEBYTES(TYPE(frame))));
              if ((FREEvm+nb+FRAMEBYTES) > CEILvm) return(VM_OVF);
              FREEvm += nb + FRAMEBYTES;
              value = (B*)VALUE_BASE(frame);
              VALUE_BASE(frame) = (L)tvalue - base;
              moveframes(frame,tframe,1L); moveL((L *)value,(L *)tvalue,nb>>2);
              break;
  case LIST:  tframe = FREEvm; tvalue = tframe + FRAMEBYTES;
              nb = LIST_CEIL(frame) - VALUE_BASE(frame);
              if ((FREEvm+nb+FRAMEBYTES) > CEILvm) return(VM_OVF);
              FREEvm += nb + FRAMEBYTES;
              value = (B *)VALUE_BASE(frame);
              VALUE_BASE(frame) = (L)tvalue - base;
              LIST_CEIL(frame) = VALUE_BASE(frame) + nb;
              moveframes(frame,tframe,1L); moveL((L *)value,(L *)tvalue,nb>>2);
              for (lframe = tvalue; lframe < (tvalue + nb); 
                   lframe += FRAMEBYTES)
                 { if (CLASS(lframe) == OP) 
                      { makename((B *)OP_NAME(lframe),lframe);
                        ATTR(lframe) |= (BIND | ACTIVE); continue;
                      }
                   if (CLASS(lframe) == BOX)
                      { TAG(lframe) = NULLOBJ; ATTR(lframe) = 0;
                        continue;
                      }
                   if (!COMPOSITE(lframe)) continue;
                   if ((retc = foldobj(lframe,base,depth)) != OK) return(retc);
                 }
              break;
  case DICT:  tframe = FREEvm; tvalue = tframe + FRAMEBYTES;
              nb = DICT_NB(frame); 
              if ((FREEvm+nb+FRAMEBYTES) > CEILvm) return(VM_OVF);
              FREEvm += nb + FRAMEBYTES;
              value = (B *)VALUE_BASE(frame);
              VALUE_BASE(frame) = (L)tvalue - base;
              moveframes(frame,tframe,1L); moveL((L *)value,(L *)tvalue,nb>>2);
              offset = ((L)tvalue) - ((L)value);
              DICT_ENTRIES(tvalue) += offset; DICT_FREE(tvalue) += offset;
              DICT_CEIL(tvalue) += offset;
              offset -= base;
              for (k = 0, link = (L *)DICT_TABHASH(tvalue);
                   k < DICT_CONHASH(tvalue); k++, link++)
                 { if (*link != (-1L)) *link += offset; }
              for (entry = (B *)DICT_ENTRIES(tvalue);
                   entry < (B *)DICT_FREE(tvalue); entry += ENTRYBYTES)
                 { if (ASSOC_NEXT(entry) != (-1L))
                       ASSOC_NEXT(entry) += offset;
                   lframe = ASSOC_FRAME(entry);
                   if (CLASS(lframe) == OP) 
                      { makename((B *)OP_NAME(lframe),lframe);
                        ATTR(lframe) |= (BIND | ACTIVE); continue;
                      }
                   if (CLASS(lframe) == BOX)
                      { TAG(lframe) = NULLOBJ; ATTR(lframe) = 0;
                        continue;
                      }
                   if (!COMPOSITE(lframe)) continue;
                   if ((retc = foldobj(lframe,base,depth)) != OK) return(retc);
                 }
              DICT_ENTRIES(tvalue) -= base; DICT_FREE(tvalue) -= base;
              DICT_CEIL(tvalue) -= base;
              break;
  default: return(CORR_OBJ);
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
   

L unfoldobj(B *frame, L base)

{
B *lframe, *dict, *entry, *dframe, *xframe, *ldict;
L retc, k, *link;

switch(CLASS(frame))
  {
  case ARRAY: VALUE_BASE(frame) += base; break;
  case LIST: VALUE_BASE(frame) += base; LIST_CEIL(frame) += base;
             for (lframe = (B *)VALUE_BASE(frame);
                  lframe < (B *)LIST_CEIL(frame); lframe += FRAMEBYTES)
              { if (ATTR(lframe) & BIND)
                   { dframe = FREEdicts - FRAMEBYTES; xframe = 0L;
                     while ((dframe >= FLOORdicts) && (xframe == 0L))
                       { ldict = (B *)VALUE_BASE(dframe);
                         xframe = lookup(frame,ldict);
                         dframe -= FRAMEBYTES;
                       }
                     if ((L)xframe > 0) 
                        { moveframes(xframe,lframe,1L); }
                       else
                        { ATTR(lframe) &= (~BIND); }
                     continue;
                   }   
                if (!COMPOSITE(lframe)) continue;
                if ((retc = unfoldobj(lframe,base)) != OK) return(retc);
              }
             break;
  case DICT: VALUE_BASE(frame) += base; dict = (B *)VALUE_BASE(frame);
             DICT_ENTRIES(dict) += base; DICT_FREE(dict) += base;
             DICT_CEIL(dict) += base;
             for (k = 0, link = (L *)DICT_TABHASH(dict); 
                  k < DICT_CONHASH(dict); k++, link++)
                { if (*link != (-1L)) *link += base; }
             for (entry = (B *)DICT_ENTRIES(dict); entry < (B *)DICT_FREE(dict);
                  entry += ENTRYBYTES)
               {
               if (ASSOC_NEXT(entry) != (-1L)) ASSOC_NEXT(entry) += base;
               lframe = ASSOC_FRAME(entry);
               if (ATTR(lframe) & BIND)
                   { dframe = FREEdicts - FRAMEBYTES; xframe = 0L;
                     while ((dframe >= FLOORdicts) && (xframe == 0L))
                       { ldict = (B *)VALUE_BASE(dframe);
                         xframe = lookup(frame,ldict);
                         dframe -= FRAMEBYTES;
                       }
                     if ((L)xframe > 0) 
                        { moveframes(xframe,lframe,1L); }
                       else
                        { ATTR(lframe) &= (~BIND); }
                     continue;
                   }   
               if (!COMPOSITE(lframe)) continue;
               if ((retc = unfoldobj(lframe,base)) != OK) return(retc);
               }
             break;
  default: return(CORR_OBJ);
  }
  moveframes(frame,(B *)VALUE_BASE(frame)-FRAMEBYTES,1L);
return(OK);
}
