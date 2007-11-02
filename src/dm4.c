/*===================== D machine Rev3.0 (dm4.c) ========================

     - operators:
          _ null
          - ]
          - pop
          - exch
          - dup
          - index
          - clear
          - count
          - roll
          - counttomark
          - cleartomark
          - dict
          - cleardict
          - array
          - list
          - begin
          - end
          - def
          - name
          - find
          - get
          - put
          - known
          - getinterval
          - countdictstack
          - dictstack
          - currentdict
*/

#include "dm.h"
#include <math.h>

/*------------------------------------- null
        --- | null
*/

L op_null(void)
{
if (o1 >= CEILopds) return(OPDS_OVF);
TAG(o1) = NULLOBJ; ATTR(o1) = 0;
FREEopds =o2;
return(OK);
}

/*------------------------------------- ]
searches for most recent mark on operand stack, moves
objects down to mark as list value into VM, and replaces
mark object by list object; if the [ operator carried the TILDE
attribute, the list pn the operand stack is made active;
*/

L op_closelist(void)
{
B *frame, *mframe;
L nframes, nb;

frame = o_1; nframes = 0;
while (CLASS(frame) != MARK)
     { frame -= FRAMEBYTES; nframes++;
       if (frame < FLOORopds) return(OPDS_UNF); }
nb = FREEopds - frame - FRAMEBYTES;
if ((FREEvm + nb + FRAMEBYTES) > CEILvm)  return(VM_OVF);
mframe = FREEvm; FREEvm += FRAMEBYTES;
TAG(mframe) = LIST;
ATTR(mframe) = PARENT | ((ATTR(frame) & TILDE)? ACTIVE : 0);
VALUE_BASE(mframe) = (L)FREEvm;
moveframes(frame + FRAMEBYTES, FREEvm, nframes);
LIST_CEIL(mframe) = (L)(FREEvm += nb);
moveframes(mframe,frame,1L);
FREEopds = frame + FRAMEBYTES;
return(OK);
}

/*------------------------------------- pop
removes top element from operand stack 
*/

L op_pop(void)
{
if (FREEopds <= FLOORopds) return(OPDS_UNF);
FREEopds -= FRAMEBYTES;
return(OK);
}

/*------------------------------------- exch
  any1 any2 | any2 any1

exchanges top two elements of operand stack
*/

L op_exch(void)
{
B framebuf[FRAMEBYTES];

if (o_2 < FLOORopds) return(OPDS_UNF);
moveframes(o_1, framebuf, 1L);
moveframes(o_2, o_1, 1L);
moveframes(framebuf, o_2, 1L);
return(OK);
}

/*------------------------------------- dup
duplicates top element of operand stack
*/

L op_dup(void)
{

if (FREEopds <= FLOORopds) return(OPDS_UNF);
if (FREEopds >= CEILopds) return(OPDS_OVF);
moveframes(o_1,o1, 1L);
FREEopds = o2;
return(OK);
}

/*------------------------------------- index
  anyn...any0 n index anyn...any0 anyn

duplicates n-th element counted from top of operand stack.
*/

L op_index(void)
{
L n,nb;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != NUM) return(OPD_CLA);
if (!VALUE(o_1,&n)) return(UNDF_VAL);
nb = (n + 1) * FRAMEBYTES;
if (((o_1 - nb) < FLOORopds) || (n < 0)) return(RNG_CHK);
moveframes(o_1 - nb, o_1, 1L);
return(OK); 
}

/*------------------------------------- clear
clears entire operand stack
*/

L op_clear(void)
{
FREEopds = FLOORopds;
return(OK);
}

/*------------------------------------- count
returns count of elements on operand stack
*/

L op_count(void)
{
L nb;

if (o1 >= CEILopds) return(OPDS_OVF);
TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
nb = FREEopds - FLOORopds;
LONG_VAL(o1) = nb / FRAMEBYTES;
FREEopds = o2;
return(OK);
}

/*------------------------------------- roll
  anyn-1...any0 n j | ...

performs a circular shift of the n objects anyn-1...any0 on the
operand stack by the amount j. Positive j indicates upward motion
on the stack, whereas negative j indicates downward motion. Note
that |j|<n, n>0, and that the operand stack needs to have space
for another n-2 elements.
*/

L op_roll(void)
{
B *base;  L absj,j,n,ubytes,cbytes,nbytes,jbytes;

if (o_2 <= FLOORopds) return(OPDS_UNF);
if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return(OPD_CLA);
if (!(VALUE(o_2,&n) && VALUE(o_1,&j))) return(UNDF_VAL);
absj = (j<0)? -j : j;
if ((n <= 0) || (absj >= n)) return(RNG_CHK);
ubytes = o_2 - FLOORopds;    cbytes = CEILopds - FLOORopds;
nbytes = n * FRAMEBYTES; jbytes = absj * FRAMEBYTES;
if (nbytes > ubytes) return(RNG_CHK);
if ((ubytes + nbytes) > cbytes) return(OPDS_OVF);
base = o_2 - nbytes;
moveframes(base, base+nbytes, n);
if (j > 0)
     { moveframes(base+nbytes, base+jbytes, n-j);
       moveframes(base+nbytes+nbytes-jbytes, base, j); }
else if (j < 0)
     { moveframes(base+nbytes, base+nbytes-jbytes, absj);
       moveframes(base+nbytes+jbytes, base, n-absj); }
FREEopds = o_2;
return(OK);
}

/*------------------------------------- counttomark
counts operand stack elements down to most recent mark,
pushes count numeral onto operand stack
*/

L op_counttomark(void)
{
B *frame; L nframes;

frame = o_1; nframes = 0;
while (CLASS(frame) != MARK)
     { nframes++;
       if ((frame -= FRAMEBYTES) < FLOORopds) return(OPDS_UNF);
     }
if (o1 >= CEILopds) return(OPDS_OVF);
TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
LONG_VAL(o1) = nframes;
FREEopds = o2;
return(OK);
}

/*------------------------------------- cleartomark
deletes operand stack elements down to and including
most recent mark
*/

L op_cleartomark(void)
{
B *frame;

frame = FREEopds;
do { frame -= FRAMEBYTES;
     if (frame < FLOORopds) return(OPDS_UNF);
   } while (CLASS(frame) != MARK);
FREEopds = frame;
return(OK);
}

/*------------------------------------ dict
  n | dictionary

makes an empty internal dictionary of length n and returns the new
dictionary frame.
*/

L op_dict(void)
{
B *dict; L n;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != NUM) return(OPD_CLA);
if (!VALUE(o_1,&n)) return(UNDF_VAL);
if (n <= 0) return(RNG_CHK);
if ((dict = makedict(n)) == (B *)(-1L)) return(VM_OVF);
moveframes(dict - FRAMEBYTES,o_1,1L);
return(OK);
}

/*------------------------------------ cleardict
   dict | dict
   
removes all definitions from a dictionary
*/

L op_cleardict(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != DICT) return(OPD_CLA);
if (ATTR(o_1) & READONLY) return(OPD_ATR);
cleardict((B *)VALUE_BASE(o_1));
return(OK);
}

/*-----------------------------------  matrix
 * rows columns | matrix
 */
L op_matrix(void)
{
		L m, n;
		B* mframe;
		B* arrayframe;
		B* listframe;
		B* list;
		B* c;
		
		if (o_2 < FLOORopds) return OPDS_UNF;
		if (CLASS(o_2) != NUM) return OPD_CLA;
		if (CLASS(o_1) != NUM) return OPD_CLA;
		if (! VALUE(o_2, &m)) return UNDF_VAL;
		if (! VALUE(o_1, &n)) return UNDF_VAL;
		
		mframe = FREEvm;
		TAG(mframe) = MATRIX; ATTR(mframe) = PARENT;
		arrayframe = MATRIX_ARRAY(mframe) = mframe+FRAMEBYTES;
		if (arrayframe >= CEILvm) return VM_OVF;
		TAG(arrayframe) = ARRAY | DOUBLETYPE; ATTR(arrayframe) = 0;
		VALUE_PTR(arrayframe) = arrayframe + FRAMEBYTES;
		ARRAY_SIZE(arrayframe) = m*n;

		if ((listframe = MATRIX_LIST(mframe)
				 = (B*) DALIGN(arrayframe + m*n*sizeof(D)))
				>= CEILvm) return VM_OVF;
		if ((B*) (DALIGN(LIST_CEIL_PTR(listframe)
								= listframe + FRAMEBYTES + m*FRAMEBYTES))
				>= CEILvm) return VM_OVF;
		
		TAG(listframe) = LIST; ATTR(listframe) = READONLY;
		VALUE_PTR(listframe) = listframe + FRAMEBYTES;
		
		for (list = VALUE_PTR(listframe), c = VALUE_PTR(arrayframe);
				 list < LIST_CEIL_PTR(listframe);
				 list += FRAMEBYTES, c += n*sizeof(D)) {
				TAG(list) = ARRAY | DOUBLETYPE; ATTR(list) = 0;
				VALUE_PTR(list) = c;
				ARRAY_SIZE(list) = n;
		}

		FREEvm = MATRIX_END(mframe) = (B*) DALIGN(list);
		
		moveframe(mframe, o_2); 
		FREEopds = o_1;
		return OK;
}

/*---------------------------------------- matrix_dim
 * matrix | m n
 */
L op_matrix_dim(void) 
{
		L m, n;
		B* listframe;
		if (o_1 < FLOORopds) return OPDS_UNF;
		if (o2 > CEILopds) return OPDS_OVF;
		if (CLASS(o_1) != MATRIX) return OPD_CLA;

		listframe = MATRIX_LIST(o_1);
		m = (LIST_CEIL(listframe) - VALUE_BASE(listframe))/FRAMEBYTES;
		n = m ? ARRAY_SIZE(VALUE_PTR(listframe)) : 0;

		TAG(o_1) = NUM | LONGTYPE; ATTR(o_1) = 0;
		LONG_VAL(o_1) = m;
		TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
		LONG_VAL(o1) = n;

		FREEopds = o2;
		return OK;
}

/*---------------------------------------- matrix_list
 * matrix | list-form
 */
L op_matrix_list(void) 
{
		if (o_1 < FLOORopds) return OPDS_UNF;
		if (CLASS(o_1) != MATRIX) return OPD_CLA;

		moveframe(MATRIX_LIST(o_1), o_1);
		return OK;
}

/*---------------------------------------- matrix_array
 * matrix | array-form
 */
L op_matrix_array(void) 
{
		if (o_1 < FLOORopds) return OPDS_UNF;
		if (CLASS(o_1) != MATRIX) return OPD_CLA;

		moveframe(MATRIX_ARRAY(o_1), o_1);
		return OK;
}


/*------------------------------------ array
  n /type_name | array

makes an array to hold an array of n numerals of specifed type (one
of b, w, l, s, d, x, p); returns new internal object.
*/

L op_array(void)
{
B *mframe, type[NAMEBYTES+1];  W t;  L n,nb;

if (o_2 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_2) != NUM) return(OPD_CLA);
if (!VALUE(o_2,&n)) return(UNDF_VAL);
if (n < 0) return(RNG_CHK);
if (CLASS(o_1) != NAME) return(OPD_ERR);
pullname(o_1,type); t = ascii[(*type) & 0x7F];
if ((t & 0x4030) != 0x4030) return(RNG_CHK); t &= 0x0F;
nb = DALIGN(n * VALUEBYTES(t)); 
if ((FREEvm + nb + FRAMEBYTES) > CEILvm) return(VM_OVF);
mframe = FREEvm; FREEvm += FRAMEBYTES;
TAG(mframe) = ARRAY | t; ATTR(mframe) = PARENT;
VALUE_BASE(mframe) = (L)FREEvm; ARRAY_SIZE(mframe) = n;
FREEvm += nb;
moveframes(mframe,o_2,1L);
ARRAY_SIZE(o_2) = n;
FREEopds = o_1;
return(OK);
}

/*------------------------------------ list
  n | list

makes a list to hold n objects; returns new internal list object.
*/

L op_list(void)
{
B *nullframe, *mframe;  L n, nb;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != NUM) return(OPD_CLA);
if (!VALUE(o_1,&n)) return(UNDF_VAL); if (n < 0) return(RNG_CHK);
nb = n * FRAMEBYTES;
if ((FREEvm + nb + FRAMEBYTES) > CEILvm) return(VM_OVF);
mframe = FREEvm; FREEvm += FRAMEBYTES;
TAG(mframe) = LIST; ATTR(mframe) = PARENT;
VALUE_BASE(mframe) = (L)FREEvm;
LIST_CEIL(mframe) = (L)(FREEvm += nb);
nullframe = (B *)VALUE_BASE(mframe);
while (nullframe < (B *)LIST_CEIL(mframe))
   { TAG(nullframe) = NULLOBJ; ATTR(nullframe) = 0; 
     nullframe += FRAMEBYTES; }
moveframes(mframe,o_1,1L);
return(OK);
}
 
/*------------------------------------ begin
  dict | ---
 
pushes a dictionary onto the dictionary stack, making it the current
directory where name searches start.
*/

L op_begin(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != DICT) return(OPD_CLA);
if (FREEdicts >= CEILdicts) return(DICTS_OVF);
moveframes(o_1, FREEdicts, 1L);
FREEdicts += FRAMEBYTES;
FREEopds = o_1;
return(OK);
}

/*------------------------------------ end
pops the current dictionary from the dictionary stack; will fail
when attempting to pop one of the two basic dictionaries (system
and user).
*/

L op_end(void)
{
if ((FREEdicts - FRAMEBYTES - FRAMEBYTES) <= FLOORdicts)
    return(DICTS_UNF);
FREEdicts -= FRAMEBYTES;
return(OK);
}

/*------------------------------------- def
  /name value | ---

 -   enters the name-value association into the topmost writable
     dictionary present on the dictionary stack
 -   on overflow of the dictionary, this dictionary object on the
     dict stack is made readonly and a DICT_OVF error is executed
*/

L op_def(void)
{
B *dframe, *dict;

if (o_2 < FLOORopds) return(OPDS_UNF);
dframe = FREEdicts;
do { dframe -= FRAMEBYTES;
     if (dframe < FLOORdicts) return(DICT_ATR);
     dict = (B *)VALUE_BASE(dframe);
   } while (ATTR(dframe) & READONLY);
if (CLASS(o_2) != NAME) return(OPD_CLA);
if (ATTR(o_2) & ACTIVE) return(OPD_ATR);
if (!insert(o_2,dict,o_1)) 
 { ATTR(dframe) |= READONLY; return(DICT_OVF); }
FREEopds = o_2;       
return(OK);
}

/*------------------------------------- name
  value /name | ---

 - analogous to 'def', but uses reverse order of operands
*/

L op_name(void)
{
B *dframe, *dict;

if (o_2 < FLOORopds) return(OPDS_UNF);
dframe = FREEdicts;
do { dframe -= FRAMEBYTES;
     if (dframe < FLOORdicts) return(DICT_ATR);
     dict = (B *)VALUE_BASE(dframe);
   } while (ATTR(dframe) & READONLY);
if (CLASS(o_1) != NAME) return(OPD_CLA);
if (ATTR(o_1) & ACTIVE) return(OPD_ATR);
if (!insert(o_1,dict,o_2)) 
   {  ATTR(dframe) |= READONLY; return(DICT_OVF); }
FREEopds = o_2;       
return(OK);
}

/*------------------------------------- find
   /name | value

searches the active dictionaries from the top of the dictionary stack
down for the name and retrieves the associated object.
*/

L op_find(void)
{
B *dframe, *dict, *aframe;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != NAME) return(OPD_CLA);
if (ATTR(o_1) & ACTIVE) return(OPD_ATR);
dframe = FREEdicts - FRAMEBYTES; aframe = 0L;
while ((dframe >= FLOORdicts) && (aframe == 0L))
   { dict = (B *)VALUE_BASE(dframe);
     aframe = lookup(o_1,dict);
     dframe -= FRAMEBYTES;
   }
if (aframe == 0) return(UNDF);
moveframes(aframe, o_1, 1L);
return(OK);
}

/*------------------------------------- get
       array index | numeral
        list index | any
  dictionary /name | any

gets the array/list element at the given index (0...), or the
object associated with a name/string in a dictionary.
*/

L op_get(void)
{
B *dict, *src;  L n;

if (o_2 < FLOORopds) return(OPDS_UNF);
switch(CLASS(o_2))
   {
   case ARRAY: if (CLASS(o_1) != NUM) return(OPD_CLA);
               if (!VALUE(o_1,&n)) return(UNDF_VAL);
               if ( (n < 0) || (n >= ARRAY_SIZE(o_2))) return(RNG_CHK);
               VALUE_BASE(o_2) += n * VALUEBYTES(TYPE(o_2));
               ARRAY_SIZE(o_2) = 1L;
               TAG(o_1) = NUM | TYPE(o_2); ATTR(o_1) = 0;
               MOVE(o_2,o_1); moveframe(o_1,o_2);
               break;

   case LIST: if (CLASS(o_1) != NUM) return(OPD_ERR);
                 if (!VALUE(o_1,&n)) return(UNDF_VAL);
              src = (B *)VALUE_BASE(o_2) + n * FRAMEBYTES;
              if ((src < (B *)VALUE_BASE(o_2)) || (src >= (B *)LIST_CEIL(o_2)))
                 return(RNG_CHK);
              moveframes(src, o_2, 1L);
              break;

   case DICT: dict = (B *)VALUE_BASE(o_2);
              if (CLASS(o_1) != NAME) return(OPD_CLA);
              if (ATTR(o_1) & ACTIVE) return(OPD_ATR);
              if ( (src = lookup(o_1,dict)) == 0L) return(UNDF);
              moveframes(src, o_2, 1L);
              break;       
   default:   return(OPD_CLA);
   }
FREEopds = o_1;
return(OK);
}

/*-------------------------------------- put
  numeral array index | ---
       any list index | ---
 any dictionary /name | ---

replaces the array element at index by the numeral, or the list
element at index by any object, or associates the name with any
object in a dictionary.
*/

L op_put(void)
{
B *dict, *dest;  L n;

if (o_3 < FLOORopds) return(OPDS_UNF);
if (ATTR(o_2) & READONLY) return(OPD_ATR);
switch(CLASS(o_2))
     {
     case ARRAY: if (CLASS(o_1) != NUM) return(OPD_CLA);
                 if (!VALUE(o_1,&n)) return(UNDF_VAL);
                 if ((n < 0) || (n >= ARRAY_SIZE(o_2))) return(RNG_CHK);
                 if (CLASS(o_3) != NUM) return(OPD_CLA);
                 VALUE_BASE(o_2) += n * VALUEBYTES(TYPE(o_2));
                 ARRAY_SIZE(o_2) = 1L;
                 MOVE(o_3, o_2);
                 break;

     case LIST: if (CLASS(o_1) != NUM) return(OPD_CLA);
                if (!VALUE(o_1,&n)) return(UNDF_VAL);
                dest = (B *)VALUE_BASE(o_2) + n * FRAMEBYTES;
                if ((dest < (B *)VALUE_BASE(o_2)) || 
                    (dest >= (B *)LIST_CEIL(o_2))) return(RNG_CHK);
                moveframes(o_3, dest, 1L);
                break;

     case DICT: dict = (B *)VALUE_BASE(o_2);
                if (CLASS(o_1) != NAME) return(OPD_CLA);
                if (ATTR(o_1) & ACTIVE) return(OPD_ATR);
                if (!insert(o_1,dict,o_3))
                   { return(DICT_OVF); }
                break;

     default: return(OPD_CLA);
     }
FREEopds = o_3;
return(OK);
}

/*------------------------------------ known
  dictionary /name | boolean

returns 'true' if the name is defined in the dictionary, else
'false'. Internal dictionaries only.
*/

L op_known(void)
{
B *dict;

if (o_2 < FLOORopds) return(OPDS_UNF);
if ((CLASS(o_2) != DICT) || (CLASS(o_1) != NAME)) return(OPD_CLA);
if (ATTR(o_1) & ACTIVE) return(OPD_ATR);
dict = (B *)VALUE_BASE(o_2);
BOOL_VAL(o_2) = (lookup(o_1,dict) != 0L);
TAG(o_2) = BOOL; ATTR(o_2) = 0;
FREEopds = o_1;
return(OK);
}

/*-------------------------------------- getinterval
  array index count | subarray
   list index count | sublist

returns a subarray or sublist starting at index in the original
array/list and extending over count elements.
*/

L op_getinterval(void)
{
B *base, *ceil; L k,n;

if (o_3 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_2) != NUM) return(OPD_CLA);
if (CLASS(o_1) != NUM) return(OPD_CLA);
if (!(VALUE(o_2,&k) && VALUE(o_1,&n))) return(UNDF_VAL);
if ((k < 0) || (n < 0)) return(RNG_CHK); 
switch(CLASS(o_3))
     {
     case ARRAY: if ((k+n) > ARRAY_SIZE(o_3)) return(RNG_CHK);
                 VALUE_BASE(o_3) += k * VALUEBYTES(TYPE(o_3));
                 ARRAY_SIZE(o_3) = n;
                 break;

     case LIST: base = (B *)VALUE_BASE(o_3) + k * FRAMEBYTES;
                ceil = base + n * FRAMEBYTES;
                if (ceil > (B *)LIST_CEIL(o_3)) return(RNG_CHK);
                VALUE_BASE(o_3) = (L)base; LIST_CEIL(o_3) = (L)ceil;
                break;

     default: return(OPD_CLA);
     }
ATTR(o_3) &= (~PARENT);
FREEopds = o_2;
return(OK);
}
 
/*------------------------------------- countdictstack
    | int

returns number of elements on dictionary stack
*/

L op_countdictstack(void)
{

if (o1 >= CEILopds) return(OPDS_OVF);
TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
LONG_VAL(o1) = (FREEdicts - FLOORdicts) / FRAMEBYTES;
FREEopds = o2;
return(OK);
}

/*-------------------------------------- dictstack
  list | sublist

 - stores all elements of the dictionary stack into the list and
   returns the sublist of copied elements in the list
 - restores DICT_NB entry in stored dict frames
*/

L op_dictstack(void)
{
B *dframe; L nb;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != LIST) return(OPD_CLA);
nb = FREEdicts - FLOORdicts;
if (nb > (LIST_CEIL(o_1) - VALUE_BASE(o_1))) return(RNG_CHK);
moveframes(FLOORdicts, (B *)VALUE_BASE(o_1), nb / FRAMEBYTES);
LIST_CEIL(o_1) = VALUE_BASE(o_1) + nb; ATTR(o_1) &= (~PARENT);
for (dframe = (B *)VALUE_BASE(o_1); dframe < (B *)LIST_CEIL(o_1);
     dframe += FRAMEBYTES)
   { DICT_NB(dframe) = DICT_NB(VALUE_BASE(dframe) - FRAMEBYTES); }
 
return(OK);
}

/*------------------------------------- currentdict
    --- | dict

returns the current dictionary, from the top of the dictionary stack.
*/

L op_currentdict(void)
{
if (o1 >= CEILopds) return(OPDS_OVF);
moveframes(FREEdicts - FRAMEBYTES, o1, 1L);
FREEopds = o2;
return(OK);
}

