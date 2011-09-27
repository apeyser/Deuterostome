/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
/*===================== D machine Rev3.0 (dm4.c) ========================

     - operators:
          _ null
          - ]
          - pop
	  - push
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
#include "dm2.h"

/*------------------------------------- null
        --- | null
*/

P op_null(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = NULLOBJ; 
  ATTR(o1) = 0;
  FREEopds = o2;
  return OK;
}

/*------------------------------------- closelist/]
searches for most recent mark on operand stack, moves
objects down to mark as list value into VM, and replaces
mark object by list object; if the [ operator carried the TILDE
attribute, the list pn the operand stack is made active;
*/

P op_closelist(void)
{
  B *frame, *mframe;
  P nframes, nb;

  for (frame = o_1, nframes = 0; 
       frame >= FLOORopds && CLASS(frame) != MARK;
       frame -= FRAMEBYTES, nframes++);
  if (frame < FLOORopds) return OPDS_UNF;

  nb = FREEopds - frame - FRAMEBYTES;
  if ((FREEvm + nb + FRAMEBYTES) > CEILvm)  return VM_OVF;
  mframe = FREEvm; 
  FREEvm += FRAMEBYTES;
  TAG(mframe) = LIST;
  ATTR(mframe) = PARENT | ((ATTR(frame) & TILDE) ? ACTIVE : 0);
  VALUE_PTR(mframe) = FREEvm;
  moveframes(frame + FRAMEBYTES, FREEvm, nframes);
  LIST_CEIL_PTR(mframe) = (FREEvm += nb);
  moveframe(mframe,frame);
  FREEopds = frame + FRAMEBYTES;
  return OK;
}

/*------------------------------------ openlist/[
 * pushes mark on the stack 
 */
P op_openlist(void)
{
  if (o2 > CEILopds) return OPDS_OVF;
  TAG(o1) = MARK;
  ATTR(o1) = 0;
  FREEopds = o2;
  return OK;
}

/*------------------------------------- pop
removes top element from operand stack 
*/

P op_pop(void)
{
  if (FREEopds <= FLOORopds) return OPDS_UNF;
  FREEopds -= FRAMEBYTES;
  return OK;
}

/*-------------------------------------- push
  [ any... | --
  any.. are moved to the execution stack
*/

P op_push(void) {
  B* i;
  P framebytes;

  for (i = o_1; i >= FLOORopds && TAG(i) != MARK; i -= FRAMEBYTES);
  if (TAG(i) != MARK) return OPDS_UNF;
  framebytes = FREEopds - i - FRAMEBYTES;

  if (x1 + framebytes > CEILexecs) return EXECS_OVF;
  moveframes(i+FRAMEBYTES, x1, framebytes/FRAMEBYTES);
  FREEexecs = x1 + framebytes;
  FREEopds = i;

  return OK;
}

/*------------------------------------- exch
  any1 any2 | any2 any1

exchanges top two elements of operand stack
*/

P op_exch(void)
{
  B framebuf[FRAMEBYTES];

  if (o_2 < FLOORopds) return OPDS_UNF;
  moveframe(o_1, framebuf);
  moveframe(o_2, o_1);
  moveframe(framebuf, o_2);
  return OK;
}

/*------------------------------------- dup
duplicates top element of operand stack
*/

P op_dup(void)
{
  if (FREEopds <= FLOORopds) return OPDS_UNF;
  if (FREEopds >= CEILopds) return OPDS_OVF;
  moveframe(o_1,o1);
  FREEopds = o2;
  return OK;
}

/*------------------------------------- index
  anyn...any0 n index anyn...any0 anyn

duplicates n-th element counted from top of operand stack.
*/

P op_index(void)
{
  P n,nb;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&n)) return UNDF_VAL;
  nb = (n + 1) * FRAMEBYTES;
  if (((o_1 - nb) < FLOORopds) || (n < 0)) return RNG_CHK;
  moveframe(o_1 - nb, o_1);
  return OK; 
}

/*------------------------------------- clear
clears entire operand stack
*/

P op_clear(void)
{
  FREEopds = FLOORopds;
  return OK;
}

/*------------------------------------- count
returns count of elements on operand stack
*/

P op_count(void)
{
  P nb;

  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  nb = FREEopds - FLOORopds;
  LONGBIG_VAL(o1) = nb / FRAMEBYTES;
  FREEopds = o2;
  return OK;
}

/*------------------------------------- roll
  anyn-1...any0 n j | ...

performs a circular shift of the n objects anyn-1...any0 on the
operand stack by the amount j. Positive j indicates upward motion
on the stack, whereas negative j indicates downward motion. Note
that |j|<n, n>0, and that the operand stack needs to have space
for another n-2 elements.
*/

P op_roll(void)
{
  B *base;  
  P absj,j,n,ubytes,cbytes,nbytes,jbytes;

  if (o_2 <= FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != NUM) || (CLASS(o_1) != NUM)) return OPD_CLA;
  if (!(PVALUE(o_2,&n) && PVALUE(o_1,&j))) return UNDF_VAL;
  absj = (j<0)? -j : j;
  if ((n <= 0) || (absj >= n)) return RNG_CHK;
  ubytes = o_2 - FLOORopds;    cbytes = CEILopds - FLOORopds;
  nbytes = n * FRAMEBYTES; jbytes = absj * FRAMEBYTES;
  if (nbytes > ubytes) return RNG_CHK;
  if ((ubytes + nbytes) > cbytes) return OPDS_OVF;
  base = o_2 - nbytes;
  moveframes(base, base+nbytes, n);
  if (j > 0) { 
    moveframes(base+nbytes, base+jbytes, n-j);
    moveframes(base+nbytes+nbytes-jbytes, base, j); 
  }
  else if (j < 0) { 
    moveframes(base+nbytes, base+nbytes-jbytes, absj);
    moveframes(base+nbytes+jbytes, base, n-absj); 
  }
  FREEopds = o_2;
  return OK;
}

/*------------------------------------- counttomark
counts operand stack elements down to most recent mark,
pushes count numeral onto operand stack
*/

P op_counttomark(void)
{
  B *frame; P nframes;

  frame = o_1; nframes = 0;
  while (CLASS(frame) != MARK) { 
    nframes++;
    if ((frame -= FRAMEBYTES) < FLOORopds) return OPDS_UNF;
  }
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = nframes;
  FREEopds = o2;
  return OK;
}

/*------------------------------------- cleartomark
deletes operand stack elements down to and including
most recent mark
*/

P op_cleartomark(void)
{
  B *frame;

  frame = FREEopds;
  do { 
    frame -= FRAMEBYTES;
    if (frame < FLOORopds) return OPDS_UNF;
  } while (CLASS(frame) != MARK);
  FREEopds = frame;
  return OK;
}

/*------------------------------------ dict
  n | dictionary

makes an empty internal dictionary of length n and returns the new
dictionary frame.
*/

P op_dict(void)
{
  B *dict; LBIG n;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!VALUE(o_1,&n)) return UNDF_VAL;
  if (n < 0 || n >= L32MAX) return RNG_CHK;
  if ((dict = makedict((L32) n)) == (B *)(-1L)) return VM_OVF;
  moveframe(dict - FRAMEBYTES,o_1);
  return OK;
}

/*------------------------------------ cleardict
   dict | dict
   
removes all definitions from a dictionary
*/

P op_cleardict(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != DICT) return OPD_CLA;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  cleardict((B *)VALUE_BASE(o_1));
  return OK;
}

/*------------------------------------ array
  n /type_name | array

makes an array to hold an array of n numerals of specifed type (one
of b, w, l=32bit, x=64bit, s, d); returns new internal object.
*/

P op_array(void)
{
  B *mframe, type[NAMEBYTES+1];  
  W t;  
  P n;
  P nb;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (!PVALUE(o_2,&n)) return UNDF_VAL;
  if (n < 0) return RNG_CHK;

  if (CLASS(o_1) != NAME) return OPD_ERR;
  pullname(o_1,type); 
  t = ascii[(*type) & 0x7F];
  if ((t & 0x4030) != 0x4030) return RNG_CHK; 
  t &= 0x0F;
  nb = DALIGN(n * VALUEBYTES(t)); 
  if ((FREEvm + nb + FRAMEBYTES) > CEILvm) return VM_OVF;
  mframe = FREEvm; FREEvm += FRAMEBYTES;
  TAG(mframe) = ARRAY | t; ATTR(mframe) = PARENT;
  VALUE_BASE(mframe) = (P)FREEvm; ARRAY_SIZE(mframe) = n;
  FREEvm += nb;
  moveframe(mframe,o_2);
  ARRAY_SIZE(o_2) = n;
  FREEopds = o_1;
  return OK;
}

/*------------------------------------ list
  n | list

makes a list to hold n objects; returns new internal list object.
*/

P op_list(void)
{
  B *nullframe, *mframe;  
  P n;
  P nb;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&n)) return UNDF_VAL; 
  if (n < 0) return RNG_CHK;

  nb = n * FRAMEBYTES;
  if ((FREEvm + nb + FRAMEBYTES) > CEILvm) return VM_OVF;
  mframe = FREEvm; FREEvm += FRAMEBYTES;
  TAG(mframe) = LIST; ATTR(mframe) = PARENT;
  VALUE_BASE(mframe) = (P)FREEvm;
  LIST_CEIL(mframe) = (P)(FREEvm += nb);
  nullframe = (B *)VALUE_BASE(mframe);
  while (nullframe < (B *)LIST_CEIL(mframe)) { 
    TAG(nullframe) = NULLOBJ; 
    ATTR(nullframe) = 0; 
    nullframe += FRAMEBYTES; 
  }
  moveframe(mframe,o_1);
  return OK;
}
 
/*------------------------------------ begin
  dict | ---
 
pushes a dictionary onto the dictionary stack, making it the current
directory where name searches start.
*/

P op_begin(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != DICT) return OPD_CLA;
  if (FREEdicts >= CEILdicts) return DICTS_OVF;
  moveframe(o_1, FREEdicts);
  FREEdicts += FRAMEBYTES;
  FREEopds = o_1;
  return OK;
}

/*------------------------------------ end
pops the current dictionary from the dictionary stack; will fail
when attempting to pop one of the two basic dictionaries (system
and user).
*/

P op_end(void)
{
  if ((FREEdicts - FRAMEBYTES - FRAMEBYTES) <= FLOORdicts) return DICTS_UNF;
  FREEdicts -= FRAMEBYTES;
  return OK;
}

/*------------------------------------- def
  /name value | ---

 -   enters the name-value association into the topmost writable
     dictionary present on the dictionary stack
 -   on overflow of the dictionary, this dictionary object on the
     dict stack is made readonly and a DICT_OVF error is executed
*/

P op_def(void)
{
  B *dframe, *dict;

  if (o_2 < FLOORopds) return OPDS_UNF;
  dframe = FREEdicts;
  do { 
    dframe -= FRAMEBYTES;
    if (dframe < FLOORdicts) return DICT_ATR;
    dict = (B *)VALUE_BASE(dframe);
  } while (ATTR(dframe) & READONLY);
  if (CLASS(o_2) != NAME) return OPD_CLA;
  if (ATTR(o_2) & ACTIVE) return OPD_ATR ;
  if (!insert(o_2,dict,o_1)) { 
    ATTR(dframe) |= READONLY; 
    return DICT_OVF; 
  }
  FREEopds = o_2;       
  return OK;
}

/*------------------------------------- name
  value /name | ---

 - analogous to 'def', but uses reverse order of operands
*/

P op_name(void)
{
  B *dframe, *dict;

  if (o_2 < FLOORopds) return OPDS_UNF;
  dframe = FREEdicts;
  do { 
    dframe -= FRAMEBYTES;
    if (dframe < FLOORdicts) return DICT_ATR;
    dict = (B *)VALUE_BASE(dframe);
  } while (ATTR(dframe) & READONLY);
  if (CLASS(o_1) != NAME) return OPD_CLA;
  if (ATTR(o_1) & ACTIVE) return OPD_ATR;
  if (!insert(o_1,dict,o_2)) {  
    ATTR(dframe) |= READONLY; 
    return DICT_OVF; 
  }
  FREEopds = o_2;       
  return OK;
}

/*------------------------------------- find
   /name | value

searches the active dictionaries from the top of the dictionary stack
down for the name and retrieves the associated object.
*/

P op_find(void)
{
  B *dframe, *dict, *aframe;
  
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NAME) return OPD_CLA;
  if (ATTR(o_1) & ACTIVE) return OPD_ATR;
  dframe = FREEdicts - FRAMEBYTES; aframe = 0L;
  while ((dframe >= FLOORdicts) && (aframe == 0L)) { 
    dict = (B *)VALUE_BASE(dframe);
    aframe = lookup(o_1,dict);
    dframe -= FRAMEBYTES;
  }
  if (aframe == 0) return UNDF;
  moveframe(aframe, o_1);
  return OK;
}

/*------------------------------------- get
       array index | numeral
        list index | any
  dictionary /name | any

gets the array/list element at the given index (0...), or the
object associated with a name/string in a dictionary.
*/

P op_get(void)
{
  B *dict, *src; 
  P n;

if (o_2 < FLOORopds) return OPDS_UNF;
 switch(CLASS(o_2)) {
   case ARRAY: 
     if (CLASS(o_1) != NUM) return OPD_CLA;
     if (!PVALUE(o_1,&n)) return UNDF_VAL;
     if ((n < 0) || (n >= ARRAY_SIZE(o_2))) return RNG_CHK;
     VALUE_BASE(o_2) += n * VALUEBYTES(TYPE(o_2));
     ARRAY_SIZE(o_2) = 1L;
     TAG(o_1) = NUM | TYPE(o_2); 
     ATTR(o_1) = 0;
     MOVE(o_2,o_1); 
     moveframe(o_1,o_2);
     break;

   case LIST: 
     if (CLASS(o_1) != NUM) return OPD_ERR;
     if (!PVALUE(o_1,&n)) return UNDF_VAL;
     if (n < 0) return RNG_CHK;
     src = (B *)VALUE_BASE(o_2) + (P)n * FRAMEBYTES;
     if (src >= (B *)LIST_CEIL(o_2)) return RNG_CHK;
     moveframe(src, o_2);
     break;
     
   case DICT: 
     dict = (B *)VALUE_BASE(o_2);
     if (CLASS(o_1) != NAME) return OPD_CLA;
     if (ATTR(o_1) & ACTIVE) return OPD_ATR;
     if ((src = lookup(o_1,dict)) == 0L) return UNDF;
     moveframe(src, o_2);
     break;
     
   default:   
     return OPD_CLA;
 }
 FREEopds = o_1;
 return OK;
}

/*-------------------------------------- put
  numeral array index | ---
       any list index | ---
 any dictionary /name | ---

replaces the array element at index by the numeral, or the list
element at index by any object, or associates the name with any
object in a dictionary.
*/

P op_put(void)
{
  B *dict, *dest;  
  P n;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_2) & READONLY) return OPD_ATR;
  switch(CLASS(o_2)) {
    case ARRAY: 
      if (CLASS(o_1) != NUM) return OPD_CLA;
      if (!PVALUE(o_1,&n)) return UNDF_VAL;
      if ((n < 0) || (n >= ARRAY_SIZE(o_2))) return RNG_CHK;
      if (CLASS(o_3) != NUM) return OPD_CLA;
      VALUE_BASE(o_2) += (P) n * VALUEBYTES(TYPE(o_2));
      ARRAY_SIZE(o_2) = 1L;
      MOVE(o_3, o_2);
      break;

    case LIST: 
      if (CLASS(o_1) != NUM) return OPD_CLA;
      if (!PVALUE(o_1,&n)) return UNDF_VAL;
      if (n < 0) return RNG_CHK;
      dest = (B *)VALUE_BASE(o_2) + n * FRAMEBYTES;
      if (dest >= (B *)LIST_CEIL(o_2)) return RNG_CHK;
      moveframe(o_3, dest);
      break;

    case DICT: 
      dict = (B *)VALUE_BASE(o_2);
      if (CLASS(o_1) != NAME) return OPD_CLA;
      if (ATTR(o_1) & ACTIVE) return OPD_ATR;
      if (!insert(o_1,dict,o_3)) return DICT_OVF;
      break;

    default: return OPD_CLA;
  }
  FREEopds = o_3;
  return OK;
}

/*------------------------------------ known
  dictionary /name | boolean

returns 'true' if the name is defined in the dictionary, else
'false'. Internal dictionaries only.
*/

P op_known(void)
{
  B *dict;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_2) != DICT) || (CLASS(o_1) != NAME)) return OPD_CLA;
  if (ATTR(o_1) & ACTIVE) return OPD_ATR;
  dict = (B *)VALUE_BASE(o_2);
  BOOL_VAL(o_2) = (lookup(o_1,dict) != 0L);
  TAG(o_2) = BOOL; 
  ATTR(o_2) = 0;
  FREEopds = o_1;
  return OK;
}

/*-------------------------------------- getinterval
  array index count | subarray
   list index count | sublist

returns a subarray or sublist starting at index in the original
array/list and extending over count elements.
*/

P op_getinterval(void)
{
  B *base, *ceil; 
  P k, n;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!(PVALUE(o_2,&k) && PVALUE(o_1,&n))) return UNDF_VAL;
  if ((k < 0) || (n < 0)) return RNG_CHK; 
  switch(CLASS(o_3)) {
    case ARRAY: 
      if ((k+n) > ARRAY_SIZE(o_3)) return RNG_CHK;
      VALUE_BASE(o_3) += k * VALUEBYTES(TYPE(o_3));
      ARRAY_SIZE(o_3) = n;
      break;

    case LIST: 
      base = (B *)VALUE_BASE(o_3) + (P) k * FRAMEBYTES;
      ceil = base + n * FRAMEBYTES;
      if (ceil > (B *)LIST_CEIL(o_3)) return RNG_CHK;
      VALUE_BASE(o_3) = (P)base; 
      LIST_CEIL(o_3) = (P)ceil;
      break;
      
    default: 
      return OPD_CLA;
  }
  ATTR(o_3) &= (~PARENT);
  FREEopds = o_2;
  return OK;
}
 
/*------------------------------------- countdictstack
    | int

returns number of elements on dictionary stack
*/

P op_countdictstack(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = (FREEdicts - FLOORdicts) / FRAMEBYTES;
  FREEopds = o2;
  return OK;
}

/*-------------------------------------- dictstack
  list | sublist

 - stores all elements of the dictionary stack into the list and
   returns the sublist of copied elements in the list
 - restores DICT_NB entry in stored dict frames
*/

P op_dictstack(void)
{
  B *dframe; 
  P nb;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != LIST) return OPD_CLA;
  nb = FREEdicts - FLOORdicts;
  if (nb > (LIST_CEIL(o_1) - VALUE_BASE(o_1))) return RNG_CHK;
  moveframes(FLOORdicts, (B *)VALUE_BASE(o_1), nb / FRAMEBYTES);
  LIST_CEIL(o_1) = VALUE_BASE(o_1) + nb; ATTR(o_1) &= (~PARENT);
  for (dframe = (B *)VALUE_BASE(o_1); 
       dframe < (B *)LIST_CEIL(o_1);
       dframe += FRAMEBYTES)
    DICT_NB(dframe) = DICT_NB(VALUE_PTR(dframe) - FRAMEBYTES);
 
  return OK;
}

/*------------------------------------- currentdict
    --- | dict

returns the current dictionary, from the top of the dictionary stack.
*/

P op_currentdict(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  moveframe(FREEdicts - FRAMEBYTES, o1);
  FREEopds = o2;
  return OK;
}

