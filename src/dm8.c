/*=================== D Machine 3.0 (dm8.c) =========================

     - operators inquiring about composite (VM or eVM) objects
          - used
          - length
          - valid
          - parent
     - operators for exchanging contents of external and internal objects
          - fax
          - merge
     - operator for accessing eVM objects sequentially
          - nextobject

========================================================================*/

/*------------------------------ externals -----------------------------*/
#include "dm.h"
#include <math.h>

/*------------------------------------ used
  dict | int

returns number of entries used in a dictionary. In case of an external
dictionary, this requires access to the medium.
*/

L op_used(void)
{
B *dict; L nd;

nd = (L)DICTBYTES;
if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != DICT) return(OPD_CLA);
dict = (B *)VALUE_BASE(o_1);
 LONG_VAL(o_1) = (DICT_FREE(dict)-DICT_ENTRIES(dict))/ENTRYBYTES;
TAG(o_1) = NUM | LONGTYPE; ATTR(o_1) = 0;
return(OK);
}

/*------------------------------------ length
  dict/array/list/box | int

returns (maximal) number of items in a composite object. If the object
is an external dictionary, this requires access to the medium.
*/

L op_length(void)
{
B *dict; L n, nd;

nd = (L)DICTBYTES;
if (o_1 < FLOORopds) return(OPDS_UNF);
switch(CLASS(o_1))
     {
     case ARRAY: n = ARRAY_SIZE(o_1); break;
     case LIST: n = (LIST_CEIL(o_1) - VALUE_BASE(o_1))/FRAMEBYTES;  break;
     case DICT: dict = (B *)VALUE_BASE(o_1);
       n = (DICT_CEIL(dict) - DICT_ENTRIES(dict))/ENTRYBYTES; break;
     case BOX: n = BOX_NB(o_1); break;
     default: return(OPD_CLA);
     }
TAG(o_1) = NUM | LONGTYPE; ATTR(o_1) = 0; LONG_VAL(o_1) = n;
return(OK);
}

/*---------------------------------------------- fax
     dest_list index1 src_list | dest_list index2
   dest_array index1 src_array | dest_array index2

 - faxes value of internal/external composite object into
   internal/external composite object (any combination)
 - array types match
 - in the case of byte arrays: dest and source starting addresses have
   to be even; also, in case of odd source length, one dest byte
   following the range mofified by 'fax' becomes undefined
 - the destination accommodates the entire source
 - the list/subarray and updated index are returned
 - no internal objects are contained in the list faxed to an external list
 - internal VM space is used as buffer of external/external faxes of
   arrays (a minimum of 20 kB is required and is used repeatedly to
   transfer the entire external fax regardless of its size)
 - external/external faxes of lists need to fit into the current VM 
   freespace
*/

L op_fax(void)
{
B *dest;
L n, nb, index; 

if (o_3 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_2) != NUM) return(OPD_CLA);
if (!VALUE(o_2,&index)) return(UNDF_VAL);
if (index < 0) return(RNG_CHK);
if (CLASS(o_3) != CLASS(o_1)) return(OPD_CLA);
switch(CLASS(o_3))
     {
     case ARRAY: goto f_arr;
     case LIST: goto f_list;
     default: return(OPD_CLA);
     } 

/*-- fax an array */
f_arr:
if (ATTR(o_3) & READONLY) return(OPD_ATR);
if (TYPE(o_3) != TYPE(o_1)) return(OPD_TYP);
n = ARRAY_SIZE(o_1);
nb = n * VALUEBYTES(TYPE(o_1));
dest = (B *)VALUE_BASE(o_3) + index * VALUEBYTES(TYPE(o_3));
if ((index + n) > ARRAY_SIZE(o_3)) return(RNG_CHK);
 switch(TYPE(o_3)) {
 case BYTETYPE: moveB((B *)VALUE_BASE(o_1),(B *)dest,n); break;
 case WORDTYPE: moveW((W *)VALUE_BASE(o_1),(W *)dest,n); break;
 case LONGTYPE: moveL((L *)VALUE_BASE(o_1),(L *)dest,n); break;
 case SINGLETYPE: moveS((S *)VALUE_BASE(o_1),(S *)dest,n); break;
 case DOUBLETYPE: moveD((D *)VALUE_BASE(o_1),(D *)dest,n); break;
 }
LONG_VAL(o_2) += n;
FREEopds = o_1;
return(OK);

/*-- fax a list */
f_list:
if (ATTR(o_3) & READONLY) return(OPD_ATR);
nb = LIST_CEIL(o_1) - VALUE_BASE(o_1);
dest = (B *)VALUE_BASE(o_3) + index * FRAMEBYTES;
if ((dest + nb) > (B *)LIST_CEIL(o_3)) return(RNG_CHK);
moveframes((B *)VALUE_BASE(o_1),(B *)dest, nb / FRAMEBYTES);
LONG_VAL(o_2) += nb / FRAMEBYTES;
FREEopds = o_1;
return(OK);
}

/*--------------------------------------------- merge
    dest_dict src_dict | dest_dict

 - accepts any combination of internal/external dictionaries
 - acts like 'put' operator with all entries of the source
 - no internal associations are accepted into external destination
*/

L op_merge(void)
{
B *dict1, *dict2, *freevm,  *entry;

if (o_2 < FLOORopds) return(OPDS_UNF);
if ((CLASS(o_2) != DICT) || (CLASS(o_1) != DICT)) return(OPD_CLA);
freevm = FREEvm;
if (ATTR(o_2) & READONLY) return(DICT_ATR);
dict2 = (B *)VALUE_BASE(o_1);
if ((freevm + DICT_NB(o_2)) > CEILvm) return(VM_OVF);
dict1 = freevm;
moveW((W *)VALUE_BASE(o_2),(W *)dict1,DICT_NB(o_2)>>1);
d_reloc(dict1,VALUE_BASE(o_2),(L)dict1);
for (entry = (B *)DICT_ENTRIES(dict2); entry < (B *)DICT_FREE(dict2); 
     entry += ENTRYBYTES)
  { if (!insert(ASSOC_NAME(entry),dict1,ASSOC_FRAME(entry)))
         return(DICT_OVF);
    }
d_rreloc(dict1,(L)dict1,VALUE_BASE(o_2));
moveW((W *)dict1,(W *)VALUE_BASE(o_2),DICT_NB(o_2)>>1);
FREEopds = o_1;
return(OK);
}

/*---------------------------------------------- nextobject

             null | first_obj true
                  | false

             obj  | next_obj true
                  | false

 - retrieves first object created from VM (null)
 - or retrieves object following obj in order of creation
 - NB: nextobject has no knowledge of child composite objects

*/

L op_nextobject(void)
{
B framebuf[FRAMEBYTES], *next;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) == NULLOBJ)
   { moveframe(FLOORvm,framebuf); goto n_pos; }
   else  /* next */
   { switch(CLASS(o_1))
       {
       case ARRAY: /* if ((ATTR(o_1) & PARENT) == 0) return(OPD_ATR);*/
                   next = (B *)VALUE_BASE(o_1) + DALIGN(ARRAY_SIZE(o_1) *
                   VALUEBYTES(TYPE(o_1))); break;
       case LIST: /* if ((ATTR(o_1) & PARENT) == 0) return(OPD_ATR); */
                  next = (B *)LIST_CEIL(o_1); break;
       case DICT: next = (B *)VALUE_BASE(o_1) + DICT_NB(o_1); break;
       case BOX:  next = (B *)VALUE_BASE(o_1) + BOX_NB(o_1); break;
       default: return(OPD_CLA); 
       }
       if (next >= FREEvm) goto n_neg;
       moveframes(next,framebuf,1L); goto n_pos;
   }

n_neg:
TAG(o_1) = BOOL; ATTR(o_1) = 0; BOOL_VAL(o_1) = FALSE;
return(OK);

n_pos:
if (o1 >= CEILopds) return(OPDS_OVF);
moveframes(framebuf,o_1,1L);
TAG(o1) = BOOL; ATTR(o1) = 0; BOOL_VAL(o1) = TRUE;
FREEopds = o2;
return(OK);
}



