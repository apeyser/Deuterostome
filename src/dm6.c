/*====================== D machine Rev3.0 (dm6.c) =======================

    - check FPU exception
          - checkFPU

    - monadic math operators
          - neg
          - abs
          - sqrt
          - exp
          - ln
          - lg
          - cos
          - sin
          - tan
          - atan
          - floor
          - ceil
          - acos
          - asin

    - universal copy/conversion operator
          - copy

    - dyadic math operators
          - add
          - sub
          - mul
          - div
          - pwr

    - VM operators / bind
          - save
          - restore
          - vmstatus
          - bind

    - class / type / attribute / conversion / string operators
          - class
          - type
          - readonly
          - active
          - mkread
          - mkact
          - mkpass
          - ctype
          - parcel
          - text
          - number
          - token
          - search
          - anchorsearch

*/

#include "dm.h"
#include <string.h>
#include <stdio.h>
#include <math.h>

/*---------------------------------------------------- checkFPU
     -- | bool

  the boolean signals that an FPU exception has occurred since the
  last check; the exception is cleared.
*/

L op_checkFPU(void)
{
  if (o1 > CEILopds) return(OPDS_OVF);
  TAG(o1) = BOOL; ATTR(o1) = 0; BOOL_VAL(o1) = numovf;
  numovf = FALSE;
  FREEopds = o2;
  return(OK);
}
  
/*---------------------------------------------------- dyadic
     num1 num2 | num1         - add to first scalar
    array1 num | array1       - add numeral to all array elements
 array1 array2 | array1       - add second array onto first

 No restriction on type; 'add' stands for any dyadic operation.
*/

static L dyadop(void)
{
if (o_2 < FLOORopds) return(OPDS_UNF);
if (ATTR(o_2) & READONLY) return(OPD_ATR);
switch(CLASS(o_2))
   { case NUM:  
                  switch(CLASS(o_1))
                   { case NUM: break;
                     case ARRAY: break;
                     default: return(OPD_CLA);
                   }
               break;
     case ARRAY: switch(CLASS(o_1))
                   { case NUM: break;
                     case ARRAY:
                       if (ARRAY_SIZE(o_2) != ARRAY_SIZE(o_1))
                         return(RNG_CHK);
                       break;
                     default: return(OPD_CLA);
                   }
                 break;
     default: return(OPD_CLA);
   }
FREEopds = o_1;
return(OK);
}

L op_thearc(void)
{
L retc;
if ((retc = dyadop()) != OK) return(retc);
THEARC(o_1,o1);
return OK;
}

L op_mod(void)
{
    L retc;
    if ((retc = dyadop()) != OK) return(retc);
    MOD(o_1,o1);
    return OK;
}

L op_add(void)
{
L retc;
if ((retc = dyadop()) != OK) return(retc);
ADD(o_1,o1);
return(OK);
}

L op_sub(void)
{
L retc;
if ((retc = dyadop()) != OK) return(retc);
SUB(o_1,o1);
return(OK);
}

L op_mul(void)
{
L retc;
if ((retc = dyadop()) != OK) return(retc);
MUL(o_1,o1);
return(OK);
}

L op_div(void)
{
L retc;
if ((retc = dyadop()) != OK) return(retc);
DIV(o_1,o1);
return(OK);
}

L op_pwr(void)
{
L retc;
if ((retc = dyadop()) != OK) return(retc);
PWR(o_1,o1);
return(OK);
}

/*-------------------------------------------------- copy
   any1..anyn n | any1..anyn any1..anyn

copies n top elements of operand stack excluding n.

  array1 array2 | subarray2
    num1 array2 | array2
    list1 list2 | sublist2

copies all elements of the first composite object into the second,
starting at index zero of the destination (array or list), and returning
the subarray/list filled by the copy (the remainder of the destination
object is unaffected). The arrays/numeral may be of different types,
then invoking automatic conversion; an array may be copied into a
differently typed replica of itself, thus converting it in place
(towards a type of equal or smaller width). With a numeral in the
place of a first array, the numeral is expanded to fill the entire
destination array, which then is returned. The resulting object
inherits the destination attributes.

NB: internals only

*/

L op_copy(void)
{
B *from, *cframe, cframebuf[FRAMEBYTES];
L n, nb;

cframe = cframebuf;
if (o_1 < FLOORopds) return(OPDS_UNF);

if (CLASS(o_1) == NUM)           /* copy operand stack elements */
   { if (!VALUE(o_1,&n)) return(UNDF_VAL);
     nb = n * FRAMEBYTES;
     if ((from = o_1 - nb) < FLOORopds) return(RNG_CHK);
     if ((o_1 + nb) > CEILopds) return(RNG_CHK);
     moveframes(from, o_1, n);
     FREEopds = o_1 + nb;
     return(OK);
   }

if (o_2 < FLOORopds) return(OPDS_UNF);
if (ATTR(o_1) & READONLY) return(OPD_ATR);

switch(CLASS(o_2))
   {
   case NUM: if (CLASS(o_1) != ARRAY) return(OPD_CLA);
             MOVE(o_2,o_1);
             moveframes(o_1,o_2,1L);
             break;
   case ARRAY: if (CLASS(o_1) != ARRAY) return(OPD_CLA);
               if (ARRAY_SIZE(o_1) < ARRAY_SIZE(o_2)) return(RNG_CHK);
               ARRAY_SIZE(o_1) = ARRAY_SIZE(o_2);
               MOVE(o_2,o_1);
               moveframes(o_1,o_2,1L);
               ATTR(o_2) &= (~PARENT);
               break;
   case LIST: if (CLASS(o_1) != LIST) return(OPD_CLA);
              nb = LIST_CEIL(o_2) - VALUE_BASE(o_2);
              n = nb / FRAMEBYTES;
              if ( (n <=0) || ((LIST_CEIL(o_1) - VALUE_BASE(o_1)) < nb))
                 return(RNG_CHK);
              moveframes((B *)VALUE_BASE(o_2), (B *)VALUE_BASE(o_1), n);
              moveframes(o_1,o_2,1L);
              LIST_CEIL(o_2) = VALUE_BASE(o_2) + nb;
              ATTR(o_2) &= (~PARENT);
              break;
   default: return(OPD_CLA);
   }
FREEopds = o_1;
return(OK);
}

/*---------------------------------------------------- monadic
     num | num
   array | array

 No restriction on type.
*/
static L monop(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
if (ATTR(o_1) & READONLY) return(OPD_ATR);
switch(CLASS(o_1))
   { case NUM: break;
     case ARRAY: break;
     default: return(OPD_CLA);
   }
return(OK);
}

L op_neg(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
NEG(o_1);
return(OK);
}

L op_abs(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
ABS(o_1);
return(OK);
}

L op_sqrt(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
SQRT(o_1);
return(OK);
}

L op_exp(void){
L retc;
if ((retc = monop()) != OK) return(retc);
EXP(o_1);
return(OK);
}

L op_ln(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
LN(o_1);
return(OK);
}

L op_lg(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
LG(o_1);
return(OK);
}

L op_cos(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
COS(o_1);
return(OK);
}

L op_sin(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
SIN(o_1);
return(OK);
}

L op_tan(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
TAN(o_1);
return(OK);
}

L op_atan(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
ATAN(o_1);
return(OK);
}

L op_floor(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
FLOOR(o_1);
return(OK);
}

L op_ceil(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
CEIL(o_1);
return(OK);
}

L op_asin(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
ASIN(o_1);
return(OK);
}

L op_acos(void)
{
L retc;
if ((retc = monop()) != OK) return(retc);
ACOS(o_1);
return(OK);
}

/*----------------------------------------------- save
     --- | VM_box
     
  - creates a box object in VM
  - returns the box object
*/

L op_save(void)
{
B *bf, *bv;

if (o1 >= CEILopds) return(OPDS_OVF);
if ((FREEvm + FRAMEBYTES + SBOXBYTES) > CEILvm) return(VM_OVF);
bf = FREEvm; bv = bf + FRAMEBYTES;
SBOX_NOPDS(bv) = 0; SBOX_NDICTS(bv) = 0; SBOX_CAP(bv) = (B *)0;
TAG(bf) = BOX; ATTR(bf) = PARENT;
VALUE_BASE(bf) = (L)bv; BOX_NB(bf) = SBOXBYTES;
FREEvm = bv + SBOXBYTES;
moveframe(bf,o1); FREEopds = o2;
return(OK);
}

/*----------------------------------------------- capsave
     VM_box | ---
     
  - requires a box object from a preceding 'save'
  - modifies the box value to direct 'restore' to discard objects created
    between 'save' and 'capsave', retaining objects created following 'capsave'
*/

L op_capsave(void)
{
B *box;
if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != BOX) return(OPD_CLA);
box = (B *)VALUE_BASE(o_1); SBOX_CAP(box) = FREEvm;
FREEopds = o_1;
return(OK);
}

/*----------------------------------------------- restore
     VM_box | ---

  - requires a box object from a preceding 'save' (optionally capped by
    'capsave')
  - objects following 'box' in VM will be discarded up to the VM freespace
    (no cap) or to the VM level established by 'capsave'
  - terminates if the execution stack holds references to discardable objects
  - NOTE: uncapped saves handle objects on any stack in the save box
    by removing them from that stack.
  - if cap exists, moves down all objects located above the cap to repack VM;
    in the process, corrects masterframes and dictionary values for the offset
  - replaces discardable objects in retained lists by nulls, maintaining
    their 'active' attribute
  - removes associations to discardable objects (name and object) from
    retained dictionaries
  - adjusts VM freespace
  - NOTE: since we have in the linux version all operator dictionaries
    (system and external) stored ABOVE the VM ceiling, these need be
    exempted from address modifications made to shifted objects
  - NOTE: we no longer support restoration of operand and dictionary
    stacks from uncapped 'save' objects
 */
L op_restore(void)
{
B *cframe, *frame, *dict, *tdict, *entry, *box, *savebox,
  *caplevel, *savefloor, *topframe;
L nb, offset;
BOOLEAN capped;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != BOX) return(OPD_CLA);
savefloor = (B *)VALUE_BASE(o_1) - FRAMEBYTES; savebox = (B *)VALUE_BASE(o_1);
if ((caplevel = SBOX_CAP(savebox)) == (B *)0)
   { capped = FALSE; caplevel = FREEvm; }
   else capped = TRUE;
offset = caplevel - savefloor;
FREEopds = o_1;

topframe = cframe = FREEexecs - FRAMEBYTES;
while (cframe >= FLOORexecs) {
    if (COMPOSITE(cframe)
          && (VALUE_BASE(cframe) >= (L)savefloor)
          && (VALUE_BASE(cframe) < (L)caplevel)) {
        if (capped) return(INV_REST);
        moveframes(cframe + FRAMEBYTES,
                   cframe,
                   (topframe - cframe)/FRAMEBYTES);
        topframe -= FRAMEBYTES;
        FREEexecs -= FRAMEBYTES;
    }
    cframe -= FRAMEBYTES;
}
 
topframe = cframe = FREEdicts - FRAMEBYTES;
while (cframe >= FLOORdicts) {
    if (COMPOSITE(cframe)
          && (VALUE_BASE(cframe) >= (L)savefloor)
          && (VALUE_BASE(cframe) < (L)caplevel)) {
        if (capped) return(INV_REST);
        moveframes(cframe + FRAMEBYTES,
                   cframe,
                   (topframe - cframe)/FRAMEBYTES);
        topframe -= FRAMEBYTES;
        FREEdicts -= FRAMEBYTES;
    }
    cframe -= FRAMEBYTES;
}
 
topframe = cframe = FREEopds - FRAMEBYTES;
while (cframe >= FLOORopds) {
    if (COMPOSITE(cframe)
          && (VALUE_BASE(cframe) >= (L)savefloor)
          && (VALUE_BASE(cframe) < (L)caplevel)) {
        if (capped) return(INV_REST);
        moveframes(cframe + FRAMEBYTES,
                   cframe,
                   (topframe - cframe)/FRAMEBYTES);
        topframe -= FRAMEBYTES;
        FREEopds -= FRAMEBYTES;
    }
    cframe -= FRAMEBYTES;
}
 
if (capped)
   moveD((D *)caplevel, (D *)savefloor, (FREEvm - caplevel)/sizeof(D));

FREEvm -= offset;
cframe = FLOORvm;
while (cframe < FREEvm)
   {
   switch(CLASS(cframe))
      {
   case ARRAY: nb = DALIGN(ARRAY_SIZE(cframe) * VALUEBYTES(TYPE(cframe)));
               if (VALUE_BASE(cframe) >= (L)caplevel)
                  VALUE_BASE(cframe) -= offset;
               cframe += nb + FRAMEBYTES; break;

   case LIST:  if (VALUE_BASE(cframe) >= (L)caplevel)
                 { VALUE_BASE(cframe) -= offset; LIST_CEIL(cframe) -= offset; }
               for (frame = (B *)VALUE_BASE(cframe);
                    frame < (B *)LIST_CEIL(cframe); frame += FRAMEBYTES)
                  { if (COMPOSITE(frame)) {  
                     if ((VALUE_BASE(frame) >= (L)caplevel) &&
                         (VALUE_BASE(frame) < (L) CEILvm))
                       { VALUE_BASE(frame) -= offset;
                         if (CLASS(frame) == LIST) 
                           LIST_CEIL(frame) -= offset;
                       }
		     else if ((VALUE_BASE(frame) >= (L)savefloor) &&
			      (VALUE_BASE(frame) < (L) caplevel))
		       { TAG(frame) = NULLOBJ; ATTR(frame) = 0; }
                    }
                  }
               cframe = (B *)LIST_CEIL(cframe); break;
   case DICT:  if (VALUE_BASE(cframe) >= (L)caplevel)
                  { VALUE_BASE(cframe) -= offset;
		    d_reloc((B *)VALUE_BASE(cframe),VALUE_BASE(cframe)+offset,
                            VALUE_BASE(cframe));
                  }
               dict = (B *)VALUE_BASE(cframe);
               if ((tdict = makedict((DICT_CEIL(dict) - DICT_ENTRIES(dict))
				     / ENTRYBYTES)) == (B *)(-1L)) 
		 return(VM_OVF);
               for (entry = (B *)DICT_ENTRIES(dict);
                    entry < (B *)DICT_FREE(dict); 
		    entry += ENTRYBYTES)
                  {
		  frame = ASSOC_FRAME(entry);
                  if (COMPOSITE(frame) && (VALUE_BASE(frame) < (L)CEILvm))
                    { if (VALUE_BASE(frame) >= (L)caplevel)
                        { VALUE_BASE(frame) -= offset;
                          if (CLASS(frame) == LIST)
                             LIST_CEIL(frame) -= offset;
                        }
                        else
                        if (VALUE_BASE(frame) >= (L)savefloor) continue;
                    }
                  insert(ASSOC_NAME(entry),tdict,frame);
                  } 
               d_rreloc(tdict,(L)tdict,(L)dict);
               moveD((D *)tdict, (D *)dict, DICT_NB(cframe)/sizeof(D));
               FREEvm = tdict - FRAMEBYTES;
               cframe += DICT_NB(cframe) + FRAMEBYTES; break;
    case BOX:  if (VALUE_BASE(cframe) >= (L)caplevel)
                 VALUE_BASE(cframe) -= offset;
               box = (B *)VALUE_BASE(cframe);
	       if (SBOX_CAP(box))
	         { if (SBOX_CAP(box) >= caplevel) SBOX_CAP(box) -= offset;
                   else if (SBOX_CAP(box) > savefloor)
		      SBOX_CAP(box) = savefloor;
                 }
               cframe += BOX_NB(cframe) + FRAMEBYTES; break;
    default:   return(CORR_OBJ);
    }
 }

 if (capped)
     { cframe = FREEexecs - FRAMEBYTES;
       while (cframe >= FLOORexecs)      
        { if (COMPOSITE(cframe))
            if ((VALUE_BASE(cframe) >= (L)caplevel) &&
                (VALUE_BASE(cframe) < (L) CEILvm))
               { VALUE_BASE(cframe) -= offset;
                 if (CLASS(cframe) == LIST) LIST_CEIL(cframe) -= offset;
               }
          cframe -= FRAMEBYTES;
        }
       cframe = FREEdicts - FRAMEBYTES;
       while (cframe >= FLOORdicts)      
        { if (COMPOSITE(cframe))
            if ((VALUE_BASE(cframe) >= (L)caplevel) &&
                (VALUE_BASE(cframe) < (L) CEILvm))
               { VALUE_BASE(cframe) -= offset;
                 if (CLASS(cframe) == LIST) LIST_CEIL(cframe) -= offset;
               }
          cframe -= FRAMEBYTES;
        }
       cframe = FREEopds - FRAMEBYTES;
       while (cframe >= FLOORopds)      
        { if (COMPOSITE(cframe))
            if ((VALUE_BASE(cframe) >= (L)caplevel) &&
                (VALUE_BASE(cframe) < (L) CEILvm))
               { VALUE_BASE(cframe) -= offset;
                 if (CLASS(cframe) == LIST) LIST_CEIL(cframe) -= offset;
               }
          cframe -= FRAMEBYTES;
        } 
     }
return(OK);
}

/*----------------------------------------------- vmstatus
   --- | max used
*/
L op_vmstatus(void)
{
if (o2 >= CEILopds) return(OPDS_OVF);

TAG(o1) = TAG(o2) = NUM | LONGTYPE;
ATTR(o1) = ATTR(o2) = 0;
LONG_VAL(o1) = CEILvm - FLOORvm;
LONG_VAL(o2) = FREEvm - FLOORvm;
FREEopds = o3;
return(OK);
}

/*----------------------------------------------- bind
   proc | proc

 - replaces executable names in proc that resolve to operators by their
   value
 - in addition, applies itself recursively to any not write-protected
   internal procedure nested in proc, and makes that procedure read-only
 - does not distinguish between system and dynamic operators
*/

// name bind conflicts with socket bind function, changed to dmbind
static L dmbind(B *pframe)
/*B *pframe;*/
{
L retc; B *frame, *xframe, *dframe, *dict;

if ((ATTR(pframe) & (READONLY | ACTIVE)) != ACTIVE) return(OK);
frame = (B *)VALUE_BASE(pframe);
while (frame < (B *)LIST_CEIL(pframe))
   { switch(CLASS(frame))
       {
       case PROC: if ((retc = dmbind(frame)) != OK) return(retc); break;
       case NAME: if ((ATTR(frame) & ACTIVE) == 0) break;
                  dframe = FREEdicts - FRAMEBYTES; xframe = 0;
                  while ((dframe >= FLOORdicts) && (xframe == 0L))
                    { dict = (B *)VALUE_BASE(dframe);
                      xframe = lookup(frame,dict);
                      dframe -= FRAMEBYTES;
                    }
                  if ((L)xframe > 0) if (CLASS(xframe) == OP)
                     moveframes(xframe,frame,1L);
                  break;
       }
     frame += FRAMEBYTES;
   }
ATTR(pframe) |= READONLY;
return(OK);
}

L op_bind(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != PROC) return(OK);
return(dmbind(o_1));
}

/*------------------------------------------- class
   object | /classname     (see below)
*/

L op_class(void)
{
B *s;

if (o_1 < FLOORopds) return(OPDS_UNF);
switch(CLASS(o_1))
     {
     case NULLOBJ:  s = "nullclass"; break;
     case NUM:   s = "numclass"; break;
     case OP:    s = "opclass"; break;
     case NAME:  s = "nameclass"; break;
     case BOOL:  s = "boolclass"; break;
     case MARK:  s = "markclass"; break;
     case ARRAY: s = "arrayclass"; break;
     case LIST:  s = "listclass"; break;
     case DICT:  s = "dictclass"; break;
     case BOX:   s = "boxclass"; break;
     default: return(CORR_OBJ);
     }
makename(s,o_1);
return(OK);
}

/*------------------------------------------- type
   object | /type (upper-case, one-letter code)
   NULLOBJ | /T (socket) or /N (none)
   BOX     | /M (mclib) or  /N (none)
   DICT    | /O (oplibtype) or /N (none)
*/

L op_type(void)
{
B c[2]; W key;

if (o_1 < FLOORopds) return(OPDS_UNF);

switch (CLASS(o_1)) {
    case NULLOBJ:
        if (TYPE(o_1) == SOCKETTYPE) *c = 'T';
        else *c = 'N';
        break;
    case BOX:
        *c = 'N';
        break;
    case DICT:
        if (TYPE(o_1) == OPLIBTYPE) *c = 'O';
        else *c = 'N';
        break;
    case NUM: case ARRAY:
        key = 0x4030 | TYPE(o_1);
        for (*c = 'A'; *c <= 'Z'; (*c)++) 
        { if ((ascii[(*c) & 0x7F] & 0x403F) == key) goto op79_1; }
        return(RNG_CHK);
    default:
        return(OPD_CLA);
};
 
op79_1:
c[1] = '\000'; makename(c,o_1);
return(OK);
}

/*------------------------------------------- readonly
     object | boolean       (reports 'read-only' attribute of frame)
*/

L op_readonly(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
BOOL_VAL(o_1) = ((ATTR(o_1) & READONLY) != 0);
TAG(o_1) = BOOL; ATTR(o_1) = 0; 
return(OK);
}

/*------------------------------------------- active
     object | boolean       (reports 'active' attribute)
*/

L op_active(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
BOOL_VAL(o_1) = ((ATTR(o_1) & ACTIVE) != 0);
TAG(o_1) = BOOL; ATTR(o_1) = 0;
return(OK);
}

/*---------------------------------------------- tilde
 * object | boolean (reports 'tilde' attribute)
 */

L op_tilde(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
BOOL_VAL(o_1) = ((ATTR(o_1) & TILDE) != 0);
TAG(o_1) = BOOL; ATTR(o_1) = 0;
return(OK);
}

/*------------------------------------------- mkread
     object | readonly_object

 - marks the 'readonly' attribute in operand frame
*/

L op_mkread(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
ATTR(o_1) |= READONLY;
return(OK);
}

/*------------------------------------------- mkact
     object | active_object
*/

L op_mkact(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
ATTR(o_1) |= ACTIVE;
return(OK);
}

/*------------------------------------------ mkpass
     object | passive_object
*/

L op_mkpass(void)
{
if (o_1 < FLOORopds) return(OPDS_UNF);
ATTR(o_1) &= (~ACTIVE);
return(OK);
}

/*------------------------------------------ ctype
     numeral /type | numeral       (type and value converted)
       array /type | array         (type converted, length adjusted)
*/

L op_ctype(void)
{
B s[NAMEBYTES+1]; W type;

if (o_2 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != NAME) return(OPD_ERR);
pullname(o_1,s); type = ascii[(*s) & 0x7F];
if ((type & 0x4030) != 0x4030) return(RNG_CHK); type &= 0x0F;
switch(CLASS(o_2))
     {
     case NUM: TAG(o_1) = NUM | type; ATTR(o_1) = ATTR(o_2);
               MOVE(o_2,o_1); moveframes(o_1,o_2,1L);
               break;
     case ARRAY: ARRAY_SIZE(o_2) = (ARRAY_SIZE(o_2) * 
                   VALUEBYTES(TYPE(o_2))) / VALUEBYTES(type);
                 TAG(o_2) = ARRAY | type; ATTR(o_2) &= (~PARENT);
                 break;
     default: return(OPD_CLA);
     }
FREEopds = o_1;
return(OK);
}

/*------------------------------------------ parcel
     array1 length /type | remainder_of_array1 array2
  
  parcels array2 of given type and length from an initial subarray of
  array 1, which may be of different type (the returned arrays are
  word-aligned as necessary).
*/

L op_parcel(void)
{
B s[NAMEBYTES+1]; W type;
L length, sadjust, badjust, nb;

if (o_3 < FLOORopds) return(OPDS_UNF);
if ((CLASS(o_3) != ARRAY) || (CLASS(o_2) != NUM)) return(OPD_CLA);
if (CLASS(o_1) != NAME) return(OPD_ERR);
if (!VALUE(o_2,&length)) return(UNDF_VAL);
pullname(o_1,s); type = ascii[(*s) & 0x7F];
if ((type & 0x4030) != 0x4030) return(RNG_CHK); type &= 0x0F;
badjust = sadjust = 0;
if (VALUEBYTES(type) & 1)
    { if ((VALUEBYTES(TYPE(o_3)) & 1) == 0) sadjust = (length & 1); }
    else
    { badjust = (VALUE_BASE(o_3) & 1); }
nb = length * VALUEBYTES(type) + badjust + sadjust;
if ((ARRAY_SIZE(o_3) * VALUEBYTES(TYPE(o_3))) < nb) return(RNG_CHK);
TAG(o_2) = ARRAY | type; ATTR(o_2) = ATTR(o_3) & (~PARENT);
VALUE_BASE(o_2) = VALUE_BASE(o_3) + badjust;
VALUE_BASE(o_3) += nb;
ARRAY_SIZE(o_2) = length;
ARRAY_SIZE(o_3) = (ARRAY_SIZE(o_3) * VALUEBYTES(TYPE(o_3)) - nb)
                     / VALUEBYTES(TYPE(o_3));
FREEopds = o_1;
return(OK);
}

/*------------------------------------------ text
   string1 index signed_width/undef one_of_x | string1 new_index

   x = numeral / string / name / operator

 A non-string item x is converted to string form (numeral: used
 as ASCII code for one character; name: namestring is used; operator:
 operator name is used). The resulting string is copied into a field
 of given or corresponding (if undefined) width in string1 starting
 at the index. A negative width specifies left-adjustment within the 
 field. The unused part of the field receives spaces.  
*/

L op_text(void)
{
B *src, *dest, code, sbuf[NAMEBYTES+1];
L index, val, length, width, start;

if (o_4 < FLOORopds) return(OPDS_UNF);
if (ATTR(o_4) & READONLY) return(OPD_ATR);
if ((TAG(o_4) != (ARRAY | BYTETYPE)) || (CLASS(o_3) != NUM))
   return(OPD_ERR);
if (!VALUE(o_3,&index)) return(UNDF_VAL);
switch(CLASS(o_1))
     {
     case NUM: if (!VALUE(o_1,&val)) return(UNDF_VAL);
               if ((val < 0) || (val > 255)) return(UNDF_VAL);
               code = (B)val; src = &code; length = 1; break;
     case OP: src = (B *)OP_NAME(o_1); length = strlen(src); break;
     case NAME: src = sbuf; pullname(o_1,src);
                length = strlen(src); break;
     case ARRAY: if (TYPE(o_1) != BYTETYPE) return(OPD_TYP);
                 src = (B *)VALUE_BASE(o_1); length = ARRAY_SIZE(o_1);
                 break;
     default: return(OPD_CLA);
     }
if (CLASS(o_2) != NUM) return(OPD_CLA);
if (!VALUE(o_2,&width)) { width = length; start = index; }
   else { if (width < 0)
             { width = -width; start = index; }
             else
             { start = index + width - length; }
          if (length > width) return(RNG_CHK);
        }
if ((index + width) > ARRAY_SIZE(o_4)) return(RNG_CHK);
TAG(o_3) = NUM | LONGTYPE;
LONG_VAL(o_3) = index + width;
dest = (B *)VALUE_BASE(o_4) + index;
while (index < start) { *(dest++) = ' '; index++; width--; }
while (length) { length--; *(dest++) = *(src++); width--; }
while (width) { *(dest++) = ' '; width--; }
FREEopds = o_2;
return(OK);
}

/*------------------------------------------ number
     string index signed_width/undef numeral format_int/undef |
     string new_index

 - sign of format integer selects between fixed-point (<0) and 
   floating-point (>0) formats; value gives precision (i.e. fractional
   digits); an undefined in this place selects automatic formatting. The
   precision is ignored for integer numerals.
*/

L op_number(void)
{
static B buf[30];
L prec; BOOLEAN fauto;

if (o_2 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) != NUM) return(OPD_CLA);
if (!VALUE(o_1,&prec)) fauto = TRUE;
   else { if ((prec < -17) || (prec > 17)) return(RNG_CHK); 
          fauto = FALSE;
        } 
if (CLASS(o_2) != NUM) return(OPD_CLA);
DECODE(o_2,fauto,prec,buf);
TAG(o_2) = ARRAY | BYTETYPE; ATTR(o_2) = READONLY;
VALUE_BASE(o_2) = (L)buf; ARRAY_SIZE(o_2) = strlen(buf);
FREEopds = o_1;
return(op_text());
}

/*------------------------------------------ token
     string | remainder_of_string object true
              string false
*/    

L op_token(void)
{
L retc; BOOLEAN bool;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if ((retc = tokenize(o_1)) == OK) bool = TRUE;
   else if (retc == DONE) bool = FALSE;
      else return(retc);
if (o1 >= CEILopds) return(OPDS_OVF);
TAG(o1) = BOOL; ATTR(o1) = 0;
BOOL_VAL(o1) = bool;
FREEopds = o2;
return(OK);
}

/*--------------------------------------------- search
   string seek | if found: post match pre true
               | else:     string false

  - searches string for substring seek; divides string on success
*/

L op_search(void)
{
UB *string, *xstring, *seek;
L nstring, nseek;

if (o_2 < FLOORopds) return(OPDS_UNF);
if ((TAG(o_2) != (ARRAY | BYTETYPE)) ||
    (TAG(o_1) != (ARRAY | BYTETYPE))) return(OPD_ERR);
if (ARRAY_SIZE(o_2) == 0) goto op90_f;
if (ARRAY_SIZE(o_1) == 0) return(RNG_CHK);
string = (UB *)VALUE_BASE(o_2); nstring = ARRAY_SIZE(o_2);

op90_1:
seek = (UB *)VALUE_BASE(o_1); nseek = ARRAY_SIZE(o_1);
while (nstring >= nseek)
   { if ((*string) == (*seek)) goto op90_2; string++; nstring--; }
goto op90_f;

op90_2:
nseek--; xstring = string + 1; nstring--;
while (nseek)
   { if ((*(xstring++)) != (*(++seek))) { string++; goto op90_1; }
     nseek--;
   }
if (o2 >= CEILopds) return(OPDS_OVF);
moveframe(o_2,o1);
ARRAY_SIZE(o1) = (L)string - VALUE_BASE(o1);
VALUE_BASE(o_1) = (L)string;
VALUE_BASE(o_2) = (L)xstring;
ARRAY_SIZE(o_2) = nstring - ARRAY_SIZE(o_1) + 1;
ATTR(o_2) &= ~PARENT;
ATTR(o1) = ATTR(o_1) = ATTR(o_2);
TAG(o2) = BOOL; ATTR(o2) = 0;
BOOL_VAL(o2) = TRUE;
FREEopds = o3;
return(OK);

op90_f:
TAG(o_1) = BOOL; ATTR(o_1) = 0; BOOL_VAL(o_1) = FALSE;
return(OK);
}

/*--------------------------------------------- anchorsearch
   string seek | if found: post match true
               | else:     string false

  - tests string for initial substring seek; divides string on success
*/

L op_anchorsearch(void)
{
UB *string, *seek;
L nstring, nseek;

if (o_2 < FLOORopds) return(OPDS_UNF);
if ((TAG(o_2) != (ARRAY | BYTETYPE)) ||
    (TAG(o_2) != (ARRAY | BYTETYPE))) return(OPD_ERR);
if (ARRAY_SIZE(o_2) == 0) goto op91_f;
if (ARRAY_SIZE(o_1) == 0) return(RNG_CHK);
string = (UB *)VALUE_BASE(o_2); nstring = ARRAY_SIZE(o_2);
seek = (UB *)VALUE_BASE(o_1); nseek = ARRAY_SIZE(o_1);
if (nstring < nseek) goto op91_f;
while (nseek)
   { if (*(string++) != *(seek++)) goto op91_f;
     nseek--;
   }
if (o1 >= CEILopds) return(OPDS_OVF);
nseek = ARRAY_SIZE(o_1);
moveframes(o_2,o_1,1L); ARRAY_SIZE(o_1) = nseek;
VALUE_BASE(o_2) += nseek; ARRAY_SIZE(o_2) -= nseek;
ATTR(o_2) &= ~PARENT; ATTR(o_1) = ATTR(o_2);
TAG(o1) = BOOL; ATTR(o1) = 0; BOOL_VAL(o1) = TRUE;
FREEopds = o2;
return(OK);

op91_f:
TAG(o_1) = BOOL; ATTR(o_1) = 0; BOOL_VAL(o_1) = FALSE;
return(OK);
}
