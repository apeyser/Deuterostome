/*==================== D Machine for PPC: DMNUM =========================

           DECODE(B *frame, BOOLEAN fauto, W prec, B *buf)
   B       ENCODE(B type, B *string, B *dnum)
   W       VALUEBYTES(B type)
   BOOLEAN VALUE(B *frame, B *val)
   W       TEST(B *frame)
   W       COMPARE(B *frame1, B *frame2)
           MOVE(B *sframe, B *dframe)

   dyadic math operators (ADD, SUB, MUL, DIV, PWR):
           dyadic(B *sframe, B *dframe)

   monadic math operators (NEG, ABS, SQRT, EXP, LN, LG,
                           FLOOR, CEIL, MOD, 
                           SIN, COS, TAN, ASIN, ACOS, ATAN):
          monadic(B *frame)
 
           DECREMENT(B *frame)
           LVALUE(B *frame, L *dest)
*/

#include "dmnum.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*----------------------- General remarks -------------------------------

 - This processor accepts the integer types, B, W, and L, and floating-
   point types conforming to the PPC-generic IEEE formats, S and D.

 - All arithmetical operations accept all operand types and combinations
   of operand types. All operations are performed in IEEE double-precision
   floating point, following conversion, as necessary, of all operands to
   double format and followed by conversion to the type of the result.

 - Undefined (*) values are quietly accepted and quietly passed on as
   undefined results. Operations that generate invalid results quietly
   return the value, *. The integer * values are represented by the most
   negative value of the respective two's complement range, and the
   S and D type * values are the quiet Nan's (+-INF) of the IEEE standard.
*/

/*----------------------- include: DMNUMINC.H */
#define Pi 3.141592653589793
static D thearc(D x, D y)
{
    D phi = acos(x/sqrt(x*x+y*y));
    return (y > 0) ? phi : 2*Pi-phi;
}

#include "dmnuminc.h"

/*---------------------------------------------------- DECODE */

void DECODE(B *frame, BOOLEAN fauto, W prec, B *buf)
{
  switch(TYPE(frame)) {
    case BYTETYPE: 
      if (*((B *)NUM_VAL(frame)) == BINF) break;
      sprintf(buf,"%d",(W)(*((B *)NUM_VAL(frame))));
      return;

    case WORDTYPE: 
      if(*((W *)NUM_VAL(frame)) == WINF) break;
      sprintf(buf,"%d",*((W *)NUM_VAL(frame)));
      return;

    case LONG32TYPE: 
      if(*((L32 *)NUM_VAL(frame)) == L32INF) break;
      sprintf(buf,"%lld",(long long) *((L32 *)NUM_VAL(frame)));
      return;

    case LONG64TYPE: 
      if (*((L64 *)NUM_VAL(frame)) == L64INF) break;
      sprintf(buf,"%lld",(long long) *((L64 *)NUM_VAL(frame)));
      return;

    case SINGLETYPE: 
      if (ISUNDEF(*((S *)NUM_VAL(frame)))) break;
      
      if (fauto) sprintf(buf,"%.6e",*((S *)NUM_VAL(frame)));
      else if (prec < 0) 
        sprintf(buf,"%.*f",-prec, (D)(*((S *)NUM_VAL(frame))));
      else sprintf(buf,"%.*e",prec, (D)(*((S *)NUM_VAL(frame))));
      return;

     case DOUBLETYPE: 
       if (ISUNDEF(*((D *)NUM_VAL(frame)))) break;
       
       if (fauto) sprintf(buf,"%.15e",*((D *)NUM_VAL(frame)));
       else if (prec < 0)
         sprintf(buf,"%.*f",-prec,(*((D *)NUM_VAL(frame))));
       else sprintf(buf,"%.*e",prec,(*((D *)NUM_VAL(frame))));
       return;

    default: 
      break;
  }
  sprintf(buf,"*");
}

/*------------------------------------- ENCODE
 converts numeral from ASCII code in 'string' into binary form and
 places the binary value at 'dnum'. Encoding is directed by 'type' bits:
     #0-3:  predefined type (B/W/L/M/S/D) if bit #6 is true
     #4:    to be undefined (U)
     #5:    string has float characteristic (F)
     #6:    type is predefined (T)
 If the conversion into the target type produces a numerical overflow,
 the result is set to 'undefined'. ENCODE returns the actual type given
 to the converted numeral.

 Note: uses a function list provided by DMNUMINC

*/

#define Ubit 0x10
#define Fbit 0x20
#define Tbit 0x40

B ENCODE(W type, B *string, B *dnum)
{
  D num;
  num = (type & Ubit)? HUGE_VAL : atof(string);
  if (type & Tbit) { 
    (*(ENCODElist[type & 0xF]))(num,dnum); 
    return(type & 0xF); 
  }
  else if (type & Fbit) {  
    (*(ENCODElist[DOUBLETYPE]))(num,dnum); 
    return(DOUBLETYPE); 
  }
  else { 
    (*(ENCODElist[LONGBIGTYPE]))(num,dnum); 
    return(LONGBIGTYPE); 
  }
}

/*----------------------------------------- VALUEBYTES
  returns width in bytes of numerical type
*/

W VALUEBYTES(B type)
{
  return(TYPEBYTES[type]);
}

/*-------------------------------------------- VALUE
  returns value of numerical frame as long big integer in the destination
  and a boolean 'true' if the value is not undefined

  Note: uses a function list provided by DMNUMINC
*/

BOOLEAN VALUE(B *frame, LBIG *val)
{
  return( (*val = (*(VALUElist[TYPE(frame)]))(NUM_VAL(frame))) != LBIGINF);
}

/*-------------------------------------------- DVALUE
  returns value of numerical frame as double in the destination
  and a boolean 'true' if the value is not undefined

  Note: uses a function list provided by DMNUMINC
*/

BOOLEAN DVALUE(B *frame, D *val) 
{
  *val = (*(TESTlist[TYPE(frame)]))(NUM_VAL(frame));
  return ! ISUNDEF(*val);
}

/*--------------------------------------------- TEST
  tests a numeral frame value for >0, < 0, ==0, and undefined;
  returns corresponding code word (1,-1,0 ,2)

  Note: uses function list provided by DMNUMINC
*/

W TEST(B *frame)
{
  D t; 

  t = (*(TESTlist[TYPE(frame)]))(NUM_VAL(frame));
  if (ISUNDEF(t)) return(UN);
  if (t) return( (t>0)? GT : LT );
  return(EQ);
}

/*------------------------------------------------ COMPARE
  compares the values of two numeral frames; returns same code word
  as does TEST
*/

W COMPARE(B *frame1, B *frame2)
{
  D t;
  
  t = (*(TESTlist[TYPE(frame1)]))(NUM_VAL(frame1)) - 
    (*(TESTlist[TYPE(frame2)]))(NUM_VAL(frame2));
  if (ISUNDEF(t)) return(UN);
  if (t) return( (t>0)? GT : LT );
  return(EQ);
}


/*--------------------------------------------- MOVE
  moves, under proper type conversion the value among the following
  numerical objects:     scalar -> scalar
                         scalar -> array
                         array  -> array

  Undefined values are copied in proper translation; conversion
  results exceeding the destination numerical range become undefined.
  Can be used for conversion in place if source type VALUEBYTES >=
  destination type VALUEBYTES.

  Note: uses function list provided by DMNUMINC

*/

void MOVE(B *sframe, B *dframe)
{
  W idx;
  
  /* index (slow to fast): source type, dest type, class combination */ 
  idx = ( TYPE(sframe) * nTYPES + TYPE(dframe) ) * 4;
  idx += (CLASS(sframe) == ARRAY)? 2 : 0;
  idx += (CLASS(dframe) == ARRAY)? 1 : 0;
  (*(MOVElist[idx]))(sframe,dframe);
} 

/*----------------------------------------- dyadic math operators:
   ADD, SUB, MUL, DIV, PWR:  ...(B *sframe, B *dframe)

 Note: use function lists provided by DMNUMINC

*/

void THEARC(B *dframe, B* sframe)
{
  W idx;
  idx = (TYPE(dframe) * nTYPES + TYPE(sframe))*4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(THEARClist[idx]))(dframe,sframe);
}

void MOD(B *dframe, B* sframe)
{
  W idx;
  idx = (TYPE(dframe) * nTYPES + TYPE(sframe))*4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(MODlist[idx]))(dframe,sframe);
}

void ADD(B *dframe, B *sframe)
{
  W idx;

  /* index (slow to fast): dest type, source type, class combination
     (ss, as, sa, aa, in order dest, source)
  */ 
  idx = ( TYPE(dframe) * nTYPES + TYPE(sframe) ) * 4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(ADDlist[idx]))(dframe,sframe);
} 

void SUB(B *dframe, B *sframe)
{
  W idx; 
  idx = ( TYPE(dframe) * nTYPES + TYPE(sframe) ) * 4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(SUBlist[idx]))(dframe,sframe);
} 

void MUL(B *dframe, B *sframe)
{
  W idx; 
  idx = ( TYPE(dframe) * nTYPES + TYPE(sframe) ) * 4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(MULlist[idx]))(dframe,sframe);
} 

void DIV(B *dframe, B *sframe)
{
  W idx; 
  idx = ( TYPE(dframe) * nTYPES + TYPE(sframe) ) * 4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(DIVlist[idx]))(dframe,sframe);
} 

void PWR(B *dframe, B *sframe)
{
  W idx; 
  idx = ( TYPE(dframe) * nTYPES + TYPE(sframe) ) * 4;
  if (CLASS(dframe) == ARRAY) idx += 1;
  if (CLASS(sframe) == ARRAY) idx += 2;
  (*(PWRlist[idx]))(dframe,sframe);
}
/*---------------------------------------  monadic math operators
   NEG, ABS, SQRT, EXP, LN, LG,FLOOR, CEIL, 
   SIN, COS, TAN, ASIN, ACOS, ATAN): ...(B *frame)
 
Note: use finction lists provided by DMNUMINC

*/ 

void NEG(B *frame)
{
  W idx;
  
  /* index (slow to fast): type, class (s or a) */ 
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(NEGlist[idx]))(frame);
} 

void ABS(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(ABSlist[idx]))(frame);
} 
 
void SQRT(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(SQRTlist[idx]))(frame);
}          

void EXP(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(EXPlist[idx]))(frame);
} 

void LN(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(LNlist[idx]))(frame);
} 

void LG(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(LGlist[idx]))(frame);
} 

void FLOOR(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(FLOORlist[idx]))(frame);
} 

void CEIL(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(CEILlist[idx]))(frame);
} 

void SIN(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(SINlist[idx]))(frame);
} 

void COS(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(COSlist[idx]))(frame);
} 

void TAN(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(TANlist[idx]))(frame);
} 

void ASIN(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(ASINlist[idx]))(frame);
} 

void ACOS(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(ACOSlist[idx]))(frame);
} 

void ATAN(B *frame)
{
  W idx;
  idx = TYPE(frame) << 1;
  if (CLASS(frame) == ARRAY) idx += 1;
  (*(ATANlist[idx]))(frame);
} 

/*------------------------------------------ DECREMENT
   subtracts 1 from the value of a numeral frame
*/

void DECREMENT(B *frame)
{
  (*(DECRlist[TYPE(frame)]))((B *)frame);
}



