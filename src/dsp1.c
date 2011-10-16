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
   /*================== D Machine 3.0 (dsp1.c) ======================
    - digital signal processing and math operators

          - interpolate
          - extrema
          - integrateOH
          - integrateOHv
          - integrateRS
          - solvetridiag
          - tile
          - ramp
          - extract
          - dilute
          - dilute_add
          - ran1
          - solve_bandmat

*/
#include "dm.h"
#include "dmnum.h"
#include "dsp1f.h"

#include <stdio.h>

/*---------------------------------------------------- interpolate
   src_array log2_expansion dest_array | dest_subarray
 
- any combination of array numeral types is accepted
- log2_expansion can be 1 to 5, corresponding to expansion factors 2-32
- dest_array length should be by the expansion factor larger than
  src_array length (exactly: (src_length - 3) * xf + 1); the returned
  subarray has been adjusted to this length;
- the first and last socket samples are used in the interpolation, but
  are not transferred into the expanded result; the first destination sample
  is identical to the second socket sample, and subsequent destination
  samples are equally spaced at the reduced sampling interval
*/

P op_interpolate(void)
{
  P nd,xf,idx;
  P log2xf;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  if ((CLASS(o_3) != ARRAY) || (CLASS(o_2) != NUM) || (CLASS(o_1) != ARRAY))
    return OPD_CLA;
  if (ARRAY_SIZE(o_3) < 4) return RNG_CHK;
  if (!PVALUE(o_2,&log2xf)) return UNDF_VAL;

  if ((log2xf < 1) || (log2xf > 5)) return RNG_CHK;
  xf = 1<<log2xf;
  if ((nd = (ARRAY_SIZE(o_3) - 3) * xf + 1) > ARRAY_SIZE(o_1)) return RNG_CHK;
  idx = TYPE(o_3) * nTYPES + TYPE(o_1);
  (*(IPlist[idx]))(o_3,o_1,log2xf);
  ARRAY_SIZE(o_1) = nd;
  moveframe(o_1,o_3);
  FREEopds = o_2;
  return OK;
}

/*------------------------------------------------- extrema

   oldmin oldmax array/list | newmin newmax
   
   - accepts any data types and combinations for operands
   - returns results in types of original extrema
*/

P op_extrema(void)
{
  B min[FRAMEBYTES], max[FRAMEBYTES], *f;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if ((CLASS(o_3) != NUM) || (CLASS(o_2) != NUM)) return OPD_CLA;
  TAG(min) = TAG(max) = NUM | DOUBLETYPE; ATTR(min) = ATTR(max) = 0;
  MOVE(o_3,min); 
  MOVE(o_2,max);
  switch(CLASS(o_1)) {
    case ARRAY: 
      (*(EXTRlist[TYPE(o_1)]))(o_1,(D *)NUM_VAL(min), (D *)NUM_VAL(max));
      break;

	case LIST:
    for (f = (B *)VALUE_BASE(o_1);
         f < (B *)LIST_CEIL(o_1);
         f += FRAMEBYTES) { 
      if (CLASS(f) != NUM) return OPD_CLA;
      if (COMPARE(f,min) < 0) MOVE(f,min);
      else if (COMPARE(f,max) > 0) MOVE(f,max);
    }
    break;

    default: 
      return OPD_CLA;
	}

  MOVE(min,o_3); MOVE(max,o_2);
  FREEopds = o_1;
  return OK;
}

/*----------------------------------------------- integrateOH 
   array | array
   
   - takes any type of array and returns results in that type
   - uses the one-half rule of integration
   - the value of 'array' is replaced by twice the running integral
     (no multiplication by the abscissa stepwidth is included)
   - an undefined element results in undefined integral elements
     starting at that index
       
*/

P op_integrateOH(void)
{
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY) return OPD_CLA;
  (*(INTEGRohlist[TYPE(o_1)]))(o_1);
  return OK;
}

/*----------------------------------------------- integrateRS 
   array | array
   
   - takes double array
   - replaces contents by running sum
        
*/

P op_integrateRS(void)
{
  D *a, sum;
  P k, n;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
  if (ATTR(o_1) == READONLY) return OPD_ATR;

  a = (D*)(VALUE_BASE(o_1)); 
  n = ARRAY_SIZE(o_1);
  sum = 0.0; 
  for (k=0; k<n; k++) {
    sum += *a; 
    *(a++) = sum;
  }
  
  return OK;
}

/*----------------------------------------------- integrateOHv 
   y_array dx_array | array
   
   - takes any type of y_array and returns results in that type (the type of
     dx_array has to match y_array)
   - uses the one-half rule of integration, applying it with the abscissa
     increments given in dx_array
   - the value of 'array' is replaced by twice the running integral
   - an undefined element results in undefined integral elements starting at
     that index
       
*/

P op_integrateOHv(void)
{
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != ARRAY) return OPD_CLA;
  if (TAG(o_1) != TAG(o_2)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) != ARRAY_SIZE(o_2)) return RNG_CHK;
  (*(INTEGRohVlist[TYPE(o_1)]))(o_2,o_1);
  FREEopds = o_1;
  return OK;
}

/*----------------------------------------------- solvetridiag
   a b c r u | u bool
   
   - all arguments are arrays of identical types, either S or D
   - a holds the subdiagonal matrix elements (the first element is ignored)
   - b holds the diagonal elements
   - c holds the supradiagonal elements (the last element is ignored)
   - r holds the right-hand sides
   - u receives the solution
   - bool indicates whether system was solvable
*/

DM_INLINE_STATIC BOOLEAN solvetridiagS(B *f)
{
  S *a, *b, *c, *r, *u, bet, *gam; 
  P n, j;

  a = (S *)VALUE_BASE(f); f += FRAMEBYTES;
  b = (S *)VALUE_BASE(f); f += FRAMEBYTES;
  c = (S *)VALUE_BASE(f); f += FRAMEBYTES;
  r = (S *)VALUE_BASE(f); f += FRAMEBYTES;
  u = (S *)VALUE_BASE(f); 
  n = ARRAY_SIZE(f);
  gam = (S *)FREEvm;
  
  if (b[0] == 0.0) return FALSE;
  u[0] = r[0]/(bet = b[0]);
  for (j=1; j<n; j++) { 
    gam[j] = c[j-1]/bet;
    bet = b[j] - a[j]*gam[j];
    if (bet == 0.0) return FALSE;
    u[j] = (r[j] - a[j]*u[j-1])/bet;
  }
  for (j = (n-2); j >=0; j--) u[j] -= gam[j+1] * u[j+1];
  return TRUE;
}

DM_INLINE_STATIC BOOLEAN solvetridiagD(B *f)
{
  D *a, *b, *c, *r, *u, bet, *gam; 
  P n, j;

  a = (D *)VALUE_BASE(f); f += FRAMEBYTES;
  b = (D *)VALUE_BASE(f); f += FRAMEBYTES;
  c = (D *)VALUE_BASE(f); f += FRAMEBYTES;
  r = (D *)VALUE_BASE(f); f += FRAMEBYTES;
  u = (D *)VALUE_BASE(f); 
  n = ARRAY_SIZE(f);
  gam = (D *)FREEvm;

  if (b[0] == 0.0) return FALSE;
  u[0] = r[0]/(bet = b[0]);
  for (j=1; j<n; j++) { 
    gam[j] = c[j-1]/bet;
	  bet = b[j] - a[j]*gam[j];
	  if (bet == 0.0) return FALSE;
	  u[j] = (r[j] - a[j]*u[j-1])/bet;
	}
  for (j = (n-2); j >=0; j--) u[j] -= gam[j+1] * u[j+1];
  return TRUE;
}

P op_solvetridiag(void)
{
  B *f; 
  BOOLEAN bool;

  if (o_5 < FLOORopds) return OPDS_UNF;
  for (f=o_4; f<=o_1; f+=FRAMEBYTES) { 
    if (TAG(f) != TAG(o_5)) return OPD_CLA;
    if (ARRAY_SIZE(f) != ARRAY_SIZE(o_5)) return RNG_CHK;
  }
  if ((FREEvm + ARRAY_SIZE(o_5) * VALUEBYTES(TYPE(o_5))) > CEILvm)
    return VM_OVF;

  switch(TYPE(o_5)) {
    case SINGLETYPE: bool = solvetridiagS(o_5); break;
    case DOUBLETYPE: bool = solvetridiagD(o_5); break;
    default: return OPD_TYP;
  }

  moveframe(o_1,o_5);
  TAG(o_4) = BOOL; ATTR(o_4) = 0; BOOL_VAL(o_4) = bool;
  FREEopds = o_3;
  return OK;
}

/*----------------------------------------------- tile
   d_arr idx count s_array | d_arr idx
   
   - faxes 'count' sequential copies of s_array into d_arr, starting at 'idx'
   - d_arr and the updated idx are returned
   - s_arr and d_arr must match in type, which can be any
*/

static void tile_B(B *sf, B *df, P idx, P count)
{
  B *sp, *dp; 
  P k;
  
  sp = (B *)VALUE_BASE(sf);
  dp = (B *)VALUE_BASE(df) + idx;
  for (k=0; k<count; k++) { 
    moveB(sp,dp,ARRAY_SIZE(sf));
    dp += ARRAY_SIZE(sf);
  }
}

static void tile_W(B *sf, B *df, P idx, P count)
{
  W *sp, *dp; 
  P k;
  sp = (W *)VALUE_BASE(sf);
  dp = (W *)VALUE_BASE(df) + idx;
  for (k=0; k<count; k++) { 
    moveW(sp,dp,ARRAY_SIZE(sf));
    dp += ARRAY_SIZE(sf);
  }
}

static void tile_L32(B *sf, B *df, P idx, P count)
{
  L32 *sp, *dp; 
  P k;
  sp = (L32 *)VALUE_BASE(sf);
  dp = (L32 *)VALUE_BASE(df) + idx;
  for (k=0; k<count; k++) { 
    moveL32(sp,dp,ARRAY_SIZE(sf));
    dp += ARRAY_SIZE(sf);
  }
}

static void tile_L64(B *sf, B *df, P idx, P count)
{
  L64 *sp, *dp; 
  P k;
  sp = (L64 *)VALUE_BASE(sf);
  dp = (L64 *)VALUE_BASE(df) + idx;
  for (k=0; k<count; k++) { 
    moveL64(sp,dp,ARRAY_SIZE(sf));
    dp += ARRAY_SIZE(sf);
  }
}

static void tile_S(B *sf, B *df, P idx, P count)
{
  S *sp, *dp; 
  P k;
  sp = (S *)VALUE_BASE(sf);
  dp = (S *)VALUE_BASE(df) + idx;
  for (k=0; k<count; k++) { 
    moveS(sp,dp,ARRAY_SIZE(sf));
    dp += ARRAY_SIZE(sf);
  }
}

static void tile_D(B *sf, B *df, P idx, P count)
{
  D *sp, *dp; 
  P k;
  sp = (D *)VALUE_BASE(sf);
  dp = (D *)VALUE_BASE(df) + idx;
  for (k=0; k<count; k++) { 
    moveD(sp,dp,ARRAY_SIZE(sf));
    dp += ARRAY_SIZE(sf);
  }
}

typedef void (*tile_fct)(B *sf, B *df, P idx, P count);
static tile_fct tile_list[] = { 
  tile_B, tile_W, tile_L32, tile_L64, tile_S, tile_D 
};

P op_tile(void)
{
  P idx, count;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_4) != ARRAY) return OPD_CLA;
  if (TAG(o_4) != TAG(o_1)) return OPD_ERR;
  if (ATTR(o_4) & READONLY) return OPD_ATR;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (CLASS(o_3) != CLASS(o_2)) return OPD_CLA;
  if (!PVALUE(o_3,&idx)) return UNDF_VAL;
  if (!PVALUE(o_2,&count)) return UNDF_VAL;
  if ((idx < 0) || (count <= 0)) return RNG_CHK;
  if ((idx + count * ARRAY_SIZE(o_1)) > ARRAY_SIZE(o_4)) return RNG_CHK;
  (*(tile_list[TYPE(o_4)]))(o_1,o_4,idx,count);

  TAG(o_3) = NUM | LONGBIGTYPE;
  ATTR(o_3) = 0;
  LONGBIG_VAL(o_3) += count * ARRAY_SIZE(o_1);
  FREEopds = o_2;
  return OK;
}

/*----------------------------------------------- ramp
   d_arr idx count first step | d_arr idx
   
   - builds, starting from 'idx' in 'd_arr', a ramp of 'count' elements, which
     begins from the value 'first' and varies in equal intervals of 'step'
   - d_arr and the updated idx are returned
   - d_arr can be any type
*/

static void ramp_B(B *df, B *ff, B *sf, P idx, P count)
{
  B *dp, first, step; 
  P k;
  dp = (B *)VALUE_BASE(df) + idx;
  first = *NUM_VAL(ff); 
  step = *NUM_VAL(sf);
  for (k=0; k<count; k++) *(dp++) = first + step * k;
}

static void ramp_W(B *df, B *ff, B *sf, P idx, P count)
{
  W *dp, first, step; 
  P k;
  dp = (W *)VALUE_BASE(df) + idx;
  first = *((W *)NUM_VAL(ff)); 
  step = *((W *)NUM_VAL(sf));
  for (k=0; k<count; k++) *(dp++) = first + step * k;
}

static void ramp_L32(B *df, B *ff, B *sf, P idx, P count)
{
  L32 *dp, first, step; 
  P k;
  dp = (L32 *)VALUE_BASE(df) + idx;
  first = *((L32 *)NUM_VAL(ff)); 
  step = *((L32 *)NUM_VAL(sf));
  for (k=0; k<count; k++) *(dp++) = first + step * k;
}

static void ramp_L64(B *df, B *ff, B *sf, P idx, P count)
{
  L64 *dp, first, step; 
  P k;
  dp = (L64 *)VALUE_BASE(df) + idx;
  first = *((L64 *)NUM_VAL(ff)); 
  step = *((L64 *)NUM_VAL(sf));
  for (k=0; k<count; k++) *(dp++) = first + step * k;
}

static void ramp_S(B *df, B *ff, B *sf, P idx, P count)
{
  S *dp, first, step; 
  P k;
  dp = (S *)VALUE_BASE(df) + idx;
  first = *((S *)NUM_VAL(ff)); 
  step = *((S *)NUM_VAL(sf));
  for (k=0; k<count; k++) *(dp++) = first + step * k;
}

static void ramp_D(B *df, B *ff, B *sf, P idx, P count)
{
  D *dp, first, step; 
  P k;
  dp = (D *)VALUE_BASE(df) + idx;
  first = *((D *)NUM_VAL(ff)); 
  step = *((D *)NUM_VAL(sf));
  for (k=0; k<count; k++) *(dp++) = first + step * k;
}

typedef void (*ramp_fct)(B *df, B *ff, B *sf, P idx, P count);
static ramp_fct ramp_list[] =  {
  ramp_B, ramp_W, ramp_L32, ramp_L64, ramp_S, ramp_D 
};

P op_ramp(void)
{
  P idx, count;
  B ff[FRAMEBYTES], sf[FRAMEBYTES];

  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_5) != ARRAY) return OPD_CLA;
  if (ATTR(o_5) & READONLY) return OPD_ATR;
  if (CLASS(o_4) != NUM) return OPD_CLA;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_4,&idx)) return UNDF_VAL;
  if (!PVALUE(o_3,&count)) return UNDF_VAL;
  if ((idx < 0) || (count <= 0)) return RNG_CHK;
  if ((idx + count) > ARRAY_SIZE(o_5)) return RNG_CHK;
  TAG(ff) = TAG(sf) = NUM | TYPE(o_5); 
  ATTR(ff) = ATTR(sf) = 0;
  MOVE(o_2,ff); 
  MOVE(o_1,sf);
  (*(ramp_list[TYPE(o_5)]))(o_5, ff, sf, idx, count);
  TAG(o_4) = NUM | LONGBIGTYPE; 
  ATTR(o_4) = 0;   
  LONGBIG_VAL(o_4) += count;
  FREEopds = o_3;
  return OK;
}

/*----------------------------------------------- extract
   s_arr idx interval d_arr | d_arr
   
   - extracts from 's_arr' elements starting at index 'idx' and spaced at
     'interval' and places them into successive locations of d_arr.
   - the operation ends when the end of s_arr ar d_arr is reached, whichever
     occurs first
   - the returned array is the filled subarray of d_arr
   - s_arr and d_arr are of the same type
 */

static void extract_B(B *sf, B *df, P first, P iv, P count)
{
  B *sp, *dp; 
  P k;
  sp = (B *)VALUE_BASE(sf) + first;
  dp = (B *)VALUE_BASE(df);
  for (k=0; k<count; k++) { *(dp++) = *sp; sp += iv; }
}

static void extract_W(B *sf, B *df, P first, P iv, P count)
{
  W *sp, *dp; 
  P k;
  sp = (W *)VALUE_BASE(sf) + first;
  dp = (W *)VALUE_BASE(df);
  for (k=0; k<count; k++) { *(dp++) = *sp; sp += iv; }
}

static void extract_L32(B *sf, B *df, P first, P iv, P count)
{
  L32 *sp, *dp; 
  P k;
  sp = (L32 *)VALUE_BASE(sf) + first;
  dp = (L32 *)VALUE_BASE(df);
  for (k=0; k<count; k++) { *(dp++) = *sp; sp += iv; }
}

static void extract_L64(B *sf, B *df, P first, P iv, P count)
{
  L64 *sp, *dp; 
  P k;
  sp = (L64 *)VALUE_BASE(sf) + first;
  dp = (L64 *)VALUE_BASE(df);
  for (k=0; k<count; k++) { *(dp++) = *sp; sp += iv; }
}

static void extract_S(B *sf, B *df, P first, P iv, P count)
{
  S *sp, *dp; 
  P k;
  sp = (S *)VALUE_BASE(sf) + first;
  dp = (S *)VALUE_BASE(df);
  for (k=0; k<count; k++) { *(dp++) = *sp; sp += iv; }
}

static void extract_D(B *sf, B *df, P first, P iv, P count)
{
  D *sp, *dp; P k;
  sp = (D *)VALUE_BASE(sf) + first;
  dp = (D *)VALUE_BASE(df);
  for (k=0; k<count; k++) { *(dp++) = *sp; sp += iv; }
}


typedef void (*extract_fct)(B *sf, B *df, P first, P iv, P count);
static extract_fct extract_list[] = { 
  extract_B, extract_W, extract_L32, extract_L64, extract_S, extract_D 
};

P op_extract(void)
{
  P first, iv;
  P count;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_4) != ARRAY) return OPD_CLA;
  if (TAG(o_4) != TAG(o_1)) return OPD_ERR;
  if (ATTR(o_1) & READONLY) return OPD_ATR;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (!PVALUE(o_3,&first)) return UNDF_VAL;
  if (!PVALUE(o_2,&iv)) return UNDF_VAL;
  if ((first < 0) || (iv == 0)) return RNG_CHK;
  count = (iv > 0 ? (ARRAY_SIZE(o_4) - 1 - first) : -first) / iv + 1;
  if (count > ARRAY_SIZE(o_1)) count = ARRAY_SIZE(o_1);
  if (count <= 0) return RNG_CHK;
  (*(extract_list[TYPE(o_4)]))(o_4,o_1,first,iv,count);
  moveframe(o_1,o_4); 
  ARRAY_SIZE(o_4) = count;
  FREEopds = o_3;
  return OK;
}

/*----------------------------------------------- dilute
   s_arr d_arr idx interval | d_arr
   
   - copies successive elements of 's_arr' into 'd_arr', starting at index
     'idx' of 'd_arr' and spaced at 'interval'
   - the operation ends when the end of s_arr ar d_arr is reached, whichever
     occurs first
   - d_arr is returned
   - s_arr and d_arr are of the same type
 */

static void dilute_B(B *sf, B *df, P first, P iv, P count)
{
  B *sp, *dp; 
  P k;
  sp = (B *)VALUE_BASE(sf);
  dp = (B *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp = *(sp++); dp += iv; }
}

static void dilute_W(B *sf, B *df, P first, P iv, P count)
{
  W *sp, *dp; 
  P k;
  sp = (W *)VALUE_BASE(sf);
  dp = (W *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp = *(sp++); dp += iv; }
}

static void dilute_L32(B *sf, B *df, P first, P iv, P count)
{
  L32 *sp, *dp; 
  P k;
  sp = (L32 *)VALUE_BASE(sf);
  dp = (L32 *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp = *(sp++); dp += iv; }
}

static void dilute_L64(B *sf, B *df, P first, P iv, P count)
{
  L64 *sp, *dp; 
  P k;
  sp = (L64 *)VALUE_BASE(sf);
  dp = (L64 *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp = *(sp++); dp += iv; }
}

static void dilute_S(B *sf, B *df, P first, P iv, P count)
{
  S *sp, *dp; 
  P k;
  sp = (S *)VALUE_BASE(sf);
  dp = (S *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp = *(sp++); dp += iv; }
}

static void dilute_D(B *sf, B *df, P first, P iv, P count)
{
  D *sp, *dp; 
  P k;
  sp = (D *)VALUE_BASE(sf);
  dp = (D *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp = *(sp++); dp += iv; }
}

typedef void (*dilute_fct)(B *sf, B *df, P first, P iv, P count);
static dilute_fct dilute_list[] = { 
  dilute_B, dilute_W, dilute_L32, dilute_L64, dilute_S, dilute_D 
};

P op_dilute(void)
{
  P first, iv;
  P count;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_4) != ARRAY) return OPD_CLA;
  if (TAG(o_4) != TAG(o_3)) return OPD_ERR;
  if (ATTR(o_3) & READONLY) return OPD_ATR;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_2,&first)) return UNDF_VAL;
  if (!PVALUE(o_1,&iv)) return UNDF_VAL;
  if ((first < 0) || (iv <= 0)) return RNG_CHK;
  count = (ARRAY_SIZE(o_3) - 1 - first) / iv + 1;
  if (count > ARRAY_SIZE(o_4)) count = ARRAY_SIZE(o_4);
  if (count <= 0) return RNG_CHK;
  (*(dilute_list[TYPE(o_4)]))(o_4,o_3,first,iv,count);
  moveframe(o_3,o_4);
  FREEopds = o_3;
  return OK;
}

/*----------------------------------------------- dilute_add
   s_arr d_arr idx interval | d_arr
   
   - adds successive elements of 's_arr' into 'd_arr', starting at index 'idx'
     of 'd_arr' and spaced at 'interval'
   - the operation ends when the end of s_arr ar d_arr is reached, whichever
     occurs first
   - d_arr is returned
   - s_arr and d_arr are of the same type
 */

static void dilute_add_B(B *sf, B *df, P first, P iv, P count)
{
  B *sp, *dp; 
  P k;
  sp = (B *)VALUE_BASE(sf);
  dp = (B *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp += *(sp++); dp += iv; }
}

static void dilute_add_W(B *sf, B *df, P first, P iv, P count)
{
  W *sp, *dp; 
  P k;
  sp = (W *)VALUE_BASE(sf);
  dp = (W *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp += *(sp++); dp += iv; }
}

static void dilute_add_L32(B *sf, B *df, P first, P iv, P count)
{
  L32 *sp, *dp; 
  P k;
  sp = (L32 *)VALUE_BASE(sf);
  dp = (L32 *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp += *(sp++); dp += iv; }
}

static void dilute_add_L64(B *sf, B *df, P first, P iv, P count)
{
  L64 *sp, *dp; 
  P k;
  sp = (L64 *)VALUE_BASE(sf);
  dp = (L64 *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp += *(sp++); dp += iv; }
}

static void dilute_add_S(B *sf, B *df, P first, P iv, P count)
{
  S *sp, *dp; 
  P k;
  sp = (S *)VALUE_BASE(sf);
  dp = (S *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp += *(sp++); dp += iv; }
}

static void dilute_add_D(B *sf, B *df, P first, P iv, P count)
{
  D *sp, *dp; 
  P k;
  sp = (D *)VALUE_BASE(sf);
  dp = (D *)VALUE_BASE(df) + first;
  for (k=0; k<count; k++) { *dp += *(sp++); dp += iv; }
}

/* typedef void (*dilute_fct)(B *sf, B *df, P first, P iv, P count); */
static dilute_fct dilute_add_list[] = { 
  dilute_add_B, dilute_add_W, 
  dilute_add_L32, dilute_add_L64, 
  dilute_add_S, dilute_add_D 
};

P op_dilute_add(void)
{
  P first, iv;
  P count;

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_4) != ARRAY) return OPD_CLA;
  if (TAG(o_4) != TAG(o_3)) return OPD_ERR;
  if (ATTR(o_3) & READONLY) return OPD_ATR;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_2,&first)) return UNDF_VAL;
  if (!PVALUE(o_1,&iv)) return UNDF_VAL;
  if ((first < 0) || (iv <= 0)) return RNG_CHK;
  count = (ARRAY_SIZE(o_3) - 1 - first) / iv + 1;
  if (count > ARRAY_SIZE(o_4)) count = ARRAY_SIZE(o_4);
  if (count <= 0) return RNG_CHK;
  (*(dilute_add_list[TYPE(o_4)]))(o_4,o_3,first,iv,count);
  moveframe(o_3,o_4);
  FREEopds = o_3;
  return OK;
}

/*----------------------------------------------- ran1 
   array bool | array
   
   - takes S array
   - the boolean 'true' initializes the random number generator
   - the returned array contains random numbers in the range 0 to 1
     excluding the exact boundaries
   - this is the ran1 routine of Press et al. (p.280)
     
*/

#define   IA  16807
#define   IM  2147483647
#define   AM  (1.0/IM)
#define   IQ  127773
#define   IR  2836
#define   NTAB 32
#define   NDIV  (1+(IM-1)/NTAB)
#define   EPS 1.2e-7
#define   RNMX  (1.0 - EPS)

P op_ran1(void)
{
  static L32 idum = -17, iv[NTAB], iy = 0;
  L32 k, j;
  P idx;
  S temp, *dp;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (TAG(o_2) != (ARRAY | SINGLETYPE)) return OPD_CLA;
  if (CLASS(o_1) != BOOL) return OPD_CLA;
  if (ATTR(o_2) & READONLY) return OPD_ATR;
  if (BOOL_VAL(o_1)) idum = -17;
  dp = (S *)VALUE_BASE(o_2);
  for (idx=0; idx<ARRAY_SIZE(o_2); idx++) { 
    if ((idum <= 0) || (!iy)) { 
      if ((-idum) < 1) idum = 1; else idum = -idum;
      for (j=NTAB+7; j>=0; j--) { 
        k = idum/IQ; idum = IA * (idum - k * IQ) - IR * k;
        if (idum < 0) idum += IM;
        if (j < NTAB) iv[j] = idum;
      }
      iy = iv[0];
    }
    k = idum / IQ;
    idum = IA * (idum - k * IQ) - IR * k;
    if (idum < 0) idum += IM;
    j = iy / NDIV;
    iy = iv[j];
    iv[j] = idum;
    if ((temp = AM * iy) > RNMX) temp = RNMX;
    *(dp++) = temp;
  }

  FREEopds = o_1;
  return OK;
}

/*----------------------------------------------- solv_bandmat
   bandlist bandrh main_idx | bool

   - solves a linear equation system whose coefficients are a bandmatrix
     (based on 'bandet1' and 'bandsol1' of Wilkinson/Reinsch I/6)
   - bandlist contains the coefficients as band arrays that run parallel
     to the main diagonal and are ordered from bottom to top; the index of
     the main diagonal array in bandlist is given by main_idx.
   - the first elements of all bandlist arrays belong to the first row of
     the band matrix; elements actually faliing outside that matrix have to
     be submitted as zero.
   - bandrh submits the right-hand sides of the linear equations;
   - the solutions of the linear equations overwrite bandrh;
   - the returned boolean indicates successful solution of the system
   - NOTE: all arrays must be of the same dimensions and of 'double' type
   
 */

P op_solve_bandmat(void)
{
  L32 nband, na, k, nb, m2, i, j, l, w;
  L32 m1;
  L32 *iarr;
  D **a, *b, **m, x, *free;
  B *fa;
  BOOLEAN good; 

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_3) != LIST) return OPD_CLA;
  if (TAG(o_2) != (ARRAY | DOUBLETYPE)) return OPD_TYP;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!L32VALUE(o_1, &m1)) return UNDF_VAL;
  if (m1 < 0) return RNG_CHK;
  if (ATTR(o_2) & READONLY) return OPD_ATR;
  na = ARRAY_SIZE(o_2);
  b = ((D *)VALUE_BASE(o_2)) - 1;
  fa = (B *)VALUE_BASE(o_3); nband = 0;
  while (fa < (B *)LIST_CEIL(o_3)) { 
    if (TAG(fa) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ATTR(fa) & READONLY) return OPD_ATR;
    if (ARRAY_SIZE(fa) != na) return RNG_CHK;
    nband++; fa += FRAMEBYTES;
  }

  /* reserve work memory in VM */   
  if (nband <= m1) return RNG_CHK;
  nb = DALIGN((nband + m1) * sizeof(D *)) 
    + m1 * na * sizeof(D) + na * sizeof(L32);
  if ((FREEvm + nb) > CEILvm) return VM_OVF;
  a = (D **)FREEvm; fa = (B *)VALUE_BASE(o_3);
  for (k=0; k<nband; k++) { 
    a[k] = ((D *)(VALUE_BASE(fa))) - 1; 
    fa += FRAMEBYTES;
  }
  m = a + nband; free = (D *)DALIGN(m + m1);
  for (k=0; k<m1; k++) { 
    m[k] = free - 1; 
    free += na; 
  }
  iarr = ((L32 *)free) - 1;
  a += m1; m--; m2 = nband - m1 - 1;
  
  /* banddet1 */
  l = m1;
  for (i=1; i<=m1; i++) { 
    for (j=1-i; j<=m2; j++) 
      (a[j-l])[i] = (a[j])[i];
    l--; 
    for (j=m2-l; j<=m2; j++) (a[j])[i] = 0.0;
  }
  l = m1;
  for (k=1; k<=na; k++) { 
    x = (a[-m1])[k]; i = k;
    if (l < na) l++;
    for (j=k+1; j<=l; j++) { 
      if (fabs((a[-m1])[j]) > fabs(x)) { 
        x = (a[-m1])[j]; i = j;
      }
    }
    iarr[k] = i;
    if (x == 0.0) { good = FALSE; goto bandmatdone; }
    if (i != k) { 
      for (j=-m1; j<=m2; j++) { 
        x = (a[j])[k]; (a[j])[k] = (a[j])[i]; (a[j])[i] = x;
      }
    }
    for (i=k+1; i<=l; i++) { 
      x = (m[i-k])[k] = (a[-m1])[i] / (a[-m1])[k];
      for (j=1-m1; j<=m2; j++) (a[j-1])[i] = (a[j])[i] - x * (a[j])[k];
      (a[m2])[i] = 0;
    }
  } 
   
  /* bandsol1 */

  l = m1;
  for (k=1; k<=na; k++) { 
    i = iarr[k];
    if (i != k) { 
      x = b[k]; 
      b[k] = b[i]; 
      b[i] = x; 
    }
    if (l < na) l++;
    for (i=k+1; i<=l; i++) { x = (m[i-k])[k]; b[i] -= x * b[k]; } 
  }

  l = -m1;
  for (i=na; i>=1; i--) {
    x = b[i]; w = i + m1; 
    for (k=1-m1; k<=l; k++) x -= (a[k])[i] * b[k+w];
    b[i] = x / (a[-m1])[i];
    if (l < m2) l++;
  }
  good = TRUE;

 bandmatdone:
  TAG(o_3) = BOOL; 
  ATTR(o_3) = 0; 
  BOOL_VAL(o_3) = good;
  FREEopds = o_2; 
  return OK;
}

/*----------------------------------------------- bandLU

   bandlist lowerlist main_idx indices
          | bandlist lowerlist main_idx indices bool

   - factorizes bandmatrix - lambda * I
     (based on 'bandet1' of Wilkinson/Reinsch I/6, bandec of Press et al., 2.4)
   - bandlist contains the coefficients as band arrays that run parallel
     to the main diagonal and are ordered from bottom to top; the index of
     the main diagonal array in bandlist is given by main_idx.
   - the first elements of all bandlist arrays belong to the first row of
     the band matrix; elements actually faliing outside that matrix have to
     be submitted as zero.
   - the returned boolean indicates successful factorization
   - bandlist receives the upper triangular factor, U, and lowerlist the lower
     triangular factor, L, of the LU decomposition of bandlist
   - indices receives indexing information regarding pivoting in the
     decomposition
   - NOTE: all arrays must be of the same dimensions and of 'double' type
   - bandlist, lowerlist, indices are needed for bandBS
   
 */

P op_bandLU(void)
{
  L32 nband, na, k, nb, m2, i, j, l;
  L32 m1;
  L32 *iarr;
  D **a, **m, x, *free;
  B *fa;
  BOOLEAN good; 

  if (o_4 < FLOORopds) return OPDS_UNF;
  if (o2 > CEILopds) return OPDS_OVF;
  if (CLASS(o_4) != LIST) return OPD_CLA;
  if (CLASS(o_3) != LIST) return OPD_CLA;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (TAG(o_1) != (ARRAY | LONG32TYPE)) return OPD_TYP;

  if ((ATTR(o_4) & READONLY) 
      || (ATTR(o_3) & READONLY) 
      || (ATTR(o_1) & READONLY))
    return OPD_ATR;

  if (!L32VALUE(o_2,&m1)) return UNDF_VAL;
  if (m1 < 0) return RNG_CHK;

  na = 0;
  fa = (B *)VALUE_BASE(o_4); nband = 0;
  while (fa < (B *)LIST_CEIL(o_4)) { 
    if (TAG(fa) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ATTR(fa) & READONLY) return OPD_ATR;
    if (na != 0) { 
      if (ARRAY_SIZE(fa) != na) return RNG_CHK; }
    else na = ARRAY_SIZE(fa);
    nband++; fa += FRAMEBYTES;
  }
  fa = (B *)VALUE_BASE(o_3); 
  k = 0;
  while (fa < (B *)LIST_CEIL(o_3)) { 
    if (TAG(fa) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ATTR(fa) & READONLY) return OPD_ATR;
    if (ARRAY_SIZE(fa) != na) return RNG_CHK;
    k++; fa += FRAMEBYTES;
  }
  if (k != m1) return RNG_CHK;
  if (nband <= m1) return RNG_CHK;
  if (ARRAY_SIZE(o_1) != na) return RNG_CHK;

  /* reserve work memory in VM */   

  nb = (nband + m1) * sizeof(D *);
  if ((FREEvm + nb) > CEILvm) return VM_OVF;

  a = (D **)FREEvm; fa = (B *)VALUE_BASE(o_4);
  for (k=0; k<nband; k++) { 
    a[k] = ((D *)(VALUE_BASE(fa))) - 1; fa += FRAMEBYTES;
  }
  m = a + nband; 
  free = (D *)(m + m1); 
  fa = (B *)VALUE_BASE(o_3);
  for (k=0; k<m1; k++) { 
    m[k] = ((D *)(VALUE_BASE(fa))) - 1; 
    fa += FRAMEBYTES;
  }
  iarr = (L32*)VALUE_BASE(o_1) - 1;
  
  a += m1; m--; m2 = nband - m1 - 1;

/* banddet1 */
  l = m1;
  for (i=1; i<=m1; i++) { 
    for (j=1-i; j<=m2; j++) (a[j-l])[i] = (a[j])[i];
    l--; 
    for (j=m2-l; j<=m2; j++) (a[j])[i] = 0.0;
  }
  l = m1;
  for (k=1; k<=na; k++) { 
    x = (a[-m1])[k]; i = k;
    if (l < na) l++;
    for (j=k+1; j<=l; j++) { 
      if (fabs((a[-m1])[j]) > fabs(x)) { 
        x = (a[-m1])[j]; 
        i = j;
      }
    }
    iarr[k] = i;
    if (x == 0.0) { good = FALSE; goto bandmatdone; }
    if (i != k) { 
      for (j=-m1; j<=m2; j++) { 
        x = (a[j])[k]; (a[j])[k] = (a[j])[i]; (a[j])[i] = x;
      }
    }
    for (i=k+1; i<=l; i++) { 
      x = (m[i-k])[k] = (a[-m1])[i] / (a[-m1])[k];
      for (j=1-m1; j<=m2; j++) (a[j-1])[i] = (a[j])[i] - x * (a[j])[k];
      (a[m2])[i] = 0;
    }
  }
  good = TRUE;

 bandmatdone:
  TAG(o1) = BOOL; 
  ATTR(o1) = 0; 
  BOOL_VAL(o1) = good;
  FREEopds = o2; 
  return OK;
}

/*----------------------------------------------- bandBS

   bandlist lowerlist main_idx indices b | b 
   
- bandlist, lowerlist, main_idx, indices are the same arguments as in bandLU
- the double array b provides the right-hand sides of the system to be solved
  and receives the solution
   
This is the procedure bandsol1.

*/

P op_bandBS(void)
{
  L32 nband, na, k, nb, m1, m2, i, l, w;
  L32 *iarr;
  D **a, *b, **m, x, *free;
  B *fa;

  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_5) != LIST) return OPD_CLA;
  if (CLASS(o_4) != LIST) return OPD_CLA;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (TAG(o_2) != (ARRAY | LONG32TYPE)) return OPD_TYP;
  if (TAG(o_1) != (ARRAY | DOUBLETYPE)) return OPD_TYP;

  if ((ATTR(o_1) & READONLY) 
      || (ATTR(o_1) & READONLY) 
      || (ATTR(o_2) & READONLY))
    return OPD_ATR;

  if (!L32VALUE(o_3,&m1)) return UNDF_VAL;
  if (m1 < 0) return RNG_CHK;

  na = 0;
  fa = (B *)VALUE_BASE(o_5); 
  nband = 0;
  while (fa < (B *)LIST_CEIL(o_5)) { 
    if (TAG(fa) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ATTR(fa) & READONLY) return OPD_ATR;
    if (na != 0)  { 
      if (ARRAY_SIZE(fa) != na) return RNG_CHK; 
    }
    else na = ARRAY_SIZE(fa);
    nband++; fa += FRAMEBYTES;
  }
  fa = (B *)VALUE_BASE(o_4); 
  k = 0;
  while (fa < (B *)LIST_CEIL(o_4)) { 
    if (TAG(fa) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ATTR(fa) & READONLY) return OPD_ATR;
    if (ARRAY_SIZE(fa) != na) return RNG_CHK;
    k++; 
    fa += FRAMEBYTES;
  }
  if (k != m1) return RNG_CHK;
  if (nband <= m1) return RNG_CHK;
  if (ARRAY_SIZE(o_2) != na) return RNG_CHK;
  if (ARRAY_SIZE(o_1) != na) return RNG_CHK;

  /* reserve work memory in VM */   

  nb = (nband + m1) * sizeof(D *);
  if ((FREEvm + nb) > CEILvm) return VM_OVF;

  a = (D **)FREEvm; 
  fa = (B *)VALUE_BASE(o_5);
  for (k=0; k<nband; k++) { 
    a[k] = ((D *)(VALUE_BASE(fa))) - 1; 
    fa += FRAMEBYTES;
  }
  m = a + nband; 
  free = (D *)(m + m1); 
  fa = (B *)VALUE_BASE(o_4);
  for (k=0; k<m1; k++) { 
    m[k] = ((D *)(VALUE_BASE(fa))) - 1; 
    fa += FRAMEBYTES;
  }
  iarr = (L32 *)VALUE_BASE(o_2) - 1;
  b = (D *)VALUE_BASE(o_1) - 1;

  a += m1; m--; m2 = nband - m1 - 1;

  /* bandsol */
  
  l = m1;
  for (k=1; k<=na; k++) { 
    i = iarr[k];
    if (i != k) { 
      x = b[k]; b[k] = b[i]; b[i] = x; 
    }
    if (l < na) l++;
    for (i=k+1; i<=l; i++) { 
      x = (m[i-k])[k]; 
      b[i] -= x * b[k]; 
    } 
  }

  l = -m1;
  for (i=na; i>=1; i--) { 
    x = b[i]; 
    w = i + m1; 
    for (k=1-m1; k<=l; k++) 
      x -= (a[k])[i] * b[k+w];
    b[i] = x / (a[-m1])[i];
    if (l < m2) l++;
  }

  moveframe(o_1,o_5);
  FREEopds = o_4;
  return OK;
}
