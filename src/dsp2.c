/*===================== D Machine 3.0 (dsp2.c) =============================

    - digital signal processing and math operators
    
          - complexFFT
          - realFFT
          - sineFFT
          
          - decompLU
          - backsubLU
          - invertLU
          
          - matmul
          - mattranspose
          - matvecmul

 

*/
#include "dm.h"
#include "threads.h"
#include "dmnum.h"

#include <math.h>
#include <inttypes.h>
#include <stdio.h>

#include "dsp2def.h"

/*------------------------------ internal library ---------------------------*/

/*--------------------------------------- FFT: complex-> complex
  (Press et al, page 507)
  data  - on input: alternating real and imaginary parts
        _ on return: alternating real and imaginary parts of transform,
          starting at 0 freq
  nn    - # of complex points in data (must be power of 2)
  dir   - 1: forward transform
          -1: inverse transform (note: results NOT divided by nn)
  NOTE: call with 'data' pointing one below the first element
*/

#define SWAP(a,b) do {tempr = (a); (a) = (b); (b) = tempr;} while (0)
#define TwoPi 6.28318530717959
#define Pi 3.141592653589793

DM_INLINE_STATIC void four1(D *data, L32 nn, LBIG dir)
{
  L32 n, mmax, m, j, istep, i;
  D wtemp, wr, wpr, wpi, wi, theta, tempr, tempi;
   
  n = nn << 1;
  j = 1;
  for (i=1; i<n; i+=2) {
    if (j>i) {
      SWAP(data[j],data[i]); 
      SWAP(data[j+1],data[i+1]);
    }
    m = n>>1;
    while (m>=2 && j>m) { 
      j -= m; 
      m >>= 1; 
    }
    j += m;
  }
  
  mmax = 2;
  while (n>mmax) {
    istep = mmax<<1;
    theta = dir * (TwoPi/mmax);
    wtemp = sin(0.5 * theta);
    wpr = -2.0 * wtemp * wtemp;
    wpi = sin(theta);
    wr = 1.0;
    wi = 0.0;
    for (m=1; m<mmax; m+=2) { 
      for (i=m; i<=n; i+=istep) { 
        j = i + mmax;
        tempr = wr * data[j] - wi * data[j+1];
        tempi = wr * data[j+1] + wi * data[j];
        data[j] = data[i] - tempr;
        data[j+1] = data[i+1] - tempi;
        data[i] += tempr;
        data[i+1] += tempi;
      }
      wr = (wtemp=wr) * wpr - wi * wpi + wr;
      wi = wi * wpr + wtemp * wpi + wi;
    }
    mmax = istep;
  }
}

/*------------------------------------ FFT: one real -> complex
  (Press et al., page 513)
  data  - on input: real parts
        _ on return: element 0 and 1 are the real first and last spectrum
          points; the other elements contain alternating real and imaginary
          parts
  n     - # of real points in data (must be power of 2)
  dir   - 1: forward transform
          -1: inverse transform (note: results NOT divided by n/2)
  NOTE: call with 'data' pointing one below the first element
*/

DM_INLINE_STATIC void realft(D *data, L32 n, LBIG dir)
{
  L32 i, i1, i2, i3, i4, np3;
  D c1, c2, h1r, h1i, h2r, h2i, wr, wi, wpr, wpi, wtemp, theta;
   
  c1 = 0.5;
  theta = Pi / (D)(n>>1);
  if (dir == 1) { 
    c2 = -0.5;
    four1(data,n>>1,1);
  } 
  else { 
    c2 = 0.5;
    theta = -theta;
  }
  wtemp = sin(0.5 * theta);
  wpr = -2.0 * wtemp * wtemp;
  wpi = sin(theta);
  wr = 1.0 + wpr;
  wi = wpi;
  np3 = n + 3;
  for (i=2; i<=(n>>2); i++) { 
    i4 = 1 + ( i3 = np3 - ( i2 = 1 + (i1 = i + i - 1)));
    h1r = c1 * (data[i1] + data[i3]);
    h1i = c1 * (data[i2] - data[i4]);
    h2r = -c2 * (data[i2] + data[i4]);
    h2i = c2 * (data[i1] - data[i3]);
    data[i1] = h1r + wr * h2r - wi * h2i;
    data[i2] = h1i + wr * h2i + wi * h2r;
    data[i3] = h1r - wr * h2r + wi * h2i;
    data[i4] = -h1i + wr * h2i + wi * h2r;
    wr = (wtemp = wr) * wpr - wi * wpi + wr;
    wi = wi * wpr + wtemp * wpi + wi;
  }
  if (dir == 1) { 
    data[1] = (h1r = data[1]) + data[2];
    data[2] = h1r - data[2];
  } 
  else {
    data[1] = c1 * ((h1r = data[1]) + data[2]);
    data[2] = c1 * (h1r - data[2]);
    four1(data, n>>1, -1);
  }
}

/*---------------------------------------- FFT: sine
  (Press et al., page 517)
- data is a double array of length N where N is a power of 2
- on input, data contains the real data to be forward-transformed or
  sine-spectrum to be backtransformed
- on return data contains the real amplitudes of the sine spectrum or
  the real data
- this function effects both forward and inverse transforms, but
  NOTE: the inverse transform does NOT divide the result by N/2
*/  


DM_INLINE_STATIC void sinft(D *y, L32 n)
{
  L32 j, n2;
  D sum, y1, y2, theta, wi, wr, wpi, wpr, wtemp;
   
  n2 = n + 2;
  wi = 0.0;
  wr = 1.0;
  theta = Pi/ (D)n;
  wtemp = sin(0.5 * theta);
  wpr = -2.0 * wtemp * wtemp;
  wpi = sin(theta);
  y[1] = 0.0;
  for (j=2; j<=(n>>1)+1; j++) { 
    wr = (wtemp = wr) * wpr - wi * wpi + wr;
    wi = wi * wpr + wtemp * wpi + wi;
    y1 = wi * (y[j] + y[n2-j]);
    y2 = 0.5 * (y[j] - y[n2-j]);
    y[j] = y1 + y2;
    y[n2-j] = y1 - y2;
  }
  realft(y,n,1);
  y[1] *= 0.5;
  sum = y[2] = 0.0;
  for (j=1; j<=n-1; j+=2) {
    sum += y[j];
    y[j] = y[j+1];
    y[j+1] = sum;
  }
}

/*------------------------------------------------------ LU decomposition
  (Press et al., page 46
*/

DM_INLINE_STATIC BOOLEAN ludcmp(D **a, L32 n, L32 *indx, D *d, D *vv)
{
  L32 i, imax = 0, j, k;
  D big, dum, sum, temp, TINY;
  TINY = 1e-20;
   
  *d = 1.0;
  for (i=1; i<=n; i++) { 
    big = 0.0;
    for (j=1; j<=n; j++)  if ((temp = fabs(a[i][j])) > big) big = temp;
    if (big == 0.0) return FALSE;
    vv[i] = 1.0 / big;
  }

  for (j=1; j<=n; j++) { 
    for (i=1; i<j; i++) { 
      sum = a[i][j];
      for (k=1; k<i; k++) sum -= a[i][k] * a[k][j];
      a[i][j] = sum;
    }

    big = 0.0;
    for (i=j; i<=n; i++) { 
      sum = a[i][j];
      for (k=1; k<j; k++) sum -= a[i][k] * a[k][j];
      a[i][j] = sum;
      if ((dum = vv[i] * fabs(sum)) >= big) { 
        big = dum;
        imax = i;
      }
    }
    if (j != imax) {
      for (k=1; k<=n; k++) { 
        dum = a[imax][k];
        a[imax][k] = a[j][k];
        a[j][k] = dum;
      }
      *d = -(*d);
      vv[imax] = vv[j];
    }
    indx[j] = imax;
    if (a[j][j] == 0.0) a[j][j] = TINY;
    if (j != n) {
      dum = 1.0 / a[j][j];
      for (i=j+1; i<=n; i++) a[i][j] *= dum;
    }
  }

  return TRUE;
}


/*------------------------------------------------------ LU backsubstitution
  (Press et al., page 47)
*/

DM_INLINE_STATIC void lubksb(D **a, L32 n, L32 *indx, D *b)
{
   L32 i, ii, ip, j;
   D sum;

   ii = 0;
   
   for (i=1; i<=n; i++) { 
     ip = indx[i];
     sum = b[ip];
     b[ip] = b[i];
     if (ii) for (j=ii; j<=i-1; j++) sum -= a[i][j] * b[j];
     else if (sum) ii=i;
     b[i] = sum;
   }
  
   for (i=n; i>=1; i--) {
     sum = b[i];
     for (j=i+1; j<=n; j++) sum -= a[i][j] * b[j];
     b[i] = sum / a[i][i];
   }
}
                                                                                
/*---------------------------------------------------- complexFFT

   data dir | data
   
- data is a double array of length 2N where N is a power of 2
- on input, data contains the complex (alternating real, imaginary parts) of
  the data to be transformed
- on return data contains the results of the transform (alternating real,
  imaginary parts, in order)
- dir controls the transform: 1 - forward; -1 - inverse
- two successive transforms give the original data
*/

P op_complexFFT(void)
{
  L32 N2, logN2, j;
  LBIG dir;
  D f, *data;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((TAG(o_2) != (ARRAY | DOUBLETYPE)) || (CLASS(o_1) != NUM))
    return OPD_CLA;
  N2 = ARRAY_SIZE(o_2);
  logN2 = 0; 
  while ((N2 >>= 1) > 0) logN2 += 1;

  if ((N2 = ARRAY_SIZE(o_2)) != (1 << logN2)) return RNG_CHK;
  if (!VALUE(o_1,&dir)) return UNDF_VAL;
  if (imaxabs(dir) != 1) return RNG_CHK;

  four1((data = ((D*) VALUE_PTR(o_2))) - 1, N2>>1, dir);
  if (dir == -1) {
    f = 2.0 / N2;
    for (j=0; j<N2; j++) data[j] *= f;
  }

  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- realFFT

   data dir | data
   
- data is a double array of length N where N is a power of 2
- on input, data contains the real data to be forward-transformed or the
  complex spectrum (in format described under 'return') for the backtransform
- on return' data contains the results of the forward transform (real parts of
  first and last spectral point, then alternating real, imaginary parts; only
  the positive half of the spectrum is returned because of symmetry) or the
  real results of the backtransform)
- dir controls the transform: 1 - forward; -1 - inverse
- successive forward and inverse transforms yield the original data
*/

P op_realFFT(void)
{
  L32 N, logN, j;
  LBIG dir;
  D f, *data;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((TAG(o_2) != (ARRAY | DOUBLETYPE)) || (CLASS(o_1) != NUM))
    return OPD_CLA;
  N = ARRAY_SIZE(o_2);
  logN = 0; 
  while ((N >>= 1) > 0) logN += 1;
  if ((N = ARRAY_SIZE(o_2)) != (1 << logN)) return RNG_CHK;
  if (! VALUE(o_1,&dir)) return UNDF_VAL;
  if (imaxabs(dir) != 1) return RNG_CHK;

  realft((data = ((D*) VALUE_PTR(o_2))) - 1, N, dir);
  if (dir == -1) {
    f = 2.0 / N;
    for (j=0; j<N; j++) data[j] *= f;
  }

  FREEopds = o_1;
  return OK;
}

/*---------------------------------------------------- sineFFT

   data dir | data
   
- data is a double array of length N where N is a power of 2
- on input, data contains the real data to be forward-transformed or
  sine-spectrum to be backtransformed
- on return data contains the real amplitudes of the sine spectrum or
  the real data
- dir defines the transform: 1 - forward, -1 - inverse
*/

P op_sineFFT(void)
{
  L32 N, logN, j;
  LBIG dir;
  D f, *data;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((TAG(o_2) != (ARRAY | DOUBLETYPE)) || (CLASS(o_1) != NUM))
    return (OPD_CLA); 
  N = ARRAY_SIZE(o_2);
  logN = 0; 
  while ((N >>= 1) > 0) logN += 1;
  if ((N = ARRAY_SIZE(o_2)) != (1 << logN)) return RNG_CHK;
  if (!VALUE(o_1, &dir)) return UNDF_VAL;
  if (imaxabs(dir) != 1) return RNG_CHK;

  sinft((data = ((D*) VALUE_PTR(o_2))) - 1, N);
  if (dir == -1) {
    f = 2.0 / N;
    for (j=0; j<N; j++) data[j] *= f;
  }

  FREEopds = o_1;
  return OK;
}

/*-------------------------------------------------------- decompLU

   a_list idx_array | d true
                    | false

- a_list is a list of double row arrays containing
  on input: the matrix to be decomposed
  on return: the decomposed matrix
- idx_array is long and receives row permutation info for backsubLU
- d signals even/odd number of permutations for backsubLU
- returns false upon detecting singular matrix
*/

P op_decompLU(void)
{
  L32 N, k;
  L32 *idxp;
  B *fp;
  D **ap, *vec, d;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != LIST) return OPD_CLA;
  N = (LIST_CEIL(o_2) - VALUE_BASE(o_2)) / FRAMEBYTES;
  if (N<=0) return RNG_CHK;
  if (FREEvm + DALIGN(N * sizeof(D *)) + N * sizeof(D) > CEILvm) 
    return VM_OVF;
  
  ap = (D **)FREEvm; 
  fp = (B *)VALUE_BASE(o_2);
  for (k=0; k<N; k++) { 
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ARRAY_SIZE(fp) != N) return RNG_CHK;
    *(ap++) = ((D *)VALUE_BASE(fp)) - 1;
    fp += FRAMEBYTES;
  }
  vec = (D *)DALIGN(ap);
  if (TAG(o_1) != (ARRAY | LONG32TYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) != N) return RNG_CHK;
  idxp = (L32 *)VALUE_BASE(o_1);

  if (ludcmp(((D **)FREEvm)-1, N, idxp-1, &d, vec-1)) { 
    TAG(o_2) = NUM | DOUBLETYPE;
    *((D *)(NUM_VAL(o_2))) = d;
    TAG(o_1) = BOOL; BOOL_VAL(o_1) = TRUE;
  } 
  else {
    TAG(o_2) = BOOL; BOOL_VAL(o_2) = FALSE;
    FREEopds = o_1;
  }
  return OK;
}

/*-------------------------------------------------------- backsubLU

   a_list idx_array b_array | b_array

- a_list is a list of double row arrays containing a previously 
   LU-decomposed matrix
- idx_array is long and provides the permutation info of the previous 
   decomposition
- b inputs the right-hand side for which the eqn system is to be solved 
   and returns the solution

*/

P op_backsubLU(void)
{
  L32 N, k;
  L32 *idxp;
  B *fp;
  D **ap, *bp;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if ( CLASS(o_3) != LIST) return OPD_CLA;
  N = (LIST_CEIL(o_3) - VALUE_BASE(o_3)) / FRAMEBYTES;
  if (N<=0) return RNG_CHK;
  if (FREEvm + N * sizeof(D *) > CEILvm) return VM_OVF;

  ap = (D **)FREEvm; fp = (B *)VALUE_BASE(o_3);
  for (k=0; k<N; k++) { 
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ARRAY_SIZE(fp) != N) return RNG_CHK;
    *(ap++) = ((D *)VALUE_BASE(fp)) - 1;
    fp += FRAMEBYTES;
  }
  if (TAG(o_2) != (ARRAY | LONG32TYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_2) != N) return RNG_CHK;
  idxp = (L32 *)VALUE_BASE(o_2);
  if (TAG(o_1) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) != N) return RNG_CHK;
  bp = (D *)VALUE_BASE(o_1);

  lubksb(((D **)FREEvm)-1, N, idxp-1, bp-1);
  moveframe(o_1,o_3);
  FREEopds = o_2;
  return OK;
}

/*-------------------------------------------------------- invertLU

   a_list idx_array a_1_list | a_1_list true
                             | false

- a_list is a list of double row arrays containing
  on input: the matrix to be decomposed
  on return: the decomposed matrix
- idx_array is long and receives row permutation info (e.g., for backsubLU)
- returns false upon detecting singular matrix
*/

P op_invertLU(void)
{
  L32 N, k, i, j;
  L32 *idxp;
  B *fp;
  D **ap, **a1p, *vec, d;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if ( CLASS(o_3) != LIST) return OPD_CLA;
  N = (LIST_CEIL(o_3) - VALUE_BASE(o_3)) / FRAMEBYTES;
  if (N<=0) return RNG_CHK;
  if (CLASS(o_1) != LIST) return OPD_CLA;
  if (N != (LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES) return RNG_CHK;
  if (FREEvm + DALIGN(N * (2 * sizeof(D *)) + N * sizeof(D)) > CEILvm)
    return VM_OVF;

  ap = (D **)FREEvm; 
  fp = (B *)VALUE_BASE(o_3);
  for (k=0; k<N; k++) { if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ARRAY_SIZE(fp) != N) return RNG_CHK;
    *(ap++) = ((D *)VALUE_BASE(fp)) - 1;
    fp += FRAMEBYTES;
  }

  a1p = ap; 
  fp = (B *)VALUE_BASE(o_1);
  for (k=0; k<N; k++) {
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (ARRAY_SIZE(fp) != N) return RNG_CHK;
    *(ap++) = ((D *)VALUE_BASE(fp)) - 1;
    fp += FRAMEBYTES;
  }

  vec = (D *)DALIGN(ap)-1;
  if (TAG(o_2) != (ARRAY | LONG32TYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_2) != N) return RNG_CHK;
  idxp = (L32 *)VALUE_BASE(o_2)-1;
  
  if (ludcmp(((D **)FREEvm)-1, N, idxp, &d, vec)) {
    for (j=1; j<=N; j++) {
      for (i=1; i<=N; i++) vec[i] = 0.0;
      vec[j] = 1.0;
      lubksb(((D **)FREEvm)-1, N, idxp, vec);
      for (i=1; i<=N; i++)
        (a1p-1)[i][j] = vec[i];
    }
    moveframe(o_1,o_3);
    TAG(o_2) = BOOL; BOOL_VAL(o_2) = TRUE;
    FREEopds = o_1;
  }
  else {
    TAG(o_3) = BOOL; BOOL_VAL(o_3) = FALSE;
    FREEopds = o_2;
  }
  return OK;
}

/*----------------------------------------------- matmul
   c a b | c          a * b -> c

   - each argument is a list of row vectors
   - number of columns in a equals number of rows in b
   - in c, the number of rows is the same as that in a, and the
     number of columns is the same as that in b
*/

#if ENABLE_THREADS
typedef struct {
  L32 Ncolc, Ncola;
  D *restrict *restrict ap, 
	*restrict *restrict bp, 
	*restrict *restrict cp;
  UL32 perthread;
  UL32 leftover;
} matmult;

P thread_matmul(UL32 id, const void* global, 
                void* local __attribute__ ((__unused__))) {
  const matmult* restrict m = (const matmult*) global;
  UL32 n = m->perthread + (thread_max() == id ? m->leftover : 0);
  UL32 i_ = m->perthread*id;
  UL32 i, j;

  for (i = i_; i < i_ + n; i++)
    for (j = 0; j < (UL32) m->Ncolc; j++)
      MATMUL_INNER(m->Ncola, m->ap, m->bp, m->cp);
  
  return OK;
}
#endif

P op_matmul(void)
{
  UL32 Nrowa, Nrowb, Nrowc, Ncola , Ncolb, Ncolc, i, j, k;
  B *fp;
  D **p, **ap, **bp, **cp;
#if ENABLE_THREADS
  UL32 nways;
#endif

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_3) != LIST) return OPD_CLA;
  if (ATTR(o_3) & READONLY) return OPD_ATR;
  Nrowc = (LIST_CEIL(o_3) - VALUE_BASE(o_3)) / FRAMEBYTES;
  if ( CLASS(o_2) != LIST) return OPD_CLA;
  Nrowa = (LIST_CEIL(o_2) - VALUE_BASE(o_2)) / FRAMEBYTES;
  if ( CLASS(o_1) != LIST) return OPD_CLA;
  Nrowb = (LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES;

  if ((FREEvm + (Nrowc + Nrowa + Nrowb) *  sizeof(D *)) > CEILvm) 
    return VM_OVF;
  cp = p = (D **)FREEvm;  
  fp = (B *)VALUE_BASE(o_3); 
  Ncolc = 0;
  for (k=0; k<Nrowc; k++) {
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (Ncolc == 0) Ncolc = ARRAY_SIZE(fp);
    else if ((UL32) ARRAY_SIZE(fp) != Ncolc) return RNG_CHK;
    *(p++) = ((D *)VALUE_BASE(fp));
    fp += FRAMEBYTES;
  }

  ap = p; 
  fp = (B *)VALUE_BASE(o_2); 
  Ncola = 0;
  for (k=0; k<Nrowa; k++) { 
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (Ncola == 0) Ncola = ARRAY_SIZE(fp);
    else if ((UL32) ARRAY_SIZE(fp) != Ncola) return RNG_CHK;
    *(p++) = ((D *) VALUE_BASE(fp));
    fp += FRAMEBYTES;
  }

  bp = p; 
  fp = (B *)VALUE_BASE(o_1); 
  Ncolb = 0;
  for (k=0; k<Nrowb; k++) { 
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (Ncolb == 0) Ncolb = ARRAY_SIZE(fp);
    else if ((UL32) ARRAY_SIZE(fp) != Ncolb) return RNG_CHK;
    *(p++) = ((D*) VALUE_BASE(fp));
    fp += FRAMEBYTES;
  }
  
  if ((Ncola != Nrowb) || (Nrowa != Nrowc) || (Ncolb != Ncolc)) return RNG_CHK;

#if ENABLE_THREADS
  if (thread_num() > 1 && ! serialized && Nrowc > 1) {
    nways = Nrowc*Ncolc*Ncola/(THREADMUL << ROLLBITS)
      + ((Nrowc*Ncolc*Ncola%(THREADMUL << ROLLBITS)) ? 1: 0);
    if (nways > Nrowc) nways = Nrowc;
  }
  else nways = 1;
  
  if (nways == 1) {
#endif
    for (i=0; i<Nrowc; i++)
      for (j=0; j<Ncolc; j++)
        MATMUL_INNER(Ncola, ap, bp, cp);
#if ENABLE_THREADS
  }
  else {
    matmult m;
    if (nways > thread_num()) nways = thread_num();
    m.perthread = Nrowc / nways;
    m.leftover = Nrowc % nways;
    
    m.Ncolc = Ncolc; m.Ncola = Ncola;
    m.ap = ap; m.bp = bp; m.cp = cp;
    threads_do(nways, thread_matmul, &m);
  }
#endif //ENABLE_THREADS
   
        
  FREEopds = o_2;
  return OK;
}

/*----------------------------------------------- mattranspose
   b a | b           a* -> b

   - each argument is a list of row vectors
   - number of columns in a equals number of rows in b
   - number of rows in a equals number of colums in b

*/

#if ENABLE_THREADS
typedef struct {
  L32 Ncola;
  UL32 perthread;
  UL32 leftover;
  D *restrict *restrict ap, *restrict *restrict bp;
} mattransposet;

P thread_mattranspose(UL32 id,
                      const void* global,
                      void* local __attribute__ ((__unused__))) {
  const mattransposet* restrict m = (const mattransposet*) global;
  UL32 n = m->perthread + (thread_max() == id ? m->leftover : 0);
  const UL32 i_ = m->perthread*id;
  UL32 i;
  
  for (i = i_; i < i_ + n; ++i) 
    MATTRANSPOSE_INNER(m->Ncola, m->ap, m->bp);
  
  return OK;
}
#endif //ENABLE_THREADS

P op_mattranspose(void)
{
  UL32 Nrowa, Nrowb, Ncola , Ncolb, i, k;
  B *fp;
  D **p, **ap, **bp;
#if ENABLE_THREADS
  UL32 nways;
#endif //ENABLE_THREADS

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != LIST) return OPD_CLA;
  if (ATTR(o_2) & READONLY) return OPD_ATR;
  Nrowb = (LIST_CEIL(o_2) - VALUE_BASE(o_2)) / FRAMEBYTES;
  if (CLASS(o_1) != LIST) return OPD_CLA;
  Nrowa = (LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES;
  if ((FREEvm + (Nrowa + Nrowb) *  sizeof(D *)) > CEILvm) return VM_OVF;
  
  bp = p = (D **)FREEvm;  
  fp = (B *)VALUE_BASE(o_2); 
  Ncolb = 0;
  for (k=0; k<Nrowb; k++) {
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (Ncolb == 0) Ncolb = ARRAY_SIZE(fp);
    else if ((UL32) ARRAY_SIZE(fp) != Ncolb) return RNG_CHK;
    *(p++) = ((D*) VALUE_BASE(fp));
    fp += FRAMEBYTES;
  }

  ap = p; 
  fp = (B *)VALUE_BASE(o_1); 
  Ncola = 0;
  for (k=0; k<Nrowa; k++) { 
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (Ncola == 0)  Ncola = ARRAY_SIZE(fp);
    else if ((UL32) ARRAY_SIZE(fp) != Ncola) return RNG_CHK;
    *(p++) = ((D*) VALUE_BASE(fp));
    fp += FRAMEBYTES;
  }
  
 if ((Ncola != Nrowb) || (Nrowa != Ncolb)) return RNG_CHK;
 
#if ENABLE_THREADS
 if (thread_num() > 1 && ! serialized && Nrowa > 1) {
   nways = Nrowa*Ncola/(THREADMUL << ROLLBITS)
     + ((Nrowa*Ncola%(THREADMUL << ROLLBITS)) ? 1 : 0);
   if (nways > Nrowa) nways = Nrowa;
 }
 else nways = 1;

 if (nways == 1) {
#endif //ENABLE_THREADS
   for (i=0; i<Nrowa; i++)
     MATTRANSPOSE_INNER(Ncola, ap, bp);
#if ENABLE_THREADS
 }
 else {
   mattransposet m;
   if (nways > thread_num()) nways = thread_num();
   m.perthread = Nrowa / nways;
   m.leftover = Nrowa % nways;
   m.Ncola = Ncola;
   m.bp = bp; m.ap = ap;
   threads_do(nways, thread_mattranspose, &m);
 }
#endif //ENABLE_THREADS
 
 FREEopds = o_1;
 return OK;
}

/*----------------------------------------------- matvecmul
   c a b | c          a * b -> c

   - a is a list of row vectors
   - b is a column vector
   - c is a column vector
   - if a is n*m, then b is m in length, and c is n
*/

#if ENABLE_THREADS
typedef struct {
  D *restrict *restrict ap,
    *restrict bp,
    *restrict cp;
  UL32 perthread;
  UL32 leftover;
  L32 Ncola;
} matvecmult;

P thread_matvecmul(UL32 id,
                   const void* global,
                   void* local __attribute__ ((__unused__))) {
  const matvecmult *restrict m = (const matvecmult*) global;
  const UL32 n = m->perthread + (thread_max() == id ? m->leftover : 0);
  const UL32 i_ = m->perthread * id;
  UL32 i;
  
  for (i = i_;  i < i_ + n; ++i)
    MATVECMUL_INNER(m->Ncola, m->ap, m->bp, m->cp);
  
  return OK;
}
#endif //ENABLE_THREADS

P op_matvecmul(void)
{
  UL32 Nrowa, Nrowb, Nrowc, Ncola, i, k;
  B *fp;
  D **p, **ap, *bp, *cp;
#if ENABLE_THREADS
  UL32 nways;
#endif //ENABLE_THREADS

  if (o_3 < FLOORopds) return OPDS_UNF;
  if ( CLASS(o_3) != ARRAY) return OPD_CLA;
  if (ATTR(o_3) & READONLY) return OPD_ATR;
  Nrowc = ARRAY_SIZE(o_3);
  if ( CLASS(o_2) != LIST) return OPD_CLA;
  Nrowa = (LIST_CEIL(o_2) - VALUE_BASE(o_2)) / FRAMEBYTES;
  if ( CLASS(o_1) != ARRAY) return OPD_CLA;
  Nrowb = ARRAY_SIZE(o_1);
  if ((FREEvm + Nrowa *  sizeof(D *)) > CEILvm) return VM_OVF;
  p = (D **)FREEvm;

  ap = p; 
  fp = (B *)VALUE_BASE(o_2); 
  Ncola = 0;
  for (k=0; k<Nrowa; k++)  {
    if (TAG(fp) != (ARRAY | DOUBLETYPE)) return OPD_ERR;
    if (Ncola == 0) Ncola = ARRAY_SIZE(fp);
    else if ((UL32) ARRAY_SIZE(fp) != Ncola) return RNG_CHK;
    *(p++) = ((D*) VALUE_BASE(fp));
    fp += FRAMEBYTES;
  }
  
  if ((Ncola != Nrowb) || (Nrowa != Nrowc)) return RNG_CHK;
  bp = (D*) VALUE_BASE(o_1); cp = (D*) VALUE_BASE(o_3);

#if ENABLE_THREADS
  if (thread_num() > 1 && ! serialized && Nrowc > 1) {
    nways = Nrowc*Ncola/(THREADMUL << ROLLBITS)
      + ((Nrowc*Ncola % (THREADMUL << ROLLBITS)) ? 1 : 0);
    if (nways > Nrowc) nways = Nrowc;
  }
  else nways = 1;

  if (nways == 1) {
#endif //ENABLE_THREADS
    for (i=0; i<Nrowc; i++) 
      MATVECMUL_INNER(Ncola, ap, bp, cp);
#if ENABLE_THREADS
  }
  else {
    matvecmult m;
    if (nways > thread_num()) nways = thread_num();
    m.perthread = Nrowc / nways;
    m.leftover = Nrowc % nways;
    
    m.Ncola = Ncola;
    m.ap = ap; m.bp = bp; m.cp = cp;
    threads_do(nways, thread_matvecmul, &m);
  }
#endif //ENABLE_THREADS
  
  FREEopds = o_2;
  return OK;
}
 
