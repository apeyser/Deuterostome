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
#include "dm-stats-header.h"

static P max_n = 0;
#define max_kn (1000)
static D binom[max_kn+1][max_kn+1] = {{1}};

// n k | (n/k)
DM_INLINE_STATIC P combinations(void) {
  P n, k, i, j;

  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! PVALUE(o_2, &n)) return UNDF_VAL;
  if (! PVALUE(o_1, &k)) return UNDF_VAL;
  if (n < 0 || k < 0 || n < k) return RNG_CHK;
  if (n > max_kn) return RNG_CHK;

  TAG(o_2) = (NUM | DOUBLETYPE);
  ATTR(o_2) = 0;
  FREEopds = o_1;
  if (n == k || k == 0) {
    *(D*) NUM_VAL(o_1) = 1;
    return OK;
  }

  if (n <= max_n) {
    *(D*) NUM_VAL(o_1) = binom[n][k];
    return OK;
  }

  for (i = max_n+1; i <= n; ++i) {
    binom[i][0] = binom[i][i] = 1;
    for (j = 1; j < i; ++j) {
      binom[i][j] = binom[i-1][j-1] + binom[i-1][j];
    }
  }
  max_n = n;

  *(D*) NUM_VAL(o_1) = binom[n][k];
  return OK;
}

// x l | Pl(x)
DM_INLINE_STATIC P legendre(void) {
  P l, i;
  D x, Px, Px_1, Px_2;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! PVALUE(o_1, &l)) return UNDF_VAL;
  if (l < 0) return RNG_CHK;
  if (TAG(o_2) != (NUM | DOUBLETYPE)) return OPD_TYP;

  FREEopds = o_1;
  if (l == 0) {
    *(D*) NUM_VAL(o_1) = 1;
    return OK;
  }

  Px = x = *(D*) NUM_VAL(o_1);
  Px_1 = 1;
  for (i = 2; i <= l; ++i) {
    Px_2 = Px_1;
    Px_1 = Px;
    Px = ((D)(2*i-1))/i*x*Px_1 - ((D)(i-1))/i*Px_2;
  }

  *(D*) NUM_VAL(o_1) = Px;
  return OK;

}

#include "dm-stats-main.h"
