1000 dict dup begin

/version 1 def

/bodycode
(
static L max_n = 0;
#define max_kn \(1000\)
static D binom[max_kn+1][max_kn+1] = {{1}};
) def


/makeops {
  [
    [
      /combinations {(
  // n k | \(n/k\)
  L n, k, i, j;

  if \(o_2 < FLOORopds\) return OPDS_UNF;
  if \(CLASS\(o_1\) != NUM\) return OPD_CLA;
  if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
  if \(! VALUE\(o_2, &n\)\) return UNDF_VAL;
  if \(! VALUE\(o_1, &k\)\) return UNDF_VAL;
  if \(n < 0 || k < 0 || n < k\) return RNG_CHK;
  if \(n > max_kn\) return RNG_CHK;

  TAG\(o_2\) = \(NUM | DOUBLETYPE\);
  ATTR\(o_2\) = 0;
  FREEopds = o_1;
  if \(n == k || k == 0\) {
    *\(D*\) NUM_VAL\(o_1\) = 1;
    return OK;
  }

  if \(n <= max_n\) {
    *\(D*\) NUM_VAL\(o_1\) = binom[n][k];
    return OK;
  }

  for \(i = max_n+1; i <= n; ++i\) {
    binom[i][0] = binom[i][i] = 1;
    for \(j = 1; j < i; ++j\) {
      binom[i][j] = binom[i-1][j-1] + binom[i-1][j];
    }
  }
  max_n = n;

  *\(D*\) NUM_VAL\(o_1\) = binom[n][k];
  return OK;
)}
    ]
    [
      /legendre {(
  // x l | Pl\(x\)
  L l, i;
  D x, Px, Px_1, Px_2;
  if \(o_2 < FLOORopds\) return OPDS_UNF;
  if \(CLASS\(o_1\) != NUM\) return OPD_CLA;
  if \(! VALUE\(o_1, &l\)\) return UNDF_VAL;
  if \(l < 0\) return RNG_CHK;
  if \(TAG\(o_2\) != \(NUM | DOUBLETYPE\)\) return OPD_TYP;

  FREEopds = o_1;
  if \(l == 0\) {
    *\(D*\) NUM_VAL\(o_1\) = 1;
    return OK;
  }

  Px = x = *\(D*\) NUM_VAL\(o_1\);
  Px_1 = 1;
  for \(i = 2; i <= l; ++i\) {
    Px_2 = Px_1;
    Px_1 = Px;
    Px = \(\(D\)\(2*i-1\)\)/i*x*Px_1 - \(\(D\)\(i-1\)\)/i*Px_2;
  }

  *\(D*\) NUM_VAL\(o_1\) = Px;
  return OK;
)} 
    ]
  ]
} bind def

/all {
  getstartupdir (plugin.d) fromfiles
  PLUGINS begin all end
} bind def

end userdict /stats put
