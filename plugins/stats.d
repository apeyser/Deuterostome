1000 dict dup begin

/version 1 def

/makeops {
  [
    [
      /combinations {(
  L n, k, num_min, denom_max, i, j;
  D v; L* num;

  if \(o_2 < FLOORopds\) return OPDS_UNF;
  if \(CLASS\(o_1\) != NUM\) return OPD_CLA;
  if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
  if \(! VALUE\(o_2, &n\)\) return UNDF_VAL;
  if \(! VALUE\(o_1, &k\)\) return UNDF_VAL;
  if \(n < 0 || k < 0 || n < k\) return RNG_CHK;

  if \(n-k+1 <= k\) {
    num_min = k+1;
    denom_max = n-k;
  }
  else {
    num_min = n-k+1;
    denom_max = k > 1 ? k : 2;
  }

  if \(FREEvm + sizeof\(L\)*\(n - num_min + 1\) >= CEILvm\) return VM_OVF;
  num = \(L*\) FREEvm;
  for \(j = num_min; j <= n; ++j\)
    num[j] = j;

  for \(i = 2; i <= denom_max; i++\) {
    for \(j = \(i < num_min\) ? num_min : i; j <= n; j++\) {
      if \(! \(num[j] % i\)\) {
        num[j] /= i;
        break;
      }
    }
  }

  v = 1.0;
  for \(j = num_min; j <= n; ++j\)
    v *= num[j];

  TAG\(o_2\) = \(NUM | DOUBLETYPE\);
  ATTR\(o_2\) = 0;
  *\(D*\) NUM_VAL\(o_2\) = v;
  FREEopds = o_1;
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
