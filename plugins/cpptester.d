1000 dict dup begin

/version 1 def

/errsdict 4 dict dup begin | [
  /BAD_ALLOC (Out of memory) def
  /STD_EXCEPTION (Std exception thrown) def
  /UNKNOWN_EXCEPTION (??? Whah??? Exception) def
  /ILLEGAL_RETURN (??? Wha?? How'dat'appen?) def | ]
end def

/bodyheaders (
#include "cpptestercode.h"

#include <stdio.h>
#include <stdlib.h>
#include "srandomdev-local.h"
) def

/bodycode {(
int runtester\(int times, int max, Tester* t\) 
{
  int i, j;
  int ret;

  for \(j = 0; j < times; j++\) {
    for \(i = 0; i < max; i++\) 
      if \(\(ret = addElem\(t, i\)\)\) return ret;
    for \(i = 0; i < max; i++\)
      if \(\(ret = removeElem\(t\)\)\) return ret;
    for \(i = 0; i < max; i++\)
      if \(\(ret = addElem\(t, i+max\)\)\) return ret;
    for \(i = 0; i < max; i++\)
      if \(\(ret = removeElem\(t\)\)\) return ret;
    if \(\(ret = resetElems\(t\)\)\) return ret;
  }

  return 0;
}

int finalize\(Tester* t\) {
  return fini\(t\);
}

#define check_ret\(ret\) \\
      switch \(ret\) { \\
        case 0: break; \\
        case 1: ) /BAD_ALLOC error_ (; \\
        case 2: ) /STD_EXCEPTION error_ (; \\
        case 3: ) /UNKNOWN_EXCEPTION error_ (; \\
        default: ) /ILLEGAL_RETURN error_ (; \\
      }

int randomtester\(int times, int inner, int max, Tester* t\) {
  static int initrand = 0;
  size_t i, j, k;
  int ret;
  void** saved = \(void**\) alloca\(sizeof\(void*\)*inner\);
  if \(! initrand\) {
    srandomdev\(\);
    initrand = 1;
  }

  for \(j = 0; j < times; j++\) {
    for \(i = 0; i < inner; i++\) saved[i] = NULL;

    for \(i = 0; i < inner; i++\) {
      for \(k = random\(\)%inner; saved[k]; k = \(k+1\)%inner\);
      if \(\(ret = createSized\(&saved[k], random\(\)%max\)\)\)
        return ret;
   }

    for \(i = 0; i < inner; i++\) {
      for \(k = random\(\)%inner; ! saved[k]; k = \(k+1\)%inner\);
      if \(\(ret = destroy\(saved[k]\)\)\)
        return ret;
     saved[k] = NULL;
    }
  }
      
  return 0;
}
)} def

/makehandles {[[/TESTER {(LONG_VAL\() handle (\))}]]} bind def

/makeops {
  [
    [
      /maketester {(
      B* mem;
      Tester* t;

      if \(CEILopds < o2\) return OPDS_OVF;
) {(
      check_ret\(init\(&t\)\);
      TAG) handle (= \(NUM | LONGTYPE\); ATTR) handle ( = 0;
      LONG_VAL\() handle (\) = \(L\) t;
     ) /TESTER make_handle (;
)} 0 /killtester (o1) build_handle (
      FREEopds = o2;
      return OK;
)}
    ][
      /runtester {(
      Tester* t;
      L times, max;
      if \(o_3 < FLOORopds\) return OPDS_UNF;
      TEST_OPAQUE\(o_1\);
      if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_2, &max\)\) return UNDF_VAL;
      if \(CLASS\(o_3\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_3, &times\)\) return UNDF_VAL;
      
      t = \(Tester*\) ) /TESTER (o_1) handle (;
      check_ret\(runtester\(times, max, t\)\);

      FREEopds = o_3;
      return OK;
)}
    ][
      /randomtester {(
      Tester* t;
      L times, inner, max;
      if \(o_4 < FLOORopds\) return OPDS_UNF;
      TEST_OPAQUE\(o_1\);
      if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_2, &max\)\) return UNDF_VAL;
      if \(CLASS\(o_3\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_3, &inner\)\) return UNDF_VAL;
      if \(CLASS\(o_4\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_4, &times\)\) return UNDF_VAL;

      t = \(Tester*\) ) /TESTER (o_1) handle (;
      check_ret\(randomtester\(times, inner, max, t\)\);

      FREEopds = o_4;
      return OK;
 )}
    ][
       /killtester {(
       Tester* t;
       if \(o_1 < FLOORopds\) return OPDS_UNF;
       TEST_OPAQUE\(o_1\);
 
       t = \(Tester*\) ) /TESTER (o_1) handle (;
       check_ret\(finalize\(t\)\);

       FREEopds = o_1; 
       KILL_OPAQUE\(o1\); 
       return OK;
)}
  ]
]} bind def

/all {
  getstartupdir (plugin.d) fromfiles
  PLUGINS begin all end
} bind def

end userdict /cpptester put
