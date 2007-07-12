1000 dict dup begin

/version 1 def

/errsdict 4 dict dup begin | [
  /BAD_ALLOC (Out of memory) def
  /ABORT_ALLOC (Error in memory allocator) def 
  /LEAK_ALLOC (Leaking in memory allocator) def
  /UNKNOWN_ALLOC (??? Whah???) def | ]
end def

/bodyheaders (
#include "newfunc.h"
#include "cpptestercode.h"
) def

/bodyheaders (
#include "cpptestercode.h"
#include "newfunc.h"

#include <stdio.h>
#include <stdlib.h>
) def

/bodycode (
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
  int ret;
  if \(\(ret = fini\(t\)\)\) return ret;
  if \(leaked\(\)\) return 3;
  return 0;
}
) def

/makehandles {[[/TESTER {(LONG_VAL\() handle (\))}]]} bind def

/makeops {
  [
    [
      /maketester {(
      L size;
      Allocator* alloc;
      Allocator* old;
      B* mem;
      Tester* t;
      int ret;
      if \(o_1 < FLOORopds\) return OPDS_UNF;
      if \(CLASS\(o_1\) != NUM\) return OPD_CLA;
      if \(VALUE\(o_1, &size\)\) return UNDF_VAL;
) {(
      mem = ) getbufferframe (;
      if \(! \(alloc = makeAllocator\(mem, size\)\)\) ) /BAD_ALLOC error_ (;
      old = setAllocator\(alloc\);
      ret = init\(&t\);
      setAllocator\(old\);
      switch \(ret\) {
        case 0: break;
        case 1: ) /BAD_ALLOC error_ (;
        case 2: ) /ABORT_ALLOC error_ (;
        case 3: ) /LEAK_ALLOC error_ (;
        default: ) /UNKNOWN_ALLOC error_ (;
      };
      TAG) handle (= \(NUM | LONGTYPE\); ATTR) handle ( = 0;
      LONG_VAL\() handle (\) = \(L\) t;
     ) /TESTER make_handle (;
)} (size) (o_1) build_handle (
      return OK;
)}
    ][
      /runtester {(
      Allocator* old;
      Tester* t;
      int ret;
      L times, max;
      if \(o_3 < FLOORopds\) return OPDS_UNF;
      TEST_OPAQUE\(o_1\);
      if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_2, &max\)\) return UNDF_VAL;
      if \(CLASS\(o_3\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_3, &times\)\) return UNDF_VAL;
      
      old = setAllocator\(\(Allocator*\)) (o_1) getbufferfrom (\);
      t = \(Tester*\) ) /TESTER (o_1) handle (;
      ret = runtester\(times, max, t\);
      setAllocator\(old\);
      switch \(ret\) {
        case 0: FREEopds = o_3; return OK;
        case 1: ) /BAD_ALLOC error_ (;
        case 2: ) /ABORT_ALLOC error_ (;
        case 3: ) /LEAK_ALLOC error_ (;
        default: ) /UNKNOWN_ALLOC error_ (;
      }
)}
    ][
       /killtester {(
       Tester* t;
       int ret;
       Allocator* alloc;
       Allocator* old;
       if \(o_1 < FLOORopds\) return OPDS_UNF;
       TEST_OPAQUE\(o_1\);
 
       old = setAllocator\(\(Allocator*\)) (o_1) getbufferfrom (\);
       t = \(Tester*\) ) /TESTER (o_1) handle (;
       ret = finalize\(t\);
       setAllocator\(old\); 
       switch \(ret\) {
        case 0: FREEopds = o_1; 
                KILL_OPAQUE\(o1\); 
                return OK;
        case 1: ) /BAD_ALLOC error_ (;
        case 2: ) /ABORT_ALLOC error_ (;
        case 3: ) /LEAK_ALLOC error_ (;
        default: ) /UNKNOWN_ALLOC error_ (;
      };
)}
  ]
]} bind def

/all {
  getstartupdir (plugin.d) fromfiles
  PLUGINS begin all end
} bind def

end userdict /cpptester put
