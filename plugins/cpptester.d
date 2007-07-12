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
#include "cpptestermap.h"
) def

/makeops {
  [
    [
      /maketester {(
      L size;
      Allocator* alloc;
      B* mem;
      if \(o_1 < FLOORopds\) return OPDS_UNF;
      if \(CLASS\(o_1\) != NUM\) return OPD_CLA;
      if \(VALUE\(o_1, &size\)\) return UNDF_VAL;
) {(
      mem = ) getbufferframe (;
      alloc = makeAllocator\(mem, size\);
      if \(! alloc\) ) /BAD_ALLOC error_ (;
)} (size) (o_1) build_handle (
      return OK;
)}
    ][
      /runtester {(
      Allocator* old;
      int ret;
      L times, max;
      if \(o_3 < FLOORopds\) return OPDS_UNF;
      TEST_OPAQUE\(o_1\);
      if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_2, &max\)\) return UNDF_VAL;
      if \(CLASS\(o_2\) != NUM\) return OPD_CLA;
      if \(!VALUE\(o_2, &times\)\) return UNDF_VAL;
      
      old = setAllocator\(\(Allocator*\)) (o_1) getbufferfrom (\);
      ret = runtester\(times, max\);
      setAllocator\(old\);
      switch \(ret\) {
        case 0: FREEopds = o_2; return OK;
        case 1: ) /BAD_ALLOC error_ (;
        case 2: ) /ABORT_ALLOC error_ (;
        case 3: ) /LEAK_ALLOC error_ (;
        default: ) /UNKNOWN_ALLOC error_ (;
      }
)}
    ]
  ]
} bind def

/all {
  getstartupdir (plugin.d) fromfiles
  PLUGINS begin all end
} bind def

end userdict /cpptester put

