
#include "newfunc.h"
#include "cpptestermap.h"

#include "cpptester.h"

UL ll_type = 0;
L op_hi(void) {return wrap_hi("cpptester V1");}
L op_libnum(void) {return wrap_libnum(ll_type);}
L ll_errc[] = {
CPPTESTER_BAD_ALLOC,
CPPTESTER_ABORT_ALLOC,
CPPTESTER_LEAK_ALLOC,
CPPTESTER_UNKNOWN_ALLOC,
  0L
};

B* ll_errm[] = {
"** cpptester: Out of memory",
"** cpptester: Error in memory allocator",
"** cpptester: Leaking in memory allocator",
"** cpptester: ??? Whah???",
  NULL
};

B* ll_export[] = {
  "hi", (B*) op_hi,
  "libnum", (B*) op_libnum,
  "INIT_", (B*) op_INIT_,
  "FINI_", (B*) op_FINI_,
"maketester", (B*) op_maketester,
"runtester", (B*) op_runtester,
  "", NULL
};

B opaquename[FRAMEBYTES];


  L op_INIT_(void) {

  return OK;
}

 L op_FINI_(void) {

  return OK;
}

L op_maketester(void) {

      L size;
      Allocator* alloc;
      B* mem;
      if (o_1 < FLOORopds) return OPDS_UNF;
      if (CLASS(o_1) != NUM) return OPD_CLA;
      if (VALUE(o_1, &size)) return UNDF_VAL;

   {
      B initframe[FRAMEBYTES];
      B* procframe = make_opaque_frame(size, opaquename, 
NULL);
     if (! procframe) return VM_OVF;

      mem = OPAQUE_MEM(procframe, buffernameframe);
      alloc = makeAllocator(mem, size);
      if (! alloc) RETURN_ERROR(CPPTESTER_BAD_ALLOC);

     moveframe(procframe, o_1);
   }

      return OK;

}

L op_runtester(void) {

      Allocator* old;
      int ret;
      L times, max;
      if (o_3 < FLOORopds) return OPDS_UNF;
      TEST_OPAQUE(o_1);
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!VALUE(o_2, &max)) return UNDF_VAL;
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!VALUE(o_2, &times)) return UNDF_VAL;
      
      old = setAllocator((Allocator*)OPAQUE_MEM(o_1, buffernameframe));
      ret = runtester(times, max);
      setAllocator(old);
      switch (ret) {
        case 0: FREEopds = o_2; return OK;
        case 1: RETURN_ERROR(CPPTESTER_BAD_ALLOC);
        case 2: RETURN_ERROR(CPPTESTER_ABORT_ALLOC);
        case 3: RETURN_ERROR(CPPTESTER_LEAK_ALLOC);
        default: RETURN_ERROR(CPPTESTER_UNKNOWN_ALLOC);
      }

}


