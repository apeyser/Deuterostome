
#include "cpptestercode.h"
#include "newfunc.h"

#include <stdio.h>
#include <stdlib.h>
#include "srandomdev-local.h"

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
"randomtester", (B*) op_randomtester,
"killtester", (B*) op_killtester,
  "", NULL
};

B opaquename[FRAMEBYTES];
static B CPPTESTER_TESTER_N[FRAMEBYTES];



int runtester(int times, int max, Tester* t) 
{
  int i, j;
  int ret;

  for (j = 0; j < times; j++) {
    for (i = 0; i < max; i++) 
      if ((ret = addElem(t, i))) return ret;
    for (i = 0; i < max; i++)
      if ((ret = removeElem(t))) return ret;
    for (i = 0; i < max; i++)
      if ((ret = addElem(t, i+max))) return ret;
    for (i = 0; i < max; i++)
      if ((ret = removeElem(t))) return ret;
    if ((ret = resetElems(t))) return ret;
  }

  return 0;
}

int finalize(Tester* t) {
  int ret;
  if ((ret = fini(t))) return ret;
  if (leaked()) return 3;
  return 0;
}

#define check_ret(ret)       switch (ret) { \
        case 0: break; \
        case 1: RETURN_ERROR(CPPTESTER_BAD_ALLOC); \
        case 2: RETURN_ERROR(CPPTESTER_ABORT_ALLOC); \
        case 3: RETURN_ERROR(CPPTESTER_LEAK_ALLOC); \
        default: RETURN_ERROR(CPPTESTER_UNKNOWN_ALLOC); \
      }

int randomtester(int times, int inner, int max, Tester* t) {
  static int initrand = 0;
  size_t i, j, k;
  int ret;
  void** saved = (void**) alloca(sizeof(void*)*inner);
  if (! initrand) {
    srandomdev();
    initrand = 1;
  }

  for (j = 0; j < times; j++) {
    for (i = 0; i < inner; i++) saved[i] = NULL;

    for (i = 0; i < inner; i++)
      for (k = random()%inner; ! saved[k]; k = (k+1)%inner);
        if ((ret = createSized(&saved[k], random()%max)))
          return ret;

    for (i = 0; i < inner; i++)
      for (k = random()%inner; saved[k]; k = (k+1)%inner);
        if ((ret = destroy(saved[k])))
          return ret;
  }
      
  return 0;
}

  L op_INIT_(void) {
makename("CPPTESTER_HANDLE", opaquename);
makename("TESTER", CPPTESTER_TESTER_N);

  return OK;
}

 L op_FINI_(void) {

  return OK;
}

L op_maketester(void) {

      L size;
      Allocator* alloc;
      Allocator* old;
      B* mem;
      Tester* t;
      int ret;
      if (o_1 < FLOORopds) return OPDS_UNF;
      if (CLASS(o_1) != NUM) return OPD_CLA;
      if (VALUE(o_1, &size)) return UNDF_VAL;

   {
      B initframe[FRAMEBYTES];
      B* procframe = make_opaque_frame(size, opaquename, 
CPPTESTER_TESTER_N,
NULL);
     if (! procframe) return VM_OVF;

      mem = OPAQUE_MEM(procframe, buffernameframe);
      if (! (alloc = makeAllocator(mem, size))) RETURN_ERROR(CPPTESTER_BAD_ALLOC);
      old = setAllocator(alloc);
      ret = init(&t);
      setAllocator(old);
      check_ret(ret);
      TAG(initframe)= (NUM | LONGTYPE); ATTR(initframe) = 0;
      LONG_VAL((initframe)) = (L) t;
     OPAQUE_MEM_SET(procframe, CPPTESTER_TESTER_N, initframe);

     moveframe(procframe, o_1);
   }

      return OK;

}

L op_runtester(void) {

      Allocator* old;
      Tester* t;
      int ret;
      L times, max;
      if (o_3 < FLOORopds) return OPDS_UNF;
      TEST_OPAQUE(o_1);
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!VALUE(o_2, &max)) return UNDF_VAL;
      if (CLASS(o_3) != NUM) return OPD_CLA;
      if (!VALUE(o_3, &times)) return UNDF_VAL;
      
      old = setAllocator((Allocator*)OPAQUE_MEM(o_1, buffernameframe));
      t = (Tester*) CPPTESTER_TESTER(o_1);
      ret = runtester(times, max, t);
      setAllocator(old);
      check_ret(ret);

      FREEopds = o_3;
      return OK;

}

L op_randomtester(void) {

      Allocator* old;
      Tester* t;
      int ret;
      L times, inner, max;
      if (o_4 < FLOORopds) return OPDS_UNF;
      TEST_OPAQUE(o_1);
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!VALUE(o_2, &max)) return UNDF_VAL;
      if (CLASS(o_3) != NUM) return OPD_CLA;
      if (!VALUE(o_3, &inner)) return UNDF_VAL;
      if (CLASS(o_4) != NUM) return OPD_CLA;
      if (!VALUE(o_4, &times)) return UNDF_VAL;

      old = setAllocator((Allocator*)OPAQUE_MEM(o_1, buffernameframe));
      t = (Tester*) CPPTESTER_TESTER(o_1);
      ret = randomtester(times, inner, max, t);
      setAllocator(old);
      check_ret(ret);

      FREEopds = o_4;
      return OK;
 
}

L op_killtester(void) {

       Tester* t;
       int ret;
       Allocator* alloc;
       Allocator* old;
       if (o_1 < FLOORopds) return OPDS_UNF;
       TEST_OPAQUE(o_1);
 
       old = setAllocator((Allocator*)OPAQUE_MEM(o_1, buffernameframe));
       t = (Tester*) CPPTESTER_TESTER(o_1);
       ret = finalize(t);
       setAllocator(old);
       check_ret(ret);

       FREEopds = o_1; 
       KILL_OPAQUE(o1); 
       return OK;

}


