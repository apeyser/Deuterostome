
#include "cpptestercode.h"

#include <stdio.h>
#include <stdlib.h>
#include "cpptester.h"
#include "srandomdev-local.h"

#include "cpptester.h"

UP ll_type = 0;
P op_hi(void) {return wrap_hi((B*)"cpptester V1");}
P op_libnum(void) {return wrap_libnum(ll_type);}
P ll_errc[] = {
CPPTESTER_BAD_ALLOC,
CPPTESTER_STD_EXCEPTION,
CPPTESTER_UNKNOWN_EXCEPTION,
CPPTESTER_ILLEGAL_RETURN,
  0L
};

B* ll_errm[] = {
(B*)"** cpptester: Out of memory",
(B*)"** cpptester: Std exception thrown",
(B*)"** cpptester: ??? Whah??? Exception",
(B*)"** cpptester: ??? Wha?? How'dat'appen?",
  NULL
};

B* ll_export[] = {
  (B*)"hi", (B*) op_hi,
  (B*)"libnum", (B*) op_libnum,
  (B*)"INIT_", (B*) op_INIT_,
  (B*)"FINI_", (B*) op_FINI_,
(B*)"maketester", (B*) op_maketester,
(B*)"runtester", (B*) op_runtester,
(B*)"randomtester", (B*) op_randomtester,
(B*)"bigtester", (B*) op_bigtester,
(B*)"killtester", (B*) op_killtester,
  (B*)"", NULL
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
  return fini(t);
}

#define check_ret(ret) \
      switch (ret) { \
        case 0: break; \
        case 1: RETURN_ERROR(CPPTESTER_BAD_ALLOC); \
        case 2: RETURN_ERROR(CPPTESTER_STD_EXCEPTION); \
        case 3: RETURN_ERROR(CPPTESTER_UNKNOWN_EXCEPTION); \
        default: RETURN_ERROR(CPPTESTER_ILLEGAL_RETURN); \
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

    for (i = 0; i < inner; i++) {
      for (k = random()%inner; saved[k]; k = (k+1)%inner);
      if ((ret = createSized(&saved[k], random()%max)))
        return ret;
   }

    for (i = 0; i < inner; i++) {
      for (k = random()%inner; ! saved[k]; k = (k+1)%inner);
      if ((ret = destroy(saved[k])))
        return ret;
     saved[k] = NULL;
    }
  }
      
  return 0;
}

int bigtester(P size, Tester* t) {
  return big(size, t);
}

P op_INIT_(void) {
makename((B*)"CPPTESTER_HANDLE", opaquename);
makename((B*)"TESTER", CPPTESTER_TESTER_N);

  return OK;
}

P op_FINI_(void) {

  return OK;
}

P op_maketester(void) {

      Tester* t;

      if (CEILopds < o2) return OPDS_OVF;

   {
      B initframe[FRAMEBYTES];
      B* procframe = make_opaque_frame(0, (B*) &ll_export[16], opaquename, 
CPPTESTER_TESTER_N,
NULL);
     if (! procframe) return VM_OVF;

      check_ret(init(&t));
      TAG(initframe)= (NUM | INNERPTYPE); ATTR(initframe) = 0;
      INNERP_VAL((initframe)) = (P) t;
     OPAQUE_MEM_SET(procframe, CPPTESTER_TESTER_N, initframe);

     moveframe(procframe, o1);
   }

      FREEopds = o2;
      return OK;

}

P op_runtester(void) {

      Tester* t;
      P times, max;
      if (o_3 < FLOORopds) return OPDS_UNF;
      TEST_OPAQUE(o_1);
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!PVALUE(o_2, &max)) return UNDF_VAL;
      if (CLASS(o_3) != NUM) return OPD_CLA;
      if (!PVALUE(o_3, &times)) return UNDF_VAL;
      
      t = (Tester*) CPPTESTER_TESTER(o_1);
      check_ret(runtester(times, max, t));

      FREEopds = o_3;
      return OK;

}

P op_randomtester(void) {

      Tester* t;
      P times, inner, max;
      if (o_4 < FLOORopds) return OPDS_UNF;
      TEST_OPAQUE(o_1);
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!PVALUE(o_2, &max)) return UNDF_VAL;
      if (CLASS(o_3) != NUM) return OPD_CLA;
      if (!PVALUE(o_3, &inner)) return UNDF_VAL;
      if (CLASS(o_4) != NUM) return OPD_CLA;
      if (!PVALUE(o_4, &times)) return UNDF_VAL;

      t = (Tester*) CPPTESTER_TESTER(o_1);
      check_ret(randomtester(times, inner, max, t));

      FREEopds = o_4;
      return OK;
 
}

P op_bigtester(void) {

      Tester* t;
      P size;
      if (o_2 < FLOORopds) return OPDS_UNF;
      TEST_OPAQUE(o_1);
      if (CLASS(o_2) != NUM) return OPD_CLA;
      if (!PVALUE(o_2, &size)) return UNDF_VAL;
      t = (Tester*) CPPTESTER_TESTER(o_1);
      check_ret(bigtester(size, t));

      FREEopds = o_2;
      return OK;

}

P op_killtester(void) {

       Tester* t;
       if (o_1 < FLOORopds) return OPDS_UNF;
       TEST_OPAQUE(o_1);
 
       t = (Tester*) CPPTESTER_TESTER(o_1);
       check_ret(finalize(t));

       KILL_OPAQUE(); 
       return OK;

}


