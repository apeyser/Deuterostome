#include <stdlib.h>
#include "../src/dm.h"
#include "libtest2.h"

UL ll_type = 0;

L op_test2_hi(void) {return wrap_hi("Test 2 V1");}
L op_test2_libnum(void) {return wrap_libnum(ll_type);}

L ll_errc[] = {TEST2_ERROR, 0L};

B* ll_errm[] = {"** TEST2: Test Error"};

B* ll_export[] = {
    "hi", (B*) op_test2_hi,
    "libnum", (B*) op_test2_libnum,
    "say5", (B*) op_say5,
    "sayerror", (B*) op_sayerror,
    "", NULL
};

L op_say5(void) 
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = NUM | LONGTYPE;
    ATTR(o1) = 0;
    LONG_VAL(o1) = 6L;
    FREEopds = o2;
    return OK;
}

L op_sayerror(void) {return ll_type | TEST2_ERROR;}
