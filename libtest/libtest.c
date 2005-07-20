#include <stdlib.h>
#include "../src/dm.h"
#include "libtest.h"

UL ll_type = 0;

L op_test_hi(void) {return wrap_hi("Test V1");}
L op_test_libnum(void) {return wrap_libnum(ll_type);}

L ll_errc[] = {TEST_ERROR, 0L};

B* ll_errm[] = {"** TEST: Test Error"};

B* ll_export[] = {
    "hi", (B*) op_test_hi,
    "libnum", (B*) op_test_libnum,
    "say5", (B*) op_say5,
    "sayerror", (B*) op_sayerror,
    "", NULL
};

L op_say5(void) 
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = NUM | LONGTYPE;
    ATTR(o1) = 0;
    LONG_VAL(o1) = 5L;
    FREEopds = o2;
    return OK;
}

L op_sayerror(void) {return ll_type | TEST_ERROR;}
