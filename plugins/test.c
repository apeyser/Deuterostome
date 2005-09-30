#include <stdlib.h>
#include "test.h"

UL ll_type = 0;
L op_hi(void) {return wrap_hi("test V1");}
L op_libnum(void) {return wrap_libnum(ll_type);}
L ll_errc[] = {TEST_ERROR, 0L};
B* ll_errm[] = {"**Test Error: test"};
B* ll_export[] = {
	"hi", (B*) op_hi,
	"libnum", (B*) op_libnum,
	"say5", (B*) op_say5,
	"sayerror", (B*) op_sayerror,
	"say5_1", (B*) op_say5,
	"sayerror_1", (B*) op_sayerror,
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

L op_sayerror(void) {RETURN_ERROR(TEST_ERROR);}

#define x PRIVATENAME(x)
int x = 0;
