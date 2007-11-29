#include <stdlib.h>
#include "test.h"

UP ll_type = 0;
P op_hi(void) {return wrap_hi("test V1");}
P op_libnum(void) {return wrap_libnum(ll_type);}
P ll_errc[] = {TEST_ERROR, 0L};
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

P op_say5(void) 
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = NUM | LONGBIGTYPE;
    ATTR(o1) = 0;
    LONGBIG_VAL(o1) = 5L;
    FREEopds = o2;
    return OK;
}

P op_sayerror(void) {RETURN_ERROR(TEST_ERROR);}

#define x PRIVATENAME(x)
int x = 0;
