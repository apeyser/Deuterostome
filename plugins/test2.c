#include <stdlib.h>
#include <stdio.h>

#include "test2.h"

UL ll_type = 0;
L op_hi(void) {return wrap_hi("test2 V1");}
L op_libnum(void) {return wrap_libnum(ll_type);}
L ll_errc[] = {TEST2_ERROR, 0L};
B* ll_errm[] = {"**Test Error: test2"};
B* ll_export[] = {
	"hi", (B*) op_hi,
	"libnum", (B*) op_libnum,
	"say5", (B*) op_say5,
	"sayerror", (B*) op_sayerror,
	"say5_2", (B*) op_say5,
	"sayerror_2", (B*) op_sayerror,
	"INIT_", (B*) op_INIT_,
	"FINI_", (B*) op_FINI_,
	"", NULL
};

static L retvalue = 6;
L op_INIT_(void) {
	retvalue++;
	return OK;
}

L op_FINI_(void) {
	fprintf(stderr, "Closing test2: retvalue=%i\n", retvalue);
	return OK;
}

L op_say5(void) 
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = NUM | LONGTYPE;
    ATTR(o1) = 0;
    LONG_VAL(o1) = retvalue;
    FREEopds = o2;
    return OK;
}

L op_sayerror(void) {RETURN_ERROR(TEST2_ERROR);}

