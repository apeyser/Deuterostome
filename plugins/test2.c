#include <stdlib.h>
#include <stdio.h>

#include "test2.h"

UP ll_type = 0;
P op_hi(void) {return wrap_hi("test2 V1");}
P op_libnum(void) {return wrap_libnum(ll_type);}
P ll_errc[] = {TEST2_ERROR, 0L};
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

static P retvalue = 6;
P op_INIT_(void) {
	retvalue++;
	return OK;
}

P op_FINI_(void) {
	fprintf(stderr, "Closing test2: retvalue=%i\n", retvalue);
	return OK;
}

P op_say5(void) 
{
    if (CEILopds < o2) return OPDS_OVF;
    TAG(o1) = NUM | LONGBIGTYPE;
    ATTR(o1) = 0;
    LONGBIG_VAL(o1) = retvalue;
    FREEopds = o2;
    return OK;
}

P op_sayerror(void) {RETURN_ERROR(TEST2_ERROR);}

