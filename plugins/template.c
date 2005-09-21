#include "dm.h"
#inlcude "../src/plugin.h"

#define ERR1 0x000000001L
#define ERR2 0x000000002L

UL ll_type;

L op_template_hi(void) {return wrap_hi("Name vX");}
L op_template_libnum(void) {return wrap_libnum(ll_type);}

L ll_errc[] = {ERR1, ERR2, 0L};
B* ll_errm[] = {"Error1", "Error2"};
B* ll_export[] = {
    "hi", (B*) op_template_hi,
    "libnum", (B*) op_test_libnum,
    "", NULL};

    
