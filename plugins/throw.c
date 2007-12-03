#include "throw.h"

UP ll_type = 0;
P op_hi(void) {return wrap_hi("throw V1");}
P op_libnum(void) {return wrap_libnum(ll_type);}
P ll_errc[] = {INV_THROW, 0L};
B* ll_errm[] = {
	"** throw: invalid throw, goes through abortmark or stopmark", 
	NULL
};
B* ll_export[] = {
	"hi", (B*) op_hi,
	"libnum", (B*) op_libnum,
	"caught", (B*) op_thrown,
	"throw", (B*) op_throw,
	"", NULL
};

const char* x_op_thrown_name = "x_op_thrown";

/*------------------------------------------- thrown
 * any_active | <any true> , false
 */
P op_thrown(void) {
	if (o_1 < FLOORopds) return OPDS_UNF;
	if (! (ATTR(o_1) & ACTIVE)) return OPD_ATR;

	if (x3 > CEILexecs) return EXECS_OVF;
	TAG(x1) = OP; ATTR(x1) = ACTIVE;
	OP_CODE(x1) = (P) x_op_thrown; OP_NAME(x1) = (P) x_op_thrown_name;
	moveframe(o_1, x2); ATTR(x2) = 0;
	FREEexecs = x3;
	FREEopds = o_1;
	return OK;
}

/*------------------------------------------- throw
 * any | <any true <<go up stack to catch>>
 */
P op_throw(void) {
    if (o_1 < FLOORopds) return OPDS_UNF;
    if (o2 > CEILopds) return OPDS_OVF;
    
    while ((FREEexecs-=FRAMEBYTES) >= FLOORexecs) {
        if (ATTR(x1) & ABORTMARK) {
            FREEexecs += FRAMEBYTES;
            RETURN_ERROR(INV_THROW);
        }
        if (ATTR(x1) & STOPMARK) {
            FREEexecs += FRAMEBYTES;
            RETURN_ERROR(INV_THROW);
        }

        if ((TAG(x1) == OP) && (OP_NAME(x1) == (P) x_op_thrown_name)) {
            TAG(o1) = BOOL; ATTR(o1) = 0;
            BOOL_VAL(o1) = TRUE;
            FREEopds = o2;
            return OK;
        }
    }
    
    return EXECS_UNF;
}
	
/*-------------------------------------------- x_unthrown
 *  -- | false 
 */
P x_op_thrown(void) {
	if (o2 > CEILopds) return OPDS_OVF;

	TAG(o1) = BOOL; ATTR(o1) = 0;
	BOOL_VAL(o1) = FALSE;
	FREEopds = o2;

	return OK;
}
