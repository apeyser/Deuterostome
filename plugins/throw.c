#include "throw.h"

UL ll_type = 0;
L op_hi(void) {return wrap_hi("throw V1");}
L op_libnum(void) {return wrap_libnum(ll_type);}
L ll_errc[] = {INV_THROW, 0L};
B* ll_errm[] = {
	"** throw: invalid throw, goes through abortmark or stopmark", 
	NULL
};
B* ll_export[] = {
	"hi", (B*) op_hi,
	"libnum", (B*) op_libnum,
	"caught", (B*) op_caught,
	"throw", (B*) op_throw,
	"", NULL
};

const char* x_op_uncaught_name = "x_op_uncaught";

/*------------------------------------------- caught
 * any_active /name | bool
 */
L op_caught(void) {
	if (o_2 < FLOORopds) return OPDS_UNF;
	if (ATTR(o_1) & ACTIVE) return OPD_ATR;
	if (TAG(o_1) != NAME) return OPD_CLA;
	if (! (ATTR(o_2) & ACTIVE)) return OPD_ATR;

	if (x4 > CEILexecs) return EXECS_OVF;
	moveframe(o_1, x1); ATTR(x1) = 0;
	TAG(x2) = OP; ATTR(x2) = ACTIVE;
	OP_CODE(x2) = (L) x_op_uncaught; OP_NAME(x2) = (L) x_op_uncaught_name;
	moveframe(o_2, x3);
	FREEexecs = x4;
	FREEopds = o_2;
	return OK;
}

/*------------------------------------------- throw
 * /name | -- <<go up stack to catch>>
 */
L op_throw(void) {
	B* xframe;

	if (o_1 < FLOORopds) return OPDS_UNF;
	if (ATTR(o_1) & ACTIVE) return OPD_ATR;
	if (TAG(o_1) != NAME) return OPD_CLA;

	while ((FREEexecs-=FRAMEBYTES) > FLOORexecs) {
		if (ATTR(x1) & ABORTMARK) {
			FREEexecs += FRAMEBYTES;
			RETURN_ERROR(INV_THROW);
		}
		if (ATTR(x1) & STOPMARK) {
			FREEexecs += FRAMEBYTES;
			RETURN_ERROR(INV_THROW);
		}
		if ((TAG(x1) != OP) || (OP_NAME(x1) != (L) x_op_uncaught_name))
			continue;

		FREEexecs -= FRAMEBYTES;
		if (TAG(x1) != NAME || ATTR(x1)) return EXECS_COR;

		if (matchname(x1, o_1)) {
			TAG(o_1) = BOOL; ATTR(o_1) = 0;
			BOOL_VAL(o_1) = TRUE;
			return OK;
		}
	}

	if (FREEexecs < FLOORexecs) FREEexecs = FLOORexecs;
	return EXECS_UNF;
}
	
/*-------------------------------------------- x_uncaught
 * <</name on execstack>> -- | false <</name popped>>
 */
L x_op_uncaught(void) {
	if (o2 > CEILopds) return OPDS_OVF;
	if (FLOORexecs > x_1) return EXECS_UNF;
	FREEexecs = x_1;
	if (ATTR(x1) || TAG(x1) != NAME) return EXECS_COR;

	TAG(o1) = BOOL; ATTR(o1) = 0;
	BOOL_VAL(o1) = FALSE;
	FREEopds = o2;

	return OK;
}
