/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
#include <stdlib.h>
#include <stdio.h>

#include "test2.h"

UP ll_type = 0;
P op_hi(void) {return wrap_hi((B*)"test2 V1");}
P op_libnum(void) {return wrap_libnum(ll_type);}
P ll_errc[] = {TEST2_ERROR, 0L};
B* ll_errm[] = {(B*)"**Test Error: test2"};
B* ll_export[] = {
	(B*)"hi", (B*) op_hi,
	(B*)"libnum", (B*) op_libnum,
	(B*)"say5", (B*) op_say5,
	(B*)"sayerror", (B*) op_sayerror,
	(B*)"say5_2", (B*) op_say5,
	(B*)"sayerror_2", (B*) op_sayerror,
	(B*)"INIT_", (B*) op_INIT_,
	(B*)"FINI_", (B*) op_FINI_,
	(B*)"", NULL
};

static P retvalue = 6;
P op_INIT_(void) {
	retvalue++;
	return OK;
}

P op_FINI_(void) {
  fprintf(stderr, "Closing test2: retvalue=%lli\n", (long long) retvalue);
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

