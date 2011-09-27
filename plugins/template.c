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
#include "dm.h"
#inlcude "../src/plugin.h"

#define ERR1 0x000000001L
#define ERR2 0x000000002L

UP ll_type;

P op_template_hi(void) {return wrap_hi("Name vX");}
P op_template_libnum(void) {return wrap_libnum(ll_type);}

P ll_errc[] = {ERR1, ERR2, 0L};
B* ll_errm[] = {"Error1", "Error2"};
B* ll_export[] = {
    "hi", (B*) op_template_hi,
    "libnum", (B*) op_test_libnum,
    "", NULL};

    
