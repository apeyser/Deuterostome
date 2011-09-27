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
#ifndef DM5_H
#define DM5_H

#include "dm.h"

DLL_SCOPE P op_die(void);
DLL_SCOPE P op_quit(void);
DLL_SCOPE DM_NORETURN void die(void);
DLL_SCOPE P quit(void);

/*-- relational, boolean, bitwise */ 
DM_HOT DLL_SCOPE P op_eq(void);
DM_HOT DLL_SCOPE P op_ne(void);
DLL_SCOPE P op_ge(void);
DLL_SCOPE P op_gt(void);
DLL_SCOPE P op_le(void);
DLL_SCOPE P op_lt(void);
DLL_SCOPE P op_and(void);
DM_HOT DLL_SCOPE P op_not(void);
DLL_SCOPE P op_or(void);
DLL_SCOPE P op_xor(void);
DLL_SCOPE P op_bitshift(void);

/*-- control */
DLL_SCOPE P op_start(void);
DM_HOT DLL_SCOPE P op_exec(void);
DM_HOT DLL_SCOPE P op_if(void);
DM_HOT DLL_SCOPE P op_ifelse(void);
DM_HOT DLL_SCOPE P op_for(void);
DM_HOT DLL_SCOPE P op_repeat(void);
DM_HOT DLL_SCOPE P op_loop(void);
DM_HOT DLL_SCOPE P op_forall(void);
DM_HOT DLL_SCOPE P op_exit(void);
DM_HOT DLL_SCOPE P op_stop(void);
DM_HOT DLL_SCOPE P op_stopped(void);
DM_HOT DLL_SCOPE P op_exitto(void);
DM_HOT DLL_SCOPE P op_exitlabel(void);
DLL_SCOPE P op_countexecstack(void);
DLL_SCOPE P op_execstack(void);

#endif //DM_H
