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
#ifndef DM6_H
#define DM6_H

#include "dm.h"

DLL_SCOPE P op_checkFPU(void);
DLL_SCOPE P op_neg(void);
DLL_SCOPE P op_abs(void);
DLL_SCOPE P op_thearc(void);
DLL_SCOPE P op_add(void);
DLL_SCOPE P op_mod(void);
DLL_SCOPE P op_sub(void);
DLL_SCOPE P op_mul(void);
DLL_SCOPE P op_div(void);
DLL_SCOPE P op_sqrt(void);
DLL_SCOPE P op_exp(void);
DLL_SCOPE P op_ln(void);
DLL_SCOPE P op_lg(void);
DLL_SCOPE P op_pwr(void);
DLL_SCOPE P op_cos(void);
DLL_SCOPE P op_sin(void);
DLL_SCOPE P op_tan(void);
DLL_SCOPE P op_atan(void);
DLL_SCOPE P op_floor(void);
DLL_SCOPE P op_ceil(void);
DLL_SCOPE P op_asin(void);
DLL_SCOPE P op_acos(void);

DLL_SCOPE P op_class(void);
DLL_SCOPE P op_type(void);
DLL_SCOPE P op_readonly(void);
DLL_SCOPE P op_active(void);
DLL_SCOPE P op_tilde(void);
DLL_SCOPE P op_mkread(void);
DLL_SCOPE P op_mkact(void);
DLL_SCOPE P op_mkpass(void);
DLL_SCOPE P op_mktilde(void);
DLL_SCOPE P op_ctype(void);
DLL_SCOPE P op_parcel(void);
DM_HOT DLL_SCOPE P op_text(void);
DM_HOT DLL_SCOPE P op_number(void);
DLL_SCOPE P (*tokenfd_func)(void);
DLL_SCOPE P op_token(void);
DLL_SCOPE P op_search(void);
DLL_SCOPE P op_anchorsearch(void);

DM_HOT DLL_SCOPE P op_copy(void);
DM_HOT DLL_SCOPE P op_save(void);
DM_HOT DLL_SCOPE P op_capsave(void);
DM_HOT DLL_SCOPE P op_restore(void);
DLL_SCOPE P op_vmstatus(void);
DM_HOT DLL_SCOPE P op_bind(void);

DLL_SCOPE P op_socketval(void);

DLL_SCOPE P (*cleanupfd_func)(void);

#endif //DM6_H
