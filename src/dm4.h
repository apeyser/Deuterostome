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
#ifndef DM_4_H
#define DM_4_H

#include "dm.h"

DM_HOT DLL_SCOPE P op_pop(void);
DM_HOT DLL_SCOPE P op_push(void);
DM_HOT DLL_SCOPE P op_exch(void);
DM_HOT DLL_SCOPE P op_dup(void);
DM_HOT DLL_SCOPE P op_index(void);
DM_HOT DLL_SCOPE P op_roll(void);
DM_HOT DLL_SCOPE P op_clear(void);
DLL_SCOPE P op_count(void);
DLL_SCOPE P op_cleartomark(void);
DM_HOT DLL_SCOPE P op_counttomark(void);

/*-- dictionary, array, list */
DM_HOT DLL_SCOPE P op_currentdict(void);
DM_HOT DLL_SCOPE P op_closelist(void); 
DM_HOT DLL_SCOPE P op_openlist(void);
DM_HOT DLL_SCOPE P op_dict(void);
DLL_SCOPE P op_cleardict(void);
DM_HOT DLL_SCOPE P op_array(void);
DM_HOT DLL_SCOPE P op_list(void);
DM_HOT DLL_SCOPE P op_begin(void);
DM_HOT DLL_SCOPE P op_end(void);
DM_HOT DLL_SCOPE P op_def(void);
DM_HOT DLL_SCOPE P op_name(void);
DM_HOT DLL_SCOPE P op_find(void);
DM_HOT DLL_SCOPE P op_get(void);
DM_HOT DLL_SCOPE P op_put(void);
DM_HOT DLL_SCOPE P op_known(void);
DM_HOT DLL_SCOPE P op_getinterval(void);
DLL_SCOPE P op_countdictstack(void);
DLL_SCOPE P op_dictstack(void);
/*-- VM and miscellaneous */
DM_HOT DLL_SCOPE P op_null(void);


#endif //DM_4_h
