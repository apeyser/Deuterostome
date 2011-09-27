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
#ifndef DM_8_H
#define DM_8_H

#include "dm.h"

DM_HOT DLL_SCOPE P op_fax(void);
DLL_SCOPE P op_merge(void);
DLL_SCOPE P op_nextobject(void);
DLL_SCOPE P (*usedfd_func)(void);
DM_HOT DLL_SCOPE P op_used(void);
DM_HOT DLL_SCOPE P op_length(void); 
DM_HOT DLL_SCOPE P op_last(void);

#endif //DM_8_H
