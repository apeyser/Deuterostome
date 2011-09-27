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
#ifndef DQUEEN_H
#define DQUEEN_H

#include "dm.h"

#if DM_ENABLE_RTHREADS

DLL_SCOPE P op_rthreads(void);
DLL_SCOPE P op_checkrthreads(void);
DLL_SCOPE P op_makerthreads(void);
DLL_SCOPE P op_rsend(void);
DLL_SCOPE P op_rsendsig(void);

DLL_SCOPE void rthreads_init(void);
DLL_SCOPE P killrthreads(void);

#endif //DM_ENABLE_RTHREADS

#endif //DQUEEN_H
