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
#ifndef DM_DPAWN_H
#define DM_DPAWN_H

#include "dm.h"

DLL_SCOPE P op_vmresize(void);
DLL_SCOPE void run_dpawn_mill(void) __attribute__ ((__noreturn__));
DLL_SCOPE void makeerror(P retc, B* error_source);
DLL_SCOPE P op_error(void);
DLL_SCOPE P op_errormessage(void);
DLL_SCOPE P op_toconsole(void);
DLL_SCOPE P op_groupconsole(void);

DLL_SCOPE P op_mpiprobe(void);
DLL_SCOPE P op_mpiiprobe(void);
DLL_SCOPE P op_mpisend(void);
DLL_SCOPE P op_mpirecv(void);
DLL_SCOPE P op_mpibroadcast(void);
DLL_SCOPE P op_mpibarrier(void);
DLL_SCOPE P op_rsend(void);
DLL_SCOPE P op_mpirank(void);
DLL_SCOPE P op_mpisize(void);

DLL_SCOPE P recur_dpawn_mill(B* execf);


#endif //DM_DPAWN_H
