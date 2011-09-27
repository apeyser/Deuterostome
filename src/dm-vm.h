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
#ifndef DM_VM_H
#define DM_VM_H

#include "dm.h"

#include "dm-dvt-vm.h"

DLL_SCOPE P op_vmresize(void);
DLL_SCOPE void maketinysetup(void);
DLL_SCOPE P op_vmresize_(void);
DLL_SCOPE P op_lock(void);
DLL_SCOPE P op_unlock(void);
DLL_SCOPE P op_locked(void);
DLL_SCOPE P op_serialize(void);
DLL_SCOPE P op_halt(void);
DLL_SCOPE P x_op_halt(void);
DLL_SCOPE P op_continue(void);

#define MSF_SIZE (100000)
DLL_SCOPE B msf[FRAMEBYTES];
DLL_SCOPE B cmsf[FRAMEBYTES];

#endif //DM_VM_H
