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
#ifndef DM_SEM_H
#define DM_SEM_H

#include "dm.h"

#ifdef DM_ENABLE_SEM //intentional ifdef for undef
#if DM_DISABLE_SEM
#undef DM_ENABLE_SEM
#ifdef DM_HAVE_CONFIG_H
#undef ENABLE_SEM
#endif //DM_HAVE_CONFIG_H
#endif //DM_DISABLE_SEM
#endif //DM_ENABLE_SEM

#if DM_ENABLE_SEM
P _do_inter_lock_init(void);
P _do_inter_lock_reset(void);
P _do_inter_lock(void);
P _do_inter_unlock(BOOLEAN force);

P op_inter_lock(void);
P op_inter_unlock(void);
P op_inter_lock_set(void);
#endif //DM_ENABLE_SEM

#endif //DM_SEM_H
