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
#ifndef DM7_H
#define DM7_H

#include "dm.h"

/*-- time/date and file access  */
DLL_SCOPE P op_random(void);
DLL_SCOPE P op_random_init(void);
DLL_SCOPE P op_gettime(void);
DLL_SCOPE P op_gettimeofday(void);
DLL_SCOPE P op_profiletime(void);
DLL_SCOPE P op_localtime(void);
DLL_SCOPE P op_sleep(void);
DLL_SCOPE P op_getwdir(void);
DLL_SCOPE P op_setwdir(void);
DLL_SCOPE P op_writefile(void);
DLL_SCOPE P op_readfile(void);
DLL_SCOPE P op_findfiles(void);
DLL_SCOPE P op_findfile(void);
DLL_SCOPE P op_readboxfile(void);
DLL_SCOPE P op_umask(void);
DLL_SCOPE P op_writeboxfile(void); 
DLL_SCOPE P op_transcribe(void);
DLL_SCOPE P op_tostderr(void);

#endif //DM7_H
