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
#ifndef TEST_H
#define TEST_H

#define PLUGIN_NAME test
#include "../src/plugin.h"

#define op_say5 EXPORTNAME(op_say5)
#define op_sayerror EXPORTNAME(op_sayerror)

#define TEST_ERROR 0x00000001L

// -- | 5
P op_say5(void);
P op_sayerror(void);

#endif
