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
#ifndef THROW_H
#define THROW_H

#define PLUGIN_NAME throw
#include "../src/plugin.h"

#define op_thrown EXPORTNAME(op_thrown)
#define op_throw  EXPORTNAME(op_throw)
#define x_op_thrown PRIVATENAME(x_op_thrown)
#define x_op_thrown_name PRIVATENAME(x_op_thrown_name)

P op_throw(void);
P op_thrown(void);
P x_op_thrown(void);
const char* x_op_uncaught_name;

#define INV_THROW 0x01L //invalid throw

#endif //THROW_H
