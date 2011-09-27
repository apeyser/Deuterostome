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
#ifndef DM_SNPRINTF
#define DM_SNPRINTF

#include "dm.h"

#include <stdarg.h>
#include <stdio.h>

static int dm_snprintf(char *str, size_t size, const char *format, ...) 
  __attribute__ ((__unused__, __format__ (__printf__, 3, 4)));

static int dm_snprintf(char *str, size_t size, const char *format, ...) {
  va_list ap;
  int nb;
  va_start(ap, format);
  if ((nb = vsnprintf(str, size, format, ap)) > (int) size) nb = size;
  va_end(ap);
  return nb;
}
  
  

#endif
