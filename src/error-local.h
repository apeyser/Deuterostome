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
#ifndef ERROR_H
#define ERROR_H

#include "dm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#define DM_IGNORE_RETURN(a) if (a);

static void va_dm_error_msg(int errnum,
			    const char* format,
			    va_list ap)
{
  char* str;
  int s;

  if (vasprintf(&str, format, ap) != -1) {
    DM_IGNORE_RETURN(write(DM_STDERR_FILENO, str, strlen(str)));
    free(str);
  }

  if (errnum) s = asprintf(&str, ": %s\n", strerror(errnum));
  else s = asprintf(&str, "\n");

  if (s != -1) {
    DM_IGNORE_RETURN(write(DM_STDERR_FILENO, str, strlen(str)));
    free(str);
  }
}

__attribute__ ((unused, format (printf, 2, 3)))
static void dm_error_msg(int errnum,
			 const char* format,
			 ...)
{
  va_list ap;
  va_start(ap, format);
  va_dm_error_msg(errnum, format, ap);
  va_end(ap);
}

__attribute__ ((noreturn))
static void va_dm_error(int errnum,
			const char* format,
			va_list ap)
{
  va_dm_error_msg(errnum, format, ap);
  exit(EXIT_FAILURE);
}


__attribute__ ((noreturn, unused, format (printf, 2, 3)))
static void dm_error(int errnum,
		     const char* format,
		     ...)
{
  va_list ap;
  va_start(ap, format);
  va_dm_error(errnum, format, ap);
}


#define error MAKEITANERROR

#define DEBUG_(t, format, ...) do {					\
    if (t) {								\
      dm_error_msg(0,							\
		   "%li: " format,					\
		   (long) getpid(), __VA_ARGS__);			\
    };									\
  } while (0)

#ifndef DEBUG_ACTIVE
#define DEBUG_ACTIVE 0
#endif //DEBUG_ACTIVE
#define DEBUG(...) DEBUG_(DEBUG_ACTIVE, __VA_ARGS__)
#define DEBUG_T(...) DEBUG_(1, __VA_ARGS__)

#endif //ERROR_H
