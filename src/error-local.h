#ifndef ERROR_H
#define ERROR_H

#include "dm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#define DM_IGNORE_RETURN(a) if (a);

__attribute__ ((unused, format (printf, 3, 4)))
static void error_local(int __status, 
			int __errnum, 
			const char* __format, ...)
{
  char* str;
  int s;

  va_list ap;
  va_start(ap, __format);
  if (vasprintf(&str, __format, ap) != -1) {
    DM_IGNORE_RETURN(write(DM_STDERR_FILENO, str, strlen(str)));
    free(str);
  }
  if (__errnum) s = asprintf(&str, ": %s\n", strerror(__errnum));
  else s = asprintf(&str, "\n");
  if (s != -1) {
    DM_IGNORE_RETURN(write(DM_STDERR_FILENO, str, strlen(str)));
    free(str);
  }
  if (__status) exit(__status);
}

#define error MAKEITANERROR

#ifndef DEBUG_ACTIVE
#define DEBUG_ACTIVE 0
#endif //DEBUG_ACTIVE
#define DEBUG(format, ...) do {						\
    if (DEBUG_ACTIVE) {							\
      error_local(0, 0, "%li: " format,					\
		  (long) getpid(), __VA_ARGS__);			\
    };									\
  } while (0)

#endif //ERROR_H
