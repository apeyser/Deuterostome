#ifndef ERROR_H
#define ERROR_H

#ifdef DM_HAVE_ERROR_H
#include <error.h>
#else //DM_HAVE_ERROR_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
__attribute__ ((unused, format (printf, 3, 4)))
  static void error(int __status, 
                    int __errnum, 
                    const char* __format, ...)
{
    va_list ap;
    va_start(ap, __format);
    vfprintf(stderr, __format, ap);
    if (__errnum) fprintf(stderr, ": %s\n", strerror(__errnum));
    if (__status) exit(__status);
}
#endif //DM_HAVE_ERROR_H

#endif //ERROR_H
