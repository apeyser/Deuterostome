#ifndef ERROR_H
#define ERROR_H

#ifdef HAVE_ERROR_H
#include <error.h>
#else //HAVE_ERROR_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
__attribute__ ((unused))
  static void error(int status, int errnum, 
		    const char* format, ...)
{
    va_list ap;
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, ": %s\n", strerror(errnum));
    if (status) exit(status);
}
#endif //HAVE_ERROR_H

#endif //ERROR_H
