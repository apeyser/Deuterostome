#ifndef ERROR_H
#define ERROR_H

#ifdef OSX
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
__attribute__ ((unused))
  static void error(int status, int errnum, 
		    const char* format __attribute__((unused)), ...)
{
  perror(strerror(errnum));
  if (status) exit(status);
} 
#else
#include <error.h>
#endif

#endif
