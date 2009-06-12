#ifndef DM_SNPRINTF
#define DM_SNPRINTF

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
