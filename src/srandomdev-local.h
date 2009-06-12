#ifndef SRANDOM_H
#define SRANDOM_H

#include "dm-config.h"
#include <stdlib.h>
#include <unistd.h>

#if !DM_HAVE_SRANDOMDEV

#include <errno.h>
#include <time.h>
#include <fcntl.h>

DM_INLINE_STATIC void srandomdev(void) 
{
  int unsigned long seed;
  int off = 0;
  int r;
  
  int fd = open("/dev/random", O_RDONLY, 0);
  if (fd == -1) {
    srandom(time(NULL));
    return;
  }

  while (off < sizeof(seed)) {
      if ((r = read(fd, (char*) &seed + off, sizeof(seed))) == -1) {
        if (errno != EINTR) {
          seed = time(NULL);
          break;
        }
        r = 0;
      }
      off += r;
    }
				
  while (close(fd) && errno == EINTR);
  srandom(seed);
}


#endif //!DM_HAVE_SRANDOMDEV

#endif //SRANDOM_H
