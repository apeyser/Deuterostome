#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include "dm.h"
#include "dmx.h"
#include "error-local.h"
#include "dqueen.h"
#include "dm-mpi.h"
#include "matrix.h"
#include "dm-nextevent.h"
#include "pluginlib.h"
#include "dregex.h"

struct emap {
  B* str;
  P val;
};

#include "dcoder.h"

int main(int argc, char* argv[]) {
  FILE* fd;
  struct emap* cemap;
  int i = 0;

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <path>\n", argv[0]);
    return 1;
  }

  if (! (fd = fopen(argv[1], "w")))
    error(1, errno, "Unable to open for writing: %s", argv[1]);

  if (fprintf(fd, "/ERRORS module 1000 dict dup begin\n") < 0)
    error(1, errno, "Unable to write line# %i", i);
  i++;

  for (cemap = emap; cemap->str; cemap++, i++)
    if (fprintf(fd, "  /%s %lldx def\n", 
		cemap->str, (long long) cemap->val) < 0)
      error(1, errno, "Unable to write line# %i", i);

  if (fprintf(fd, "end _module\n") < 0)
    error(1, errno, "Unable to write line# %i", i);

  return 0;
}
