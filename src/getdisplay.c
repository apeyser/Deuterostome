#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

int main(int argc, char* argv[]) {
  const char* display = getenv("DISPLAY");
  char q;

  if (! display) {
    fprintf(stderr, "Display not defined!\n");
    fprintf(stdout, "\n");
    return 1;
  }

  if (printf("%s\n", display) < 0) {
    fprintf(stderr, "Unable to print $DISPLAY\n");
    fprintf(stdout, "\n");
    return 2;
  }

  if (fclose(stdout)) {
    perror("dvtdisplay close stdout failure");
    return 3;
  }

  if (read(STDIN_FILENO,  &q, 1) == -1 && errno != EINTR) {
    perror("dvtdisplay read failure");
    return 4;
  }

  return 0;
}
