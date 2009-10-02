#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
  const char* display = getenv("DISPLAY");
  if (! display) {
    fprintf(stderr, "Display not defined!\n");
    return 1;
  }

  if (printf("%s\n", display) < 0) {
    fprintf(stderr, "Unable to print $DISPLAY\n");
    return 2;
  }

  pause();
  return 0;
}
