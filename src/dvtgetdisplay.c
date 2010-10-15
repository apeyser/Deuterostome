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

  if (fclose(stderr)) {
    perror("dvtdisplay close stderr failure");
    return 4;
  }

  return read(STDIN_FILENO,  &q, 1) != -1 ? 0 : errno == EINTR ? 0 : 5;
}
