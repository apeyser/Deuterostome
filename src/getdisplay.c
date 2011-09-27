/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
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
