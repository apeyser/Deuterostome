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
#include <stdlib.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <stdarg.h>

#if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
#define ATTR(attr) __attribute__ (( __##attr##__ ))
#else
#define ATTR(attr)
#endif

static Display* disp;

ATTR(noreturn)
static void err(int r, const char* str, ...) {
  char buf[1024];
  va_list ap;
  va_start(ap, str);
  vsnprintf(buf, sizeof(buf), str, ap);
  va_end(ap);
  buf[sizeof(buf)-1] = '\0';
  fprintf(stderr, "%s\n", buf);
  exit(r);
}

static int xerr(Display* disp, XErrorEvent* ev) {
  char buf[1024];
  XGetErrorText(disp, ev->error_code, buf, sizeof(buf));
  err(1, "%s", buf);
  return 0;
}

static void closedisplay(void) {
  if (XCloseDisplay(disp)) err(6, "Unable to close display");
}

int main(int argc, char* argv[]) {
  Window wid;
  char* endptr;

  if (argc != 2) err(2, "Invalid number of args %i", argc);
  wid = (Window) strtol(argv[1], &endptr, 0);
  if (! *argv[1] || *endptr) err(3, "Invalid window id: %s", argv[1]);

  if (! (disp = XOpenDisplay(NULL))) err(4, "Unable to connect to display");

  if (atexit(closedisplay)) err(5, "Unable to set exit function");
  XSetErrorHandler(xerr);

  XSetInputFocus(disp, wid, RevertToParent, CurrentTime);

  return 0;
}
