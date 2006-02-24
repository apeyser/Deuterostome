#ifndef DMX_H
#define DMX_H

#include "dm.h"

DLL_SCOPE int xsocket;

#if ! DM_X_DISPLAY_MISSING

#define MAXDVTWINDOWS  ((L)20)
#define MAXCACHEDFONTS ((L)10)

DLL_SCOPE Display *dvtdisplay;
DLL_SCOPE B displayname[80];
DLL_SCOPE Screen *dvtscreen;
DLL_SCOPE Window dvtrootwindow;
DLL_SCOPE XWindowAttributes rootwindowattr;
DLL_SCOPE GC dvtgc;
DLL_SCOPE L ndvtwindows;
DLL_SCOPE L dvtwindows[MAXDVTWINDOWS];
DLL_SCOPE L ncachedfonts;

typedef struct {
  XFontStruct *fontstruct;
  B fontname[100];
} cachedfont;
DLL_SCOPE cachedfont cachedfonts[MAXCACHEDFONTS];

#endif //! X_DISPLAY_MISSING

/*----------------------- X windows operators */
L op_Xwindows(void);
L op_Xwindows_(void);
L op_Xdisplayname(void);
L op_Xconnect(void);
L op_Xdisconnect(void);
L op_screensize(void);
L op_makewindow(void);
L op_deletewindow(void);
L op_mapwindow(void);
L op_resizewindow(void);
L op_Xsync(void);
L op_mapcolor(void);
L op_drawline(void);
L op_drawsymbols(void);
L op_fillrectangle(void);
L op_drawtext(void);
L op_makewindowtop(void);
L op_setinputfocus(void);

#endif


