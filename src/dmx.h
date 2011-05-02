#ifndef DMX_H
#define DMX_H

#include "dm.h"

DLL_SCOPE int xsocket;

#if ! DM_X_DISPLAY_MISSING

#define MAXDVTWINDOWS  ((P)127)
#define MAXCACHEDFONTS ((P)63)

DLL_SCOPE Display *dvtdisplay;
DLL_SCOPE B displayname[80];
DLL_SCOPE B xkbext;
DLL_SCOPE Screen *dvtscreen;
DLL_SCOPE Window dvtrootwindow;
DLL_SCOPE XWindowAttributes rootwindowattr;
DLL_SCOPE GC dvtgc;
DLL_SCOPE P ndvtwindows;
DLL_SCOPE P dvtwindows[MAXDVTWINDOWS];
DLL_SCOPE P ncachedfonts;

typedef struct {
  XFontStruct *fontstruct;
  B fontname[100];
} cachedfont;
DLL_SCOPE cachedfont cachedfonts[MAXCACHEDFONTS];

#endif //! X_DISPLAY_MISSING

/*----------------------- X windows operators */
P op_Xwindows(void);
P op_Xwindows_(void);
P op_Xdisplayname(void);
P op_Xconnect(void);
P op_Xdisconnect(void);
P op_screensize(void);
P op_makewindow(void);
P op_deletewindow(void);
P op_mapwindow(void);
P op_resizewindow(void);
P op_Xsync(void);
P op_mapcolor(void);
P op_drawline(void);
P op_drawsymbols(void);
P op_fillrectangle(void);
P op_drawtext(void);
P op_makewindowtop(void);
void bell(P percent);
P op_bell(void);

#endif


