#ifndef DMX_H
#define DMX_H

#include "dm.h"

DLL_SCOPE int xsocket;

#define NO_XWINDOWS 0x00000A01L /* X windows unavailable                 */
#define X_ERR       0x00000A02L /* X lib error                           */
#define X_BADFONT   0x00000A03L /* X font does not exist                 */
#define X_BADHOST   0x00000A04L /* X server cannot be connected          */
#define X_SEC_MISS  0x00000A05L /* X security extension missing          */
#define X_SEC_GEN   0x00000A06L /* X generate failure                    */
#define X_SEC_REV   0x00000A07L /* X revoke failure                      */
#define X_SEC_LIB   0x00000A08L /* X security lib missing                */

#if ! DM_X_DISPLAY_MISSING

#define MAXDVTWINDOWS  ((P)20)
#define MAXCACHEDFONTS ((P)10)

DLL_SCOPE Display *dvtdisplay;
DLL_SCOPE B displayname[80];
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
P op_setinputfocus(void);

P op_xauthrev(void);
P op_xauthset(void);
P op_xauthgen(void);
P op_xauth(void);

#endif


