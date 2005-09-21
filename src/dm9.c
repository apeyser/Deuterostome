/*==================== D machine Rev3.0: dm9.c ==========================
    X windows operators:

     - Xwindows
     - Xsync
     - makewindow
     - deletewindow
     - mapwindow
     - resizewindow
     - mapcolor
     - drawline
     - drawsymbols
     - fillrectangle
     - drawtext

     NOTE: X windows events are detected in the supervisor of the D mill
           for dnodes, or the 'nextevent' operator in a dvt.
*/

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <strings.h>
#include <string.h>

#include "dm.h"
#include "dmx.h"

int xsocket = -1;

#if ! X_DISPLAY_MISSING
Display *dvtdisplay;
B displayname[80] = "";
Screen *dvtscreen;
Window dvtrootwindow;
XWindowAttributes rootwindowattr;
GC dvtgc;
L ndvtwindows;
L dvtwindows[MAXDVTWINDOWS];
L ncachedfonts;
cachedfont cachedfonts[MAXCACHEDFONTS];
#endif

/*----------------- some details on operands ----------------------------

  Coordinates to be used by X windows operators can be specified in a
  variety of formats (referred to as 'xy' below):

   < x y ... >
   [ x y ... ]
   < x ... > < y ... >
   < x ... > [ y ... ]
   [ x ... ] < y ... >
   [ x ... ] [ y ... ]
*/

static L wid, colidx;
static L x,y,s;

/*-------------------------- support routines -------------------------------*/

/*------ copy_array
  - copies a source array of any type into temporary long array
*/

static L copy_array(B *sf, B **df, B **freevm)
{
B *f;

f = (B *)*freevm;
if ((*freevm += FRAMEBYTES + ARRAY_SIZE(sf)*VALUEBYTES(WORDTYPE))
     > CEILvm) return(VM_OVF);
TAG(f) = ARRAY | WORDTYPE; ATTR(f) = 0;
VALUE_BASE(f) = (L)f + FRAMEBYTES; ARRAY_SIZE(f) = ARRAY_SIZE(sf);
MOVE(sf,f); *df = f;
return(OK);
}

/*------ copy_list
  - copies a source list of numerals into temporary long array
*/

static L copy_list(B *sf, B **df, B **freevm)
{
B *f, *cf; L k, n;

f = (B *)*freevm;
n = (LIST_CEIL(sf) - VALUE_BASE(sf)) / FRAMEBYTES;
if ((*freevm += FRAMEBYTES + n * VALUEBYTES(WORDTYPE)) > CEILvm)
    return(VM_OVF);
TAG(f) = ARRAY | WORDTYPE; ATTR(f) = 0;
VALUE_BASE(f) = (L)f + FRAMEBYTES; ARRAY_SIZE(f) = 1;
cf = (B *)VALUE_BASE(sf); k = n;
while (k)
   { if (CLASS(cf) != NUM) return(OPD_CLA);
     MOVE(cf,f); VALUE_BASE(f) += VALUEBYTES(WORDTYPE);
     cf += FRAMEBYTES; k--;
   }
VALUE_BASE(f) = (L)f + FRAMEBYTES; ARRAY_SIZE(f) = n;
*df = f;
return(OK);
}

/*------ merge
  - combines separate x and y long arrays into one xy long array
*/

static L merge(B *xf, B *yf, B **xyf, B **freevm)
{
B *f; W *xp, *yp, *dp; L n;

f = (B *)*freevm;
if ((*freevm += FRAMEBYTES + 2 * ARRAY_SIZE(xf)*VALUEBYTES(WORDTYPE))
     > CEILvm) return(VM_OVF);
TAG(f) = ARRAY | WORDTYPE; ATTR(f) = 0;
VALUE_BASE(f) = (L)f + FRAMEBYTES; ARRAY_SIZE(f) = n = 2 * ARRAY_SIZE(xf);
xp = (W *)VALUE_BASE(xf); yp = (W *)VALUE_BASE(yf);
dp = (W *)VALUE_BASE(f);
while (n)
  { *(dp++) = *(xp++); *(dp++) = *(yp++); n--; }
*xyf = f;
return(OK);
}

/*------ xy
  - copies the source array(s) while converting into long type
  - merges separate x,y arrays into one
  - new arrays are stored in temporary VM space
*/

static L xy(B **xyf, B **freevm)
{
L retc; B *xf, *yf;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_1) == ARRAY)
   { if (o_2 < FLOORopds) goto do_one_array;
     if (CLASS(o_2) == ARRAY) goto do_array_array;
     if (CLASS(o_2) == LIST) goto do_array_list;
     goto do_one_array;
   }
else if (CLASS(o_1) == LIST)
   { if (o_2 < FLOORopds) goto do_one_list;
     if (CLASS(o_2) == ARRAY) goto do_list_array;
     if (CLASS(o_2) == LIST) goto do_list_list;
     goto do_one_list;
   }
else return(OPD_CLA);

do_one_array:
if (ARRAY_SIZE(o_1) & 1) return(RNG_CHK);
if ((retc = copy_array(o_1,xyf,freevm)) != OK) return(retc);
FREEopds = o_1; return(OK);

do_array_array:
if ((retc = copy_array(o_2,&xf,freevm)) != OK) return(retc);
if ((retc = copy_array(o_1,&yf,freevm)) != OK) return(retc);
if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return(RNG_CHK);
if ((retc = merge(xf,yf,xyf,freevm)) != OK) return(retc);
FREEopds = o_2; return(OK);

do_array_list:
if ((retc = copy_list(o_2,&xf,freevm)) != OK) return(retc);
if ((retc = copy_array(o_1,&yf,freevm)) != OK) return(retc);
if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return(RNG_CHK);
if ((retc = merge(xf,yf,xyf,freevm)) != OK) return(retc);
FREEopds = o_2; return(OK);

do_one_list:
if (((LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES) & 1) return(RNG_CHK);
if ((retc = copy_list(o_1,xyf,freevm)) != OK) return(retc);
FREEopds = o_1; return(OK);

do_list_array:
if ((retc = copy_array(o_2,&xf,freevm)) != OK) return(retc);
if ((retc = copy_list(o_1,&yf,freevm)) != OK) return(retc);
if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return(RNG_CHK);
if ((retc = merge(xf,yf,xyf,freevm)) != OK) return(retc);
FREEopds = o_2; return(OK);

do_list_list:
if ((retc = copy_list(o_2,&xf,freevm)) != OK) return(retc);
if ((retc = copy_list(o_1,&yf,freevm)) != OK) return(retc);
if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return(RNG_CHK);
if ((retc = merge(xf,yf,xyf,freevm)) != OK) return(retc);
FREEopds = o_2; return(OK);
}

/*------- evaluate color operand */

L coloropd()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&colidx)) return(UNDF_VAL);
  XSetForeground(dvtdisplay,dvtgc,colidx);
  FREEopds = o_1;
  return(OK);
#endif
}

/*----------------------------------------------- Xwindows

   -- | bool

  - reports whether Xwindows is available (if not, X windows operators
    will return the error NO_XWINDOWS)
*/

L op_Xwindows()
{
  if (o1 >= CEILopds) return(OPDS_OVF);
  TAG(o1) = BOOL; ATTR(o1) = 0;
#if X_DISPLAY_MISSING
	BOOL_VAL(o1) = FALSE;
#else
  BOOL_VAL(o1) = (dvtdisplay != NULL);
#endif
  FREEopds = o2;
  return(OK);
}

/*------------------------------------------------ screensize

   -- | width height

   - reports the screen dimensions
*/

L op_screensize()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o2 > CEILopds) return(OPDS_OVF);
  TAG(o1) = TAG(o2) = NUM | LONGTYPE; ATTR(o1) = ATTR(o2) = 0;
  LONG_VAL(o1) = rootwindowattr.width;
  LONG_VAL(o2) = rootwindowattr.height;
  FREEopds = o3;
  return(OK);
#endif
}

/*------------------------------------------------ makewindow

    xy (name) (iconname) | window# 

   - creates a new window in rectangle (xy: x, y, width, height)
   - the window manager will dress the window using name
     (no more than 30 characters) and iconname ( <=12 characters)
   - 'nextevent' will identify window by the window#
*/

L op_makewindow()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  static XClassHint classhint = {"d_machine", "d_machine"};
  L retc; W *pxy;
  B *xyf, *freevm, nstr[31], icstr[13],
    *pn[1] = { nstr }, *pic[1] = { icstr };
  XSetWindowAttributes attr;
  XTextProperty wname, icname;
	Atom atom;

  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  attr.event_mask = ButtonPressMask | ExposureMask |
    StructureNotifyMask;
  attr.override_redirect = True;

  if (o_3 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if (ARRAY_SIZE(o_1) > 12) return(RNG_CHK);
  moveB((B *)VALUE_BASE(o_1),icstr,ARRAY_SIZE(o_1));
  icstr[ARRAY_SIZE(o_1)] = '\000';
  if (XStringListToTextProperty((char**) pic,1,&icname) == 0)
    return(X_ERR);
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if (ARRAY_SIZE(o_2) > 30) return(RNG_CHK);
  moveB((B *)VALUE_BASE(o_2),nstr,ARRAY_SIZE(o_2));
  nstr[ARRAY_SIZE(o_2)] = '\000';
  if (XStringListToTextProperty((char**)pn,1,&wname) == 0)
    return(X_ERR);
  FREEopds = o_2;
  freevm = FREEvm;
  if ((retc = xy(&xyf,&freevm)) != OK) return(retc);
  pxy = (W *)VALUE_BASE(xyf);
  if (ARRAY_SIZE(xyf) != 4) return(RNG_CHK);
  if (ndvtwindows >= MAXDVTWINDOWS) return(RNG_CHK);

  wid = XCreateWindow(dvtdisplay, dvtrootwindow, pxy[0], pxy[1],
		      pxy[2], pxy[3], 0, CopyFromParent,
		      InputOutput, CopyFromParent,
		      CWEventMask, &attr);
  //XSetTransientForHint(dvtdisplay, wid, dvtrootwindow);
  XSetWMName(dvtdisplay,wid,&wname);
  XSetWMIconName(dvtdisplay,wid,&icname);
  XSetClassHint(dvtdisplay,wid,&classhint);
	atom = XInternAtom(dvtdisplay, "WM_DELETE_WINDOW", False);
	XSetWMProtocols(dvtdisplay, wid, &atom, 1);
  dvtwindows[ndvtwindows++] = wid;
  TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
  LONG_VAL(o1) = wid;
  FREEopds = o2;
  return(OK);    
#endif
}

/*------------------------------------------------ deletewindow

   window# | --

   - deletes an existing window
   - does nothing if the window does not exist
*/

L op_deletewindow()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  L k;
  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&wid)) return(UNDF_VAL);
  k = 0;
  while (1) { if (k >= ndvtwindows) { FREEopds = o_1; return(OK); }
              if (dvtwindows[k] == wid) break; k++;
            }
  --ndvtwindows;
  for ( ; k<ndvtwindows; k++) dvtwindows[k] = dvtwindows[k+1];
  XDestroyWindow(dvtdisplay,wid);
  FREEopds = o_1;
  return(OK);
#endif
}

/*------------------------------------------------ mapwindow

   window# bool | --

   - true: shows the window as the top window on the screen 
   - false: hides the window
   - does nothing if window does not exist 
*/

L op_mapwindow()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  L k;
  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_2 < FLOORopds) return(OPDS_UNF);
  if (CLASS(o_2) != NUM) return(OPD_CLA);
  if (!VALUE(o_2,&wid)) return(UNDF_VAL);
  if (CLASS(o_1) != BOOL) return(OPD_CLA);
  k = 0;
  while (1) { if (k >= ndvtwindows) { FREEopds = o_2; return(OK); }
              if (dvtwindows[k] == wid) break; k++;
            }
  if (BOOL_VAL(o_1))
    XMapRaised(dvtdisplay,wid);
  else
    XUnmapWindow(dvtdisplay,wid);
  FREEopds = o_2;
  return(OK);
#endif
}

/*------------------------------------------------- resizewindow
   window# width height | --
*/

L op_resizewindow()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  L width, height, k;

  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_3 < FLOORopds) return(OPDS_UNF);
  if (CLASS(o_3) != NUM) return(OPD_CLA);
  if (!VALUE(o_3,&wid)) return(UNDF_VAL);
  if (CLASS(o_2) != NUM) return(OPD_CLA);
  if (!VALUE(o_2,&width)) return(UNDF_VAL);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&height)) return(UNDF_VAL);
  k = 0;
  while (1) { if (k >= ndvtwindows) { FREEopds = o_3; return(OK); }
              if (dvtwindows[k] == wid) break; k++;
            }
  XResizeWindow(dvtdisplay,wid,width,height);
  FREEopds = o_3;
  return(OK);
#endif
}

/*------------------------------------------------- Xsync
   -- | --

   Flushes buffered graphics instruction, thus rendering their effects.
*/

L op_Xsync()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  XFlush(dvtdisplay);
  return OK;
#endif
}

/*------------------------------------------------- mapcolor
   < red green blue > | color_index

   Color contributions are expressed as fractions between 0.0 and 1.0.
   Translates an RGB color into a color map index.
*/

L op_mapcolor()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  XColor color; B f[FRAMEBYTES]; D val[3]; L k;
 
  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_1< FLOORopds) return(OPDS_UNF);
  if (CLASS(o_1) != ARRAY) return(OPD_CLA);
  if (ARRAY_SIZE(o_1) != 3) return(RNG_CHK);
  TAG(f) = (ARRAY | DOUBLETYPE); ATTR(f) = 0;
  VALUE_BASE(f) = (L)val; ARRAY_SIZE(f) = 3;
  MOVE(o_1,f);
  for (k=0; k<3; k++) if ((val[k] < 0.0) | (val[k] > 1.0)) return(RNG_CHK);
  color.red = val[0] * 65535.0;
  color.green = val[1] * 65535.0;
  color.blue = val[2] * 65535.0;
  if ((k = XAllocColor(dvtdisplay,XDefaultColormapOfScreen(dvtscreen),
		       &color)) == 0) return(RNG_CHK);
  TAG(o_1) = (NUM | LONGTYPE); ATTR(o_1) = 0;
  LONG_VAL(o_1) = (L)color.pixel;
  return(OK);
#endif
}


/*------------------------------------------------- fillrectangle
   window# xy color_index | --

   xy      defines a rectangle (left, top, width, height)
   color_index  as obtained from 'mapcolor'

   - does nothing but consume the operands if window# does not exist 
*/

L op_fillrectangle()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *xyf, *freevm; L retc, k;

  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  freevm = FREEvm;
  if ((retc = coloropd()) != OK) return(retc);
  if ((retc = xy(&xyf,&freevm)) != OK) return(retc);
  if (ARRAY_SIZE(xyf) != 4) return(RNG_CHK);
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&wid)) return(UNDF_VAL);
  FREEopds = o_1;
  k = 0;
  while (1) { if (k >= ndvtwindows) return(OK);
              if (dvtwindows[k] == wid) break; k++;
            }
  XFillRectangles(dvtdisplay,wid,dvtgc,(XRectangle *)VALUE_BASE(xyf),1);
  return(OK);
#endif
}

/*------------------------------------------------- drawline
   window# xy color_index | --

   xy      defines two or more coordinate pairs
   color   array or list of 3 elements: red green blue (range: 0-255)

   - does nothing but consume the operands if window# does not exist
   - the line is solid and covers previously drawn items 
*/

L op_drawline()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *xyf, *freevm; L retc, k;
  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_1 < FLOORopds) return(OPDS_UNF);
  freevm = FREEvm;
  if ((retc = coloropd()) != OK) return(retc);
  if ((retc = xy(&xyf,&freevm)) != OK) return(retc);
  if (ARRAY_SIZE(xyf) < 4) return(RNG_CHK);
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&wid)) return(UNDF_VAL);
  FREEopds = o_1;
  k = 0;
  while (1) { if (k >= ndvtwindows) return(OK);
              if (dvtwindows[k] == wid) break; k++;
            }
  XDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)VALUE_BASE(xyf),
	     ARRAY_SIZE(xyf)>>1, CoordModeOrigin);
  return(OK);
#endif
}

/*------------------------------------------------- drawsymbols
   window# xy color_index symbol# symbolsize | --

   xy      defines two or more coordinate pairs
   color   array or list of 3 elements: red green blue (range: 0-255)
   symbol# 0 - dot
           1 - stroked diamond
           2 - filed square
           3 - stroked square
           4 - stroked square with horizontal bar
           5 - cross
           6 - x
           7 - filled circle
           8 - stroked circle
           9 - stroked circle with horizontal bar
          10 - asterisk
          11 - filled up triangle
          12 - filled down triangle
          13 - filled right triangle
          14 - filled left triangle
          15 - vertical bar, centered
          16 - verical bar, bottom adjusted
          17 - vertical bar, top adjusted
          18 - horizontal bar, centered
          19 - horizontal bar, left adjusted
          20 - horizontal bar, right adjusted
   symbolsize in pixels

   - does nothing but consume the operands if window# does not exist
   - the symbol covers previously drawn items such as a line within its
     entire perimeter even if it is stroked
*/

#if ! X_DISPLAY_MISSING
static void DOTsymbol()
{
  XDrawPoint(dvtdisplay,wid,dvtgc,(L)x,(L)y);
}

static void DIAMONDsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y,
            x,   y-d,
            x+d, y,
            x,   y+d,
            x-d, y };
 XDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,5,CoordModeOrigin);   
}

static void fSQUAREsymbol()
{
  W d = s>>1;
  XFillRectangle(dvtdisplay,wid,dvtgc,(L)(x-d),(L)(y-d),(UL)s,(UL)(s));
}

static void sSQUAREsymbol()
{
  W d = s>>1;
  XDrawRectangle(dvtdisplay,wid,dvtgc,(L)(x-d),(L)(y-d),(UL)s,(UL)(s));
}

static void hsSQUAREsymbol()
{
  W d = s>>1;
  XDrawRectangle(dvtdisplay,wid,dvtgc,(L)(x-d),(L)(y-d),(UL)s,(UL)(s));
  XDrawLine(dvtdisplay,wid,dvtgc,(L)(x-d),(L)y,(L)(x+d),(L)y);
}

static void PLUSsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y, x+d, y, x, y-d, x, y+d };
  XDrawSegments(dvtdisplay,wid,dvtgc,(XSegment *)p,2);
}

static void Xsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y-d, x+d, y+d, x-d, y+d, x+d, y-d };
  XDrawSegments(dvtdisplay,wid,dvtgc,(XSegment *)p,2);
}

static void fCIRCLEsymbol()
{
  W d = s>>1;
  XFillArc(dvtdisplay,wid,dvtgc,(L)(x-d),(L)(y-d),(UL)s,(UL)s,0L,360L<<6);
}

static void sCIRCLEsymbol()
{
  W d = s>>1;
  XDrawArc(dvtdisplay,wid,dvtgc,(L)(x-d),(L)(y-d),(UL)s,(UL)s,0L,360L<<6);
}

static void hsCIRCLEsymbol()
{
  W d = s>>1;
  XDrawArc(dvtdisplay,wid,dvtgc,(L)(x-d),(L)(y-d),(UL)s,(UL)s,0L,360L<<6);
  XDrawLine(dvtdisplay,wid,dvtgc,(L)(x-d),(L)y,(L)(x+d),(L)y);
}

static void ASTERsymbol()
{
  PLUSsymbol(); Xsymbol();
}

static void usTRIsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y+d,
            x,   y-d,
            x+d, y+d,
            x-d, y+d };
 XDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void dsTRIsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y-d,
            x+d, y-d,
            x,   y+d,
            x-d, y-d };
 XDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void rsTRIsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y-d,
            x+d, y,
            x-d, y+d,
            x-d, y-d };
 XDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void lsTRIsymbol()
{
  W d = s>>1;
  W p[] = { x-d, y,
            x+d, y-d,
            x+d, y+d,
            x-d, y };
 XDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void vcBARsymbol()
{
  W d = s>>1;
  XDrawLine(dvtdisplay,wid,dvtgc,(L)x,(L)(y-d),(L)x,(L)(y+d));
}

static void vdBARsymbol()
{
  XDrawLine(dvtdisplay,wid,dvtgc,(L)x,(L)y,(L)x,(L)(y+s));
}

static void vuBARsymbol()
{
  XDrawLine(dvtdisplay,wid,dvtgc,(L)x,(L)y,(L)x,(L)(y-s));
}

static void hcBARsymbol()
{
  W d = s>>1;
  XDrawLine(dvtdisplay,wid,dvtgc,(L)(x-d),(L)y,(L)x+d,(L)y);
}

static void hlBARsymbol()
{
  XDrawLine(dvtdisplay,wid,dvtgc,(L)x,(L)y,(L)(x+s),(L)y);
}

static void hrBARsymbol()
{
  XDrawLine(dvtdisplay,wid,dvtgc,(L)x,(L)y,(L)(x-s),(L)y);
}

typedef void (*SYMBfunction)();

static SYMBfunction SYMBlist[] = {
  DOTsymbol, DIAMONDsymbol, fSQUAREsymbol, sSQUAREsymbol, hsSQUAREsymbol,
  PLUSsymbol, Xsymbol, fCIRCLEsymbol, sCIRCLEsymbol, hsCIRCLEsymbol,
  ASTERsymbol, usTRIsymbol, dsTRIsymbol, rsTRIsymbol, lsTRIsymbol,
  vcBARsymbol, vuBARsymbol, vdBARsymbol, hcBARsymbol, hrBARsymbol,
  hlBARsymbol };
#endif //! X_DISPLAY_MISSING

L op_drawsymbols()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *xyf, *freevm; L retc, k, symbol; SYMBfunction symbfct; W *p;

  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_5 < FLOORopds) return(OPDS_UNF);
  if (CLASS(o_2) != NUM) return(OPD_CLA);
  if (!VALUE(o_2,&symbol)) return(UNDF_VAL);
  if ((symbol < 0) || (symbol > 20)) return(RNG_CHK);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&k)) return(UNDF_VAL); s = k;
  if ((s < 1) || (s >20)) return(RNG_CHK);
  FREEopds = o_2;
  freevm = FREEvm;
  if ((retc = coloropd()) != OK) return(retc);
  if ((retc = xy(&xyf,&freevm)) != OK) return(retc);
  if (ARRAY_SIZE(xyf) < 2) return(RNG_CHK);
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&wid)) return(UNDF_VAL);
  FREEopds = o_1;
  k = 0;
  while (1) { if (k >= ndvtwindows) return(OK);
              if (dvtwindows[k] == wid) break; k++;
            }
  symbfct = SYMBlist[symbol];
  p = (W *)VALUE_BASE(xyf);
  for (k=0; k<ARRAY_SIZE(xyf); k+=2)
    {
      x = p[k]; y = p[k+1]; (*symbfct)();
    }
  return(OK);
#endif
}

/*-------------------------------------- drawtext
   window# x y (text) [ (font_spec) col_idx h_align v_align ] | window# x y 

  - x, y are the coordinates of the alignment point
  - the window# and updated coordinates are returned to easy continuation
  - text: to be rendered (printable characters only)
  - fontspec: a string that specifies the font in the Xwindows
    convention; used fonts are cached (up to 10), so switching of fonts
    does not cause delays due to loading from a font file
  - col_idx is a color index obtained via 'mapcolor'
  - h_align: <0 (left), 0 (center), >0 (right) alignment
  - v_align  <0 bottom), 0 (center), >0 (top) alignment
  - the returned y is unchanged; the returned x always is adjusted
    to the position following the last written character
*/

L op_drawtext()
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *fl, fontspec[100]; L haln, valn, k, nname, dx; XFontStruct *font;
  BOOLEAN fontcached;

  if (dvtdisplay == NULL) return(NO_XWINDOWS);
  if (o_5 < FLOORopds) return(OPDS_UNF);
  if (CLASS(o_1) != LIST) return(OPD_CLA);
  if (((LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES) != 4)
    return(RNG_CHK);
  fl = (B *)VALUE_BASE(o_1);
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if ((CLASS(o_3) != NUM) || (CLASS(o_4) != NUM) || (CLASS(o_5) != NUM))
     return(OPD_CLA);
  if (!VALUE(o_5, &wid)) return(UNDF_VAL);
  if (!VALUE(o_4, &x)) return(UNDF_VAL);
  if (!VALUE(o_3, &y)) return(UNDF_VAL);
  if (TAG(fl) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if ((nname = ARRAY_SIZE(fl)) > 99) return(RNG_CHK);
  if (!VALUE(fl + FRAMEBYTES, &colidx)) return(UNDF_VAL);
  if (!VALUE(fl + 2 * FRAMEBYTES, &haln)) return(UNDF_VAL);
  if (!VALUE(fl + 3 * FRAMEBYTES, &valn)) return(UNDF_VAL);
  FREEopds = o_2;

  k = 0;
  while (1) { if (k >= ndvtwindows) return(OK);
              if (dvtwindows[k] == wid) break; k++;
            }
  moveB((B *)VALUE_BASE(fl), fontspec, nname);
  fontspec[nname] = '\000';
  k = 0; fontcached = FALSE;
  while(1) { if (k >= ncachedfonts) break;
        if (strcasecmp((cachedfonts[k]).fontname, fontspec) == 0)
           { fontcached = TRUE; break; }
        k++;
      }
  if (!fontcached)
    { if ((ncachedfonts + 1) > MAXCACHEDFONTS)
      {
	XFreeFont(dvtdisplay,(cachedfonts[0]).fontstruct);
        for (k=1; k<ncachedfonts; k++)
	  cachedfonts[k-1] = cachedfonts[k];
	k = MAXCACHEDFONTS - 1; ncachedfonts--;
      }
      else 
      {
	k = ncachedfonts;
      }
      if (( font = XLoadQueryFont(dvtdisplay, fontspec)) == NULL)
      	return(X_BADFONT);
      (cachedfonts[k]).fontstruct = font;
      moveB(fontspec, (cachedfonts[k]).fontname, nname+1);
      ncachedfonts++;
    }
  font = (cachedfonts[k]).fontstruct;
  XSetFont(dvtdisplay,dvtgc,font->fid);
  XSetForeground(dvtdisplay,dvtgc,colidx);

  dx = XTextWidth(font,(B *)VALUE_BASE(o1),ARRAY_SIZE(o1));
  if (haln == 0) x -= dx / 2; else if (haln > 0) x += dx;
  if (valn == 0) y += font->max_bounds.ascent / 2;
    else if (valn > 0) y += font->max_bounds.ascent;
  TAG(o_2) = (NUM | LONGTYPE); ATTR(o_2) = 0;
  LONG_VAL(o_2) = x + dx;
  
  XDrawString(dvtdisplay,wid,dvtgc,x,y,(B *)VALUE_BASE(o1),ARRAY_SIZE(o1));
  return(OK);
#endif
}

//----------------------------------------------- Xdisplayname
// bytearray | bytearray
// stuffs the current displayname in the bytearray, and shrinks
//   the array as necessary
// returns a RNG_CHK if the bytearray isn't large enough,
// and a 0 length string if no dvt display

L op_Xdisplayname(void)
{
    L len;
    if (o_1 < FLOORopds) return OPDS_UNF;
    if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

#if ! X_DISPLAY_MISSING
		if (dvtdisplay == NULL) {
#endif
			ARRAY_SIZE(o_1) = 0;
			return OK;
#if ! X_DISPLAY_MISSING
		}

    len = strlen(displayname);
    if (ARRAY_SIZE(o_1) < len) return RNG_CHK;
    moveB(displayname, (B*) VALUE_BASE(o_1), len);
    ARRAY_SIZE(o_1) = len;

    return OK;
#endif
}
