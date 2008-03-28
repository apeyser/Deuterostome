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
#include <time.h>
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
#include "xhack.h"

int xsocket = -1;

#if ! X_DISPLAY_MISSING
Display *dvtdisplay;
B displayname[80] = "";
Screen *dvtscreen;
Window dvtrootwindow;
XWindowAttributes rootwindowattr;
GC dvtgc;
P ndvtwindows;
P dvtwindows[MAXDVTWINDOWS];
P ncachedfonts;
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

static P wid;
static P colidx;
static P x,y,s;

/*-------------------------- support routines -------------------------------*/

/*------ copy_array
  - copies a source array of any type into temporary long array
*/

static P copy_array(B *sf, B **df, B **freevm)
{
  B *f;

  f = (B *)*freevm;
  if ((*freevm += FRAMEBYTES + ARRAY_SIZE(sf)*VALUEBYTES(WORDTYPE))
      > CEILvm) 
    return VM_OVF;
  TAG(f) = ARRAY | WORDTYPE; 
  ATTR(f) = 0;
  VALUE_BASE(f) = (P)f + FRAMEBYTES; 
  ARRAY_SIZE(f) = ARRAY_SIZE(sf);
  MOVE(sf,f); 
  *df = f;
  return OK;
}

/*------ copy_list
  - copies a source list of numerals into temporary long array
*/

static P copy_list(B *sf, B **df, B **freevm)
{
  B *f, *cf; 
  P k, n;

  f = (B *)*freevm;
  n = (LIST_CEIL(sf) - VALUE_BASE(sf)) / FRAMEBYTES;
  if ((*freevm += FRAMEBYTES + n * VALUEBYTES(WORDTYPE)) > CEILvm)
    return VM_OVF;
  TAG(f) = ARRAY | WORDTYPE; 
  ATTR(f) = 0;
  VALUE_BASE(f) = (P)f + FRAMEBYTES; 
  ARRAY_SIZE(f) = 1;
  cf = (B *)VALUE_BASE(sf); 
  k = n;
  while (k) { 
    if (CLASS(cf) != NUM) return OPD_CLA;
    MOVE(cf,f); 
    VALUE_BASE(f) += VALUEBYTES(WORDTYPE);
    cf += FRAMEBYTES; k--;
  }
  VALUE_BASE(f) = (P)f + FRAMEBYTES; 
  ARRAY_SIZE(f) = n;
  *df = f;
  return OK;
}

/*------ merge
  - combines separate x and y long arrays into one xy long array
*/

static P merge(B *xf, B *yf, B **xyf, B **freevm)
{
  B *f; 
  W *xp, *yp, *dp; 
  P n;

  f = (B *)*freevm;
  if ((*freevm += FRAMEBYTES + 2 * ARRAY_SIZE(xf)*VALUEBYTES(WORDTYPE))
      > CEILvm) 
    return VM_OVF;
  TAG(f) = ARRAY | WORDTYPE; 
  ATTR(f) = 0;
  VALUE_BASE(f) = (P)f + FRAMEBYTES; 
  ARRAY_SIZE(f) = n = 2 * ARRAY_SIZE(xf);
  xp = (W *)VALUE_BASE(xf); 
  yp = (W *)VALUE_BASE(yf);
  dp = (W *)VALUE_BASE(f);
  while (n) { 
    *(dp++) = *(xp++); 
    *(dp++) = *(yp++); 
    n--; 
  }
  *xyf = f;
  return OK;
}

/*------ xy
  - copies the source array(s) while converting into long type
  - merges separate x,y arrays into one
  - new arrays are stored in temporary VM space
*/

static P xy(B **xyf, B **freevm)
{
  P retc; 
  B *xf, *yf;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) == ARRAY) { 
    if (o_2 < FLOORopds) goto do_one_array;
    if (CLASS(o_2) == ARRAY) goto do_array_array;
    if (CLASS(o_2) == LIST) goto do_array_list;
    goto do_one_array;
  }
  else if (CLASS(o_1) == LIST) { 
    if (o_2 < FLOORopds) goto do_one_list;
    if (CLASS(o_2) == ARRAY) goto do_list_array;
    if (CLASS(o_2) == LIST) goto do_list_list;
    goto do_one_list;
  }
  else return OPD_CLA;

 do_one_array:
  if (ARRAY_SIZE(o_1) & 1) return RNG_CHK;
  if ((retc = copy_array(o_1,xyf,freevm)) != OK) return retc;
  FREEopds = o_1; 
  return OK;

 do_array_array:
  if ((retc = copy_array(o_2,&xf,freevm)) != OK) return retc;
  if ((retc = copy_array(o_1,&yf,freevm)) != OK) return retc;
  if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return RNG_CHK;
  if ((retc = merge(xf,yf,xyf,freevm)) != OK) return retc;
  FREEopds = o_2; 
  return OK;

 do_array_list:
  if ((retc = copy_list(o_2,&xf,freevm)) != OK) return retc;
  if ((retc = copy_array(o_1,&yf,freevm)) != OK) return retc;
  if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return RNG_CHK;
  if ((retc = merge(xf,yf,xyf,freevm)) != OK) return retc;
  FREEopds = o_2; 
  return OK;

 do_one_list:
  if (((LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES) & 1) return RNG_CHK;
  if ((retc = copy_list(o_1,xyf,freevm)) != OK) return retc;
  FREEopds = o_1; 
  return OK;

 do_list_array:
  if ((retc = copy_array(o_2,&xf,freevm)) != OK) return retc;
  if ((retc = copy_list(o_1,&yf,freevm)) != OK) return retc;
  if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return RNG_CHK;
  if ((retc = merge(xf,yf,xyf,freevm)) != OK) return retc;
  FREEopds = o_2; 
  return OK;

 do_list_list:
  if ((retc = copy_list(o_2,&xf,freevm)) != OK) return retc;
  if ((retc = copy_list(o_1,&yf,freevm)) != OK) return retc;
  if (ARRAY_SIZE(xf) != ARRAY_SIZE(yf)) return RNG_CHK;
  if ((retc = merge(xf,yf,xyf,freevm)) != OK) return retc;
  FREEopds = o_2; 
  return OK;
}

/*------- evaluate color operand */

P coloropd(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&colidx)) return UNDF_VAL;
  HXSetForeground(dvtdisplay,dvtgc,colidx);
  FREEopds = o_1;
  return OK;
#endif
}

/*----------------------------------------------- Xwindows

   -- | bool

  - reports whether Xwindows is available (if not, X windows operators
    will return the error NO_XWINDOWS)
*/

P op_Xwindows(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  TAG(o1) = BOOL; ATTR(o1) = 0;
#if X_DISPLAY_MISSING
  BOOL_VAL(o1) = FALSE;
#else
  BOOL_VAL(o1) = (dvtdisplay != NULL);
#endif
  FREEopds = o2;
  return OK;
}

/*------------------------------------------------ screensize

   -- | width height

   - reports the screen dimensions
*/

P op_screensize(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o2 > CEILopds) return OPDS_OVF;
  TAG(o1) = TAG(o2) = NUM | LONGBIGTYPE; 
  ATTR(o1) = ATTR(o2) = 0;
  LONGBIG_VAL(o1) = rootwindowattr.width;
  LONGBIG_VAL(o2) = rootwindowattr.height;
  FREEopds = o3;
  return OK;
#endif
}

/*------------------------------------------------ makewindow

    xy (name) (iconname) | window# 

   - creates a new window in rectangle (xy: x, y, width, height)
   - the window manager will dress the window using name
     (no more than 30 characters) and iconname ( <=12 characters)
   - 'nextevent' will identify window by the window#
*/

P op_makewindow(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  static XClassHint classhint = {"d_machine", "d_machine"};
  static XWMHints xwmhints = {InputHint, False};
  static Atom atom[2];
  P retc; 
  W *pxy;
  B *xyf, *freevm, nstr[31], icstr[13],
    *pn[1] = { nstr }, *pic[1] = { icstr };
  XSetWindowAttributes attr;
  XTextProperty wname, icname;

  if (dvtdisplay == NULL) return NO_XWINDOWS;
  attr.event_mask = ButtonPressMask | ExposureMask | StructureNotifyMask;
  attr.override_redirect = True;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_1) > 12) return RNG_CHK;
  moveB((B *)VALUE_BASE(o_1),icstr,ARRAY_SIZE(o_1));
  icstr[ARRAY_SIZE(o_1)] = '\000';
  if (XStringListToTextProperty((char**) pic,1,&icname) == 0)
    return X_ERR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (ARRAY_SIZE(o_2) > 30) return RNG_CHK;
  moveB((B *)VALUE_BASE(o_2),nstr,ARRAY_SIZE(o_2));
  nstr[ARRAY_SIZE(o_2)] = '\000';
  if (XStringListToTextProperty((char**)pn,1,&wname) == 0)
    return X_ERR;
  FREEopds = o_2;
  freevm = FREEvm;
  if ((retc = xy(&xyf,&freevm)) != OK) return retc;
  pxy = (W *)VALUE_BASE(xyf);
  if (ARRAY_SIZE(xyf) != 4) return RNG_CHK;
  if (ndvtwindows >= MAXDVTWINDOWS) return RNG_CHK;

  wid = HXCreateWindow(dvtdisplay, dvtrootwindow, pxy[0], pxy[1],
                      pxy[2], pxy[3], 0, CopyFromParent,
                      InputOutput, CopyFromParent,
                      CWEventMask, &attr);
  HXSetWMName(dvtdisplay,wid,&wname);
  HXSetWMIconName(dvtdisplay,wid,&icname);
  HXSetClassHint(dvtdisplay, wid,&classhint);
  atom[0] = HXInternAtom(dvtdisplay, "WM_DELETE_WINDOW", False);
  atom[1] = HXInternAtom(dvtdisplay, "WM_TAKE_FOCUS", False);
  HXSetWMProtocols(dvtdisplay, wid, atom, 2);
  HXSetWMHints(dvtdisplay, wid, &xwmhints);
  dvtwindows[ndvtwindows++] = wid;

  TAG(o1) = NUM | LONGBIGTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = wid;
  FREEopds = o2;
  return OK;    
#endif
}

/*------------------------------------------------ topwindow
 * 
 * window# true | --
 *
 * make window float above other iff true
 */
P op_makewindowtop(void) 
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
	P k;
	XEvent event;

	if (dvtdisplay == NULL) return NO_XWINDOWS;
	if (o_2 < FLOORopds) return OPDS_UNF;
	if (CLASS(o_1) != BOOL || CLASS(o_2) != NUM) return OPD_CLA;
	if (!PVALUE(o_2, &wid)) return UNDF_VAL;

	FREEopds = o_2;
	for (k = 0; k < ndvtwindows && dvtwindows[k] != wid; k++);
	if (k == ndvtwindows) return OK;

	event.xclient.type = ClientMessage;
	event.xclient.display = dvtdisplay;
	event.xclient.window = wid;
	event.xclient.message_type 
		= HXInternAtom(dvtdisplay, "_NET_WM_STATE", False);
	event.xclient.format = 32;
	event.xclient.data.l[0] = (BOOL_VAL(o2) ? 1 : 0);
	event.xclient.data.l[1] 
		= HXInternAtom(dvtdisplay, "_NET_WM_STATE_ABOVE", False);
	event.xclient.data.l[2] = 0;
	event.xclient.data.l[3] = 2;
	event.xclient.data.l[4] = 0;
	HXSendEvent(dvtdisplay, XRootWindowOfScreen(dvtscreen),
		    False, (SubstructureNotifyMask|SubstructureRedirectMask),
		    &event);
	return OK;
#endif
}
	

/*------------------------------------------------ deletewindow

   window# | --

   - deletes an existing window
   - does nothing if the window does not exist
*/

P op_deletewindow(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  P k;
  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_1 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&wid)) return UNDF_VAL;
  k = 0;
  while (1) { if (k >= ndvtwindows) { FREEopds = o_1; return OK; }
              if (dvtwindows[k] == wid) break; k++;
            }
  --ndvtwindows;
  for ( ; k<ndvtwindows; k++) dvtwindows[k] = dvtwindows[k+1];
  HXDestroyWindow(dvtdisplay,wid);
  FREEopds = o_1;
  return OK;
#endif
}

/*------------------------------------------------ mapwindow

   <window#|null> bool | --

   - true: shows the window as the top window on the screen 
   - false: hides the window
   - does nothing if window does not exist 
	 - if window# == null then map and raise or unmap all windows.
*/
#if ! X_DISPLAY_MISSING
void mapraisewindow(P win) {
	XEvent event;
	HXMapRaised(dvtdisplay, win);
	event.xclient.type = ClientMessage;
	event.xclient.display = dvtdisplay;
	event.xclient.window = win;
	event.xclient.message_type 
	  = HXInternAtom(dvtdisplay, "_NET_ACTIVE_WINDOW", False);
	event.xclient.format = 32;
	event.xclient.data.l[0] = 2;
	event.xclient.data.l[1] = time(NULL);
	event.xclient.data.l[2] = 0;
	event.xclient.data.l[3] = 0;
	event.xclient.data.l[4] = 0;
	HXSendEvent(dvtdisplay, XRootWindowOfScreen(dvtscreen), 
		    False, (SubstructureNotifyMask|SubstructureRedirectMask), 
		    &event);
}
#endif //! X_DISPLAY_MISSING

P op_mapwindow(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  P k;
  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != BOOL) return OPD_CLA;

	switch (CLASS(o_2)) {
		case NULLOBJ:
			for (k = 0; k < ndvtwindows; k++)
				if (BOOL_VAL(o_1)) mapraisewindow(dvtwindows[k]);
				else HXUnmapWindow(dvtdisplay, dvtwindows[k]);
			break;

		case NUM:
			if (!PVALUE(o_2,&wid)) return UNDF_VAL;
			for (k = 0; k < ndvtwindows && dvtwindows[k] != wid; k++);
			if (k == ndvtwindows) break;

			if (BOOL_VAL(o_1)) mapraisewindow(wid);
			else HXUnmapWindow(dvtdisplay,wid);
			break;

		default:
			return OPD_CLA;
	};

	FREEopds = o_2;
	return OK;
#endif
}

/*------------------------------------------------- resizewindow
   window# width height | --
*/

P op_resizewindow(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  P width, height;
  P k;

  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_3 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (!PVALUE(o_3,&wid)) return UNDF_VAL;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (!PVALUE(o_2,&width)) return UNDF_VAL;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&height)) return UNDF_VAL;
  k = 0;
  for (k = 0; k < ndvtwindows; k++)
    if (dvtwindows[k] == wid) break;
  if (k >= ndvtwindows) {FREEopds = o_3; return OK;}

  HXResizeWindow(dvtdisplay,wid, width, height);
  FREEopds = o_3;
  return OK;
#endif
}

/*------------------------------------------------- Xsync
   -- | --

   Flushes buffered graphics instruction, thus rendering their effects.
*/

P op_Xsync(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  if (dvtdisplay == NULL) return NO_XWINDOWS;
  HXFlush(dvtdisplay);
  return OK;
#endif
}

/*------------------------------------------------- mapcolor
   < red green blue > | color_index

   Color contributions are expressed as fractions between 0.0 and 1.0.
   Translates an RGB color into a color map index.
*/

P op_mapcolor(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  XColor color; 
  B f[FRAMEBYTES]; 
  D val[3]; 
  P k;
 
  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_1< FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != ARRAY) return OPD_CLA;
  if (ARRAY_SIZE(o_1) != 3) return RNG_CHK;
  TAG(f) = (ARRAY | DOUBLETYPE); 
  ATTR(f) = 0;
  VALUE_BASE(f) = (P)val; 
  ARRAY_SIZE(f) = 3;
  MOVE(o_1,f);

  for (k=0; k<3; k++) if ((val[k] < 0.0) | (val[k] > 1.0)) return RNG_CHK;
  color.red = val[0] * 65535.0;
  color.green = val[1] * 65535.0;
  color.blue = val[2] * 65535.0;
  if ((k = HXAllocColor(dvtdisplay,
			XDefaultColormapOfScreen(dvtscreen),
			&color)) 
      == 0)
    return RNG_CHK;

  TAG(o_1) = (NUM | LONGBIGTYPE); 
  ATTR(o_1) = 0;
  LONGBIG_VAL(o_1) = color.pixel;
  return OK;
#endif
}


/*------------------------------------------------- fillrectangle
   window# xy color_index | --

   xy      defines a rectangle (left, top, width, height)
   color_index  as obtained from 'mapcolor'

   - does nothing but consume the operands if window# does not exist 
*/

P op_fillrectangle(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *xyf, *freevm; 
  P retc, k;

  if (dvtdisplay == NULL) return NO_XWINDOWS;
  freevm = FREEvm;
  if ((retc = coloropd()) != OK) return retc;
  if ((retc = xy(&xyf,&freevm)) != OK) return retc;
  if (ARRAY_SIZE(xyf) != 4) return RNG_CHK;
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&wid)) return UNDF_VAL;
  FREEopds = o_1;

  for (k = 0; k < ndvtwindows; k++) 
    if (dvtwindows[k] == wid) break;
  if (k >= ndvtwindows) return OK;

  HXFillRectangles(dvtdisplay,wid,dvtgc,(XRectangle *)VALUE_BASE(xyf),1);
  return OK;
#endif
}

/*------------------------------------------------- drawline
   window# xy color_index | --

   xy      defines two or more coordinate pairs
   color   array or list of 3 elements: red green blue (range: 0-255)

   - does nothing but consume the operands if window# does not exist
   - the line is solid and covers previously drawn items 
*/

P op_drawline(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *xyf, *freevm; 
  P retc, k;

  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_1 < FLOORopds) return OPDS_UNF;
  freevm = FREEvm;
  if ((retc = coloropd()) != OK) return retc;
  if ((retc = xy(&xyf,&freevm)) != OK) return retc;
  if (ARRAY_SIZE(xyf) < 4) return RNG_CHK;
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&wid)) return UNDF_VAL;
  FREEopds = o_1;
  for (k = 0; k < ndvtwindows; k++)
    if (dvtwindows[k] == wid) break;
  if (k >= ndvtwindows) return OK;

  HXDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)VALUE_BASE(xyf),
             ARRAY_SIZE(xyf)>>1, CoordModeOrigin);
  return OK;
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
static void DOTsymbol(void)
{
  HXDrawPoint(dvtdisplay,wid,dvtgc,x,y);
}

static void DIAMONDsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y,
            x,   y-d,
            x+d, y,
            x,   y+d,
            x-d, y };
  HXDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,5,CoordModeOrigin);   
}

static void fSQUAREsymbol(void)
{
  W d = s>>1;
  HXFillRectangle(dvtdisplay,wid,dvtgc,(x-d),(y-d),(UP)s,(UP)(s));
}

static void sSQUAREsymbol(void)
{
  W d = s>>1;
  HXDrawRectangle(dvtdisplay,wid,dvtgc,(x-d),(y-d),(UP)s,(UP)(s));
}

static void hsSQUAREsymbol(void)
{
  W d = s>>1;
  HXDrawRectangle(dvtdisplay,wid,dvtgc,(x-d),(y-d),(UP)s,(UP)(s));
  HXDrawLine(dvtdisplay,wid,dvtgc,(x-d),y,(x+d),y);
}

static void PLUSsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y, x+d, y, x, y-d, x, y+d };
  HXDrawSegments(dvtdisplay,wid,dvtgc,(XSegment *)p,2);
}

static void Xsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y-d, x+d, y+d, x-d, y+d, x+d, y-d };
  HXDrawSegments(dvtdisplay,wid,dvtgc,(XSegment *)p,2);
}

static void fCIRCLEsymbol(void)
{
  W d = s>>1;
  HXFillArc(dvtdisplay,wid,dvtgc,(x-d),(y-d),(UP)s,(UP)s,0L,360L<<6);
}

static void sCIRCLEsymbol(void)
{
  W d = s>>1;
  HXDrawArc(dvtdisplay,wid,dvtgc,(x-d),(y-d),(UP)s,(UP)s,0L,360L<<6);
}

static void hsCIRCLEsymbol(void)
{
  W d = s>>1;
  HXDrawArc(dvtdisplay,wid,dvtgc,(x-d),(y-d),(UP)s,(UP)s,0L,360L<<6);
  HXDrawLine(dvtdisplay,wid,dvtgc,(x-d),y,(x+d),y);
}

static void ASTERsymbol(void)
{
  PLUSsymbol(); Xsymbol();
}

static void usTRIsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y+d,
            x,   y-d,
            x+d, y+d,
            x-d, y+d };
  HXDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void dsTRIsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y-d,
            x+d, y-d,
            x,   y+d,
            x-d, y-d };
  HXDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void rsTRIsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y-d,
            x+d, y,
            x-d, y+d,
            x-d, y-d };
  HXDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void lsTRIsymbol(void)
{
  W d = s>>1;
  W p[] = { x-d, y,
            x+d, y-d,
            x+d, y+d,
            x-d, y };
  HXDrawLines(dvtdisplay,wid,dvtgc,(XPoint *)p,4,CoordModeOrigin);
}

static void vcBARsymbol(void)
{
  W d = s>>1;
  HXDrawLine(dvtdisplay,wid,dvtgc,x,(y-d),x,(y+d));
}

static void vdBARsymbol(void)
{
  HXDrawLine(dvtdisplay,wid,dvtgc,x,y,x,(y+s));
}

static void vuBARsymbol(void)
{
  HXDrawLine(dvtdisplay,wid,dvtgc,x,y,x,(y-s));
}

static void hcBARsymbol(void)
{
  W d = s>>1;
  HXDrawLine(dvtdisplay,wid,dvtgc,(x-d),y,x+d,y);
}

static void hlBARsymbol(void)
{
  HXDrawLine(dvtdisplay,wid,dvtgc,x,y,(x+s),y);
}

static void hrBARsymbol(void)
{
  HXDrawLine(dvtdisplay,wid,dvtgc,x,y,(x-s),y);
}

typedef void (*SYMBfunction)(void);

static SYMBfunction SYMBlist[] = {
  DOTsymbol, DIAMONDsymbol, fSQUAREsymbol, sSQUAREsymbol, hsSQUAREsymbol,
  PLUSsymbol, Xsymbol, fCIRCLEsymbol, sCIRCLEsymbol, hsCIRCLEsymbol,
  ASTERsymbol, usTRIsymbol, dsTRIsymbol, rsTRIsymbol, lsTRIsymbol,
  vcBARsymbol, vuBARsymbol, vdBARsymbol, hcBARsymbol, hrBARsymbol,
  hlBARsymbol };
#endif //! X_DISPLAY_MISSING

P op_drawsymbols(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *xyf, *freevm; 
  P retc, k;
  P symbol; 
  SYMBfunction symbfct; 
  W *p;

  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (!PVALUE(o_2,&symbol)) return UNDF_VAL;
  if ((symbol < 0) || (symbol > 20)) return RNG_CHK;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&k)) return UNDF_VAL; 
  s = k;
  if ((s < 1) || (s > 20)) return RNG_CHK;
  FREEopds = o_2;
  freevm = FREEvm;
  if ((retc = coloropd()) != OK) return retc;
  if ((retc = xy(&xyf,&freevm)) != OK) return retc;
  if (ARRAY_SIZE(xyf) < 2) return RNG_CHK;
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (!PVALUE(o_1,&wid)) return UNDF_VAL;
  FREEopds = o_1;
  
  for (k = 0; k < ndvtwindows; k++)
    if (dvtwindows[k] == wid) break;
  if (k >= ndvtwindows) return OK;

  symbfct = SYMBlist[symbol];
  p = (W *)VALUE_BASE(xyf);
  for (k = 0; k < ARRAY_SIZE(xyf); k+=2) {
    x = p[k]; 
    y = p[k+1]; 
    (*symbfct)();
  }

  return OK;
#endif
}

/*--------------------------------------- setinputfocus
 * window# | --
 * window# gets keyboard focus
 */
P op_setinputfocus(void) {
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
	if (dvtdisplay == NULL) return NO_XWINDOWS;
	if (o_1 < FLOORopds) return OPDS_UNF;
	if (CLASS(o_1) != NUM) return OPD_CLA;
	if (! PVALUE(o_1, &wid)) return UNDF_VAL;
	
	HXSetInputFocus(dvtdisplay, wid, RevertToParent, CurrentTime);
	return OK;
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

P op_drawtext(void)
{
#if X_DISPLAY_MISSING
	return NO_XWINDOWS;
#else
  B *fl, fontspec[100]; 
  P haln, valn;
  P k, nname, dx; 
  XFontStruct *font;
  BOOLEAN fontcached;

  if (dvtdisplay == NULL) return NO_XWINDOWS;
  if (o_5 < FLOORopds) return OPDS_UNF;
  if (CLASS(o_1) != LIST) return OPD_CLA;
  if (((LIST_CEIL(o_1) - VALUE_BASE(o_1)) / FRAMEBYTES) != 4)
    return RNG_CHK;
  fl = (B *)VALUE_BASE(o_1);
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if ((CLASS(o_3) != NUM) || (CLASS(o_4) != NUM) || (CLASS(o_5) != NUM))
     return OPD_CLA;
  if (!PVALUE(o_5, &wid)) return UNDF_VAL;
  if (!PVALUE(o_4, &x)) return UNDF_VAL;
  if (!PVALUE(o_3, &y)) return UNDF_VAL;
  if (TAG(fl) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if ((nname = ARRAY_SIZE(fl)) > 99) return RNG_CHK;
  if (!PVALUE(fl + FRAMEBYTES, &colidx)) return UNDF_VAL;
  if (!PVALUE(fl + 2 * FRAMEBYTES, &haln)) return UNDF_VAL;
  if (!PVALUE(fl + 3 * FRAMEBYTES, &valn)) return UNDF_VAL;
  FREEopds = o_2;

  for (k = 0; k < ndvtwindows; k++)
    if (dvtwindows[k] == wid) break;
  if (k >= ndvtwindows) return OK;

  moveB((B *)VALUE_BASE(fl), fontspec, nname);
  fontspec[nname] = '\000';
  k = 0; fontcached = FALSE;
  for (k = 0; k < ncachedfonts; k++)
    if (strcasecmp((char*)cachedfonts[k].fontname, (char*)fontspec) == 0) { 
      fontcached = TRUE; 
      break; 
    }

  if (!fontcached) { 
    if ((ncachedfonts + 1) > MAXCACHEDFONTS) {
      HXFreeFont(dvtdisplay,(cachedfonts[0]).fontstruct);
      for (k=1; k<ncachedfonts; k++)
        cachedfonts[k-1] = cachedfonts[k];
      k = MAXCACHEDFONTS - 1; 
      ncachedfonts--;
    }
    else k = ncachedfonts;

    if ((font = HXLoadQueryFont(dvtdisplay, (char*)fontspec)) == NULL)
      return X_BADFONT;

    cachedfonts[k].fontstruct = font;
    moveB(fontspec, cachedfonts[k].fontname, nname+1);
    ncachedfonts++;
  }
  font = cachedfonts[k].fontstruct;
  HXSetFont(dvtdisplay,dvtgc,font->fid);
  HXSetForeground(dvtdisplay,dvtgc,colidx);

  dx = XTextWidth(font, (char*)VALUE_BASE(o1),ARRAY_SIZE(o1));
  if (haln == 0) x -= dx / 2; 
  else if (haln > 0) x += dx;
  if (valn == 0) y += font->max_bounds.ascent / 2;
  else if (valn > 0) y += font->max_bounds.ascent;

  TAG(o_2) = (NUM | LONGBIGTYPE); 
  ATTR(o_2) = 0;
  LONGBIG_VAL(o_2) = x + dx;
  
  HXDrawString(dvtdisplay,wid, dvtgc, x,  y,
	       (char*)VALUE_BASE(o1), ARRAY_SIZE(o1));
  return OK;
#endif
}

//----------------------------------------------- Xdisplayname
// bytearray | bytearray
// stuffs the current displayname in the bytearray, and shrinks
//   the array as necessary
// returns a RNG_CHK if the bytearray isn't large enough,
// and a 0 length string if no dvt display

P op_Xdisplayname(void)
{
    P len;
    if (o_1 < FLOORopds) return OPDS_UNF;
    if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;

#if ! X_DISPLAY_MISSING
		if (dvtdisplay == NULL) {
#endif
			ARRAY_SIZE(o_1) = 0;
			return OK;
#if ! X_DISPLAY_MISSING
		}

    len = strlen((char*)displayname);
    if (ARRAY_SIZE(o_1) < len) return RNG_CHK;
    moveB(displayname, (B*) VALUE_BASE(o_1), len);
    ARRAY_SIZE(o_1) = len;

    return OK;
#endif
}
