/*=================== D machine 3.0: header file ========================
   History:

   2.0    - major overhaul of 1.41 version for porting D to the PPC,
                - all code is written in ANSII C
                - modules are clearly separated into two groups,
                  host-independent (DM prefix) or host-specific (dvt
                  or dnode)
                - the virtual math coprocessor is based on IEEE formats
                  and within this constraint host-independent
   2.1    - VM and eVM alignment reworked (quantum is system constant)
   2.2    - added more math operators and boxfile operators

   3.0    - major overhaul to move on to Linux
                - added support for networked clusters of D machines
                - virtual terminal  is farmed out to special D machine
                - external VM's are gone
                - new memory management
                - added support for external operator libraries

*/

/*---------------------------------------------- standard data types */

#ifndef DM_H
#define DM_H

#include "dm-config.h"
#ifdef DM_HAVE_CONFIG_H
#include "config.h"
#endif

#include <features.h>

#include "basic-defs.h"

#if __cplusplus
extern "C" {
#endif		

#if ! defined __GNUC__ && ! defined __attribute__
#define __attribute__(attr)
#endif // ! defined __GNUC__ && ! defined __attribute__

#if DM_DISABLE_REGEX && DM_ENABLE_REGEX
#undef DM_ENABLE_REGEX
#endif

#ifndef _dm_const
#define _dm_const const
#endif //_dm_const

#ifndef _dm_restrict
#define _dm_restrict restrict
#endif //_dm_restrict

#ifndef _dm_inline
#define _dm_inline inline
#endif //_dm_inline

#ifdef __GNUC__
#define DM_GNUC_VERSION (__GNUC__ * 10000 \
                         + __GNUC_MINOR__ * 100 \
                         + __GNUC_PATCHLEVEL__)
#endif

#if DM_DISABLE_XDISPLAY

#ifdef DM_X_DISPLAY_MISSING
#undef DM_X_DISPLAY_MISSING
#endif //def DM_X_DISPLAY_MISSING

#ifdef X_DISPLAY_MISSING
#undef X_DISPLAY_MISSING
#endif //def X_DISPLAY_MISSING

#define DM_X_DISPLAY_MISSING 1
#define X_DISPLAY_MISSING 1

#endif //DM_DISABLE_XDISPLAY

// Need to be here because we define things such as x1
#if ! DM_X_DISPLAY_MISSING
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

#ifdef _WIN32
#ifdef DLL_EXPORT
#define DLL_SCOPE __declspec(dllexport)
#else //! DLL_EXPORT
#ifdef LIBDM_DLL_IMPORT
#define DLL_SCOPE extern __declspec(dllimport)
#endif //LIBDM_DLL_EXPORT
#endif //DLL_EXPORT
#endif //_WIN32

#ifndef DLL_SCOPE
#define DLL_SCOPE extern
#endif

#include <sys/socket.h>
#include <sys/select.h>
#if ! DM_NO_ENDIAN_HDR
#include DM_ENDIAN_HDR
#endif //DM_NO_ENDIAN_HDR

#define ISUNDEF(n) (isinf(n) || isnan(n))

/*-------------------------- network packet size ----------------------*/

#define PACKET_SIZE 8192

/*---------------------------------------------- frame */

/* Classes:              Applicable types:
!
!      null              SOCKETTYPE
!      num               BYTETYPE...DOUBLETYPE
!      op                
!      name              
!      bool             
!      mark              
!      array (string)    BYTETYPE...DOUBLETYPE
!      list  (proc)     
!      dict             
!      box 
|
| Null and Box have opaque values that are not accessible for common
| use. More Types might be defined for these objects in the future.             
*/

#define TAG(frame)                 (*((UB *)(frame)))
#define ATTR(frame)                (*(((UB*)(frame))+1))
#define FORMAT(frame)              (*(((UB *)(frame))+2))

#define CLASS(frame)               ((*((UB *)(frame))) & 0xF0)
#define TYPE(frame)                ((*((UB *)(frame))) & 0x0F)

#define NULLOBJ                    ((UB) 0x00)      /* classes */
#define NUM                        ((UB) 0x10)
#define OP                         ((UB) 0x20)
#define NAME                       ((UB) 0x30)
#define BOOL                       ((UB) 0x40)
#define MARK                       ((UB) 0x50)
#define ARRAY                      ((UB) 0x60)
#define STRING                     ((UB) 0x60)
#define LIST                       ((UB) 0x70)
#define PROC                       ((UB) 0x70)
#define DICT                       ((UB) 0x80)
#define BOX                        ((UB) 0x90)
	
#define COMPOSITE(frame)           ((UB)(CLASS(frame)) > (UB)(MARK))

#define BYTETYPE                   ((UB) 0x00)       /* numeral types */
#define WORDTYPE                   ((UB) 0x01)
#define LONG32TYPE                 ((UB) 0x02)
#define LONG64TYPE                 ((UB) 0x03)
#define LONGBIGTYPE                LONG64TYPE
#define SINGLETYPE                 ((UB) 0x04)
#define DOUBLETYPE                 ((UB) 0x05)

#define SOCKETTYPE                 ((UB) 0x01)      /* null types */
#define OPLIBTYPE                  ((UB) 0x02)      /* operator lib type */
#define OPAQUETYPE                 ((UB) 0x03)      /* opaque dictionary */

/* attributes qualify a frame, not an object value: */

#define ACTIVE                     ((UB)0x01)  
#define READONLY                   ((UB)0x02)
#define PARENT                     ((UB)0x04)
#define TILDE                      ((UB)0x08)

/* Composite endianness */
#define BIGENDIAN                  ((UB) 0x00)
#define LITTLEENDIAN               ((UB) 0x01)
#define ENDIANMASK                 ((UB) 0x01)

/* Composite host bit format */
#define HOSTBITS32                 ((UB) 0x00)
#define HOSTBITS64                 ((UB) 0x02)
#define HOSTBITSMASK               ((UB) 0x02)

/* Format specifier */
#define FORMAT32                   ((UB) 0x10)
#define FORMAT64                   ((UB) 0x20)
#define FORMATMASK                 ((UB) 0xF0)

#define FORMAT_BITS_DEFAULT        FORMAT64
#if DM_HOST_IS_32_BIT
#define HOSTBITS_DEFAULT           HOSTBITS32
#else
#define HOSTBITS_DEFAULT           HOSTBITS64
#endif //DM_HOST_IS_32_BIT

// make BSD and Linux look alike

#if ! NO_ENDIAN_HDR && ! BYTE_ORDER
#define BYTE_ORDER __BYTE_ORDER
#define LITTLE_ENDIAN __LITTLE_ENDIAN
#define BIG_ENDIAN __BIG_ENDIAN
#endif // ! NO_ENDIAN_HDR && ! BYTE_ORDER

#ifndef DM_WORDS_BIGENDIAN
#define ENDIAN_DEFAULT LITTLEENDIAN
#else
#define ENDIAN_DEFAULT BIGENDIAN
#endif //DM_WORDS_BIGENDIAN

#define HOSTLAYOUTMASK            ((B) (HOSTBITSMASK | ENDIANMASK))
#define HOSTLAYOUT_DEFAULT         ((B) (HOSTBITS_DEFAULT | ENDIAN_DEFAULT))

#define GETNATIVE_FORMATSTATE(frame, mask, defaultbits) \
  ((BOOLEAN) ((FORMAT(frame) & (mask)) == (defaultbits)))
#define SETNATIVE_FORMATSTATE(frame, defaultbits) \
  do {FORMAT(frame) |= (defaultbits);} while (0)

#define GETNATIVEENDIAN(frame)                              \
  GETNATIVE_FORMATSTATE(frame, ENDIANMASK, ENDIAN_DEFAULT)
#define SETNATIVEENDIAN(frame)                  \
  SETNATIVE_FORMATSTATE(frame, ENDIAN_DEFAULT)

#define GETNATIVEHOSTBITS(frame)                                \
  GETNATIVE_FORMATSTATE(frame, HOSTBITSMASK, HOSTBITS_DEFAULT)
#define SETNATIVEHOSTBITS(frame) \
  SETNATIVE_FORMATSTATE(frame, HOSTBITS_DEFAULT)

#if ! NO_ENDIAN_HDR && __FLOAT_WORD_ORDER
#if __FLOAT_WORD_ORDER != __BYTE_ORDER
#error "Can't handle float word order __FLOAT_WORD_ORDER"
#endif //__FLOAT_WORD_ORDER
//#else //! NO_ENDIAN_HDR
//#warning "Confirm that float word order = word order"
#endif //! NO_ENDIAN_HDR

#define GETNATIVEFORMAT(frame) \
  GETNATIVE_FORMATSTATE(frame, FORMATMASK, FORMAT_BITS_DEFAULT)
#define SETNATIVEFORMAT(frame) \
  SETNATIVE_FORMATSTATE(frame, FORMAT_BITS_DEFAULT)

#define GETNATIVE(frame)                        \
  (GETNATIVEFORMAT(frame)                       \
   && GETNATIVEENDIAN(frame)                    \
   && GETNATIVEHOSTBITS(frame))

#define SETNATIVE(frame)do {                    \
    FORMAT(frame) = 0;                          \
    SETNATIVEFORMAT(frame);                     \
    SETNATIVEENDIAN(frame);                     \
    SETNATIVEHOSTBITS(frame);                   \
  } while (0)

#define GETNATIVEUNDEF(frame) \
  ((BOOLEAN) ! (FORMAT(frame) & ~(FORMATMASK | HOSTLAYOUTMASK)))

#define GETNONNATIVE(frame) \
  ((HOSTLAYOUTMASK & FORMAT(frame)) ^ HOSTLAYOUT_DEFAULT)
#define HASNATIVEENDIAN(nomatchbits) (! ((nomatchbits) & ENDIANMASK))
#define HASNATIVEBITS(nomatchbits) (! ((nomatchbits) & HOSTBITSMASK))
  

#define EXITMARK                   ((UB)0x10)   /* execstack marks */
#define STOPMARK                   ((UB)0x20)
#define ABORTMARK                  ((UB)0x40)
#define XMARK                      ((UB)0x70)
#define BIND                       ((UB)0x80)   /* box op housekeeping */

#define PF_PTR(frame, offset) (((B*)(frame))+(offset)*PACK_FRAME)

#define BOOL_VAL(frame)      (*((BOOLEAN *)(((B*)(frame))+2)))
#define NAME_KEY(frame)      (*((W *)      (((B*)(frame))+2)))

#define NUM_VAL(frame)       ( ((B *)  PF_PTR(frame,1)))
#define LONGBIG_VAL(frame)   (*((LBIG*)PF_PTR(frame,1)))
#define LONG32_VAL(frame)    (*((L32*) PF_PTR(frame,1)))
#define LONG64_VAL(frame)    (*((L64*) PF_PTR(frame,1)))
#define BYTE_VAL(frame)      (*((B*)   PF_PTR(frame,1)))
#define WORD_VAL(frame)      (*((W*)   PF_PTR(frame,1)))
#define SOCKET_VAL(frame)    (*((P*)   PF_PTR(frame,1)))

#define VALUE_BASE(frame)    (*((P *)  PF_PTR(frame,1)))
#define VALUE_PTR(frame)     (*((B**)  PF_PTR(frame,1)))
#define OP_CODE(frame)       (*((P *)  PF_PTR(frame,1)))

#define OP_NAME(frame)       (*((P *)  PF_PTR(frame,2)))
#define LIST_CEIL(frame)     (*((P *)  PF_PTR(frame,2)))
#define ARRAY_SIZE(frame)    (*((P *)  PF_PTR(frame,2)))
#define DICT_NB(frame)       (*((P *)  PF_PTR(frame,2)))
#define DICT_CURR(frame)     (*((P *)  PF_PTR(frame,2)))
#define BOX_NB(frame)        (*((P *)  PF_PTR(frame,2)))
#define LIST_CEIL_PTR(frame) (*((B**)  PF_PTR(frame,2)))
#define DGRAM_VAL(frame)     (*((P *)  PF_PTR(frame,2)))

/*-------------------------------------------- dictionary */
 
#define ASSOC_NAME(entry)    ( ((B*)(entry)))
#define ASSOC_NEXT(entry)    (*((P*)(((B*)(entry))+FRAMEBYTES)))
#define ASSOC_FRAME(entry)   ( ((B*)(entry))+ENTRYBYTES-FRAMEBYTES)

// keep frame on 64 bit boundaries, so that doubles will be so.
#define ENTRYBYTES  DALIGN(FRAMEBYTES+PACK_FRAME+FRAMEBYTES)

#define DICT_ENTRIES(dict)        (*((P *) (dict)))
#define DICT_FREE(dict)           (*((P *) PF_PTR(dict,1)))
#define DICT_CONHASH(dict)        (*((W *) PF_PTR(dict,3)))
#define DICT_TABHASH(dict)        (*((P *) PF_PTR(dict,2)))
#define DICT_TABHASH_ARR(dict, n) (*(P*) &(((LBIG*) DICT_TABHASH(dict))[n]))

#define DICTBYTES             DALIGN(4*PACK_FRAME)

#define LIB_DATA(frame)       (VALUE_PTR(frame) + DICT_NB(frame))

#define LIB_TYPE(frame)       (*(P*)   (LIB_DATA(frame)))
#define LIB_HANDLE(frame)     (*(P*)   (PF_PTR(LIB_DATA(frame),1)))
#define LIB_ERRC(frame)       (*(P**)  (PF_PTR(LIB_DATA(frame),2)))
#define LIB_ERRM(frame)       (*(B***) (PF_PTR(LIB_DATA(frame),3)))

#define LIBBYTES             DALIGN(4*PACK_FRAME)

/*---------------------------------- C Operator definition */

#define OPDEF_NAME(operator)  (((P*)(operator))[0])
#define OPDEF_CODE(operator)  (((P*)(operator))[1])
#define OPDEFBYTES            (2*sizeof(P))

/*---------------------------------------- save box */
#define SBOXBYTES             DALIGN(1*PACK_FRAME)
#define SBOX_CAP(box)         (*(B **) PF_PTR(box,0))

/*--------------------------------------------- Internal message codes */
#include "dm-errs.h"

/* compare results */

#define LT                         (-1)
#define EQ                         0
#define GT                         1
#define UN                         2

/*----------------------------------------------- globals  for DMACHINE */
DLL_SCOPE B *FLOORopds;
DLL_SCOPE B *FREEopds;
DLL_SCOPE B* CEILopds;
DLL_SCOPE B* FLOORdicts;
DLL_SCOPE B* FREEdicts;
DLL_SCOPE B* CEILdicts;
DLL_SCOPE B* FLOORexecs;
DLL_SCOPE B* FREEexecs;
DLL_SCOPE B* CEILexecs;
DLL_SCOPE B* FLOORvm;
DLL_SCOPE B* FREEvm;
DLL_SCOPE B* CEILvm;
DLL_SCOPE B* TOPvm;

DLL_SCOPE B* errsource;
DLL_SCOPE B errorframe[FRAMEBYTES];
DLL_SCOPE B** syserrm;
DLL_SCOPE P* syserrc;
DLL_SCOPE B** sysop;

DLL_SCOPE B locked;
DLL_SCOPE B serialized;

DLL_SCOPE BOOLEAN halt_flag;          /* execution block due to 'halt'     */
DLL_SCOPE fd_set sock_fds;
DLL_SCOPE BOOLEAN timeout;             /* for I/O operations          */
DLL_SCOPE BOOLEAN abortflag;
DLL_SCOPE BOOLEAN numovf;             /* FPU overflow status            */
DLL_SCOPE BOOLEAN tinymemory;
DLL_SCOPE P recsocket;
DLL_SCOPE P maxsocket;
DLL_SCOPE P consolesocket;
DLL_SCOPE fd_set sock_fds;            /* active sockets                 */
DLL_SCOPE _dm_const char* startup_dir; // setup by makefile,
                                   // defines which directory
                                   // to use for the startup file
DLL_SCOPE B* startup_dir_frame; // points the frame holding ^^^,
                                // at the bottom of the vm
DLL_SCOPE B* home_dir_frame; //points to the frame holding $HOME
DLL_SCOPE B* plugin_dir_frame; //points to the frame holding plugindir
DLL_SCOPE B* conf_dir_frame; //points to frame holding confdir
DLL_SCOPE B* myname_frame;  //points to frame holding my hostname
DLL_SCOPE B* myfqdn_frame; // point to frame holding my fully qualified domain name
DLL_SCOPE B* myxname_frame; //points to the frame buffering the DISPLAY name

DLL_SCOPE _dm_const UW ascii[];

/*----------------------- operator hands ------------------------------*/

#define o_1                  (FREEopds-FRAMEBYTES)
#define o_2                  (o_1-FRAMEBYTES)
#define o_3                  (o_2-FRAMEBYTES)
#define o_4                  (o_3-FRAMEBYTES)
#define o_5                  (o_4-FRAMEBYTES)
#define o_6                  (o_5-FRAMEBYTES)
#define o_7                  (o_6-FRAMEBYTES)
#define o_8                  (o_7-FRAMEBYTES)
#define o_9                  (o_8-FRAMEBYTES)
#define o_10                 (o_9-FRAMEBYTES)

#define o1                   FREEopds
#define o2                   (o1+FRAMEBYTES)
#define o3                   (o2+FRAMEBYTES)
#define o4                   (o3+FRAMEBYTES)
#define o5                   (o4+FRAMEBYTES)
#define o6                   (o5+FRAMEBYTES)
#define o7                   (o6+FRAMEBYTES)


#define x_1                  (FREEexecs-FRAMEBYTES)
#define x_2                  (x_1-FRAMEBYTES)
#define x_3                  (x_2-FRAMEBYTES)
#define x_4                  (x_3-FRAMEBYTES)
#define x_5                  (x_4-FRAMEBYTES)
#define x1                   FREEexecs
#define x2                   (x1+FRAMEBYTES)
#define x3                   (x2+FRAMEBYTES)
#define x4                   (x3+FRAMEBYTES)
#define x5                   (x4+FRAMEBYTES)
#define x6                   (x5+FRAMEBYTES)
#define x7                   (x6+FRAMEBYTES)

/*----------------------- function prototypes ------------------------*/

/*--- DM1 */
DLL_SCOPE P tokenize(B *stringframe);

/*-- dm-conv.c */
DLL_SCOPE void moveframe(B *socket, B *dest);
DLL_SCOPE void moveframes(B *socket, B *dest, P n);
DLL_SCOPE void moveB(B *socket, B *dest, P n);
DLL_SCOPE void moveW(W *socket, W *dest, P n);
DLL_SCOPE void moveL32(L32 *socket, L32 *dest, P n);
DLL_SCOPE void moveL64(L64 *socket, L64 *dest, P n);
DLL_SCOPE void moveLBIG(LBIG *socket, LBIG *dest, P n);
DLL_SCOPE void moveS(S *socket, S *dest, P n);
DLL_SCOPE void moveD(D *socket, D *dest, P n);

/*--- DMNUM */
DLL_SCOPE void DECODE(B *frame, BOOLEAN fauto, W prec, B *buf);
DLL_SCOPE B ENCODE(W type, B *string, B *dnum);
DLL_SCOPE W VALUEBYTES(B type);
DLL_SCOPE BOOLEAN VALUE(B *frame, LBIG *val);
DLL_SCOPE BOOLEAN DVALUE(B *frame, D *val);
DLL_SCOPE W TEST(B *frame);
DLL_SCOPE W COMPARE(B *frame1, B *frame2);
DLL_SCOPE void THEARC(B *dframe, B *sframe);
DLL_SCOPE void MOD(B *dframe, B *sframe);
DLL_SCOPE void MOVE(B *sframe, B *dframe);
DLL_SCOPE void ADD(B *dframe, B *sframe);
DLL_SCOPE void SUB(B *dframe, B *sframe);
DLL_SCOPE void MUL(B *dframe, B *sframe);
DLL_SCOPE void DIV(B *dframe, B *sframe);
DLL_SCOPE void PWR(B *dframe, B *sframe);
DLL_SCOPE void NEG(B *frame);
DLL_SCOPE void ABS(B *frame);
DLL_SCOPE void SQRT(B *frame);
DLL_SCOPE void EXP(B *frame);
DLL_SCOPE void LN(B *frame);
DLL_SCOPE void LG(B *frame);
DLL_SCOPE void FLOOR(B *frame);
DLL_SCOPE void CEIL(B *frame);
DLL_SCOPE void SIN(B *frame);
DLL_SCOPE void COS(B *frame);
DLL_SCOPE void TAN(B *frame);
DLL_SCOPE void ASIN(B *frame);
DLL_SCOPE void ACOS(B *frame);
DLL_SCOPE void ATAN(B *frame);
DLL_SCOPE void DECREMENT(B *frame);

/*----------------------- system operators */
DLL_SCOPE P op_pop(void);
DLL_SCOPE P op_exch(void);
DLL_SCOPE P op_dup(void);
DLL_SCOPE P op_copy(void);
DLL_SCOPE P op_index(void);
DLL_SCOPE P op_roll(void);
DLL_SCOPE P op_clear(void);
DLL_SCOPE P op_count(void);
DLL_SCOPE P op_cleartomark(void);
DLL_SCOPE P op_counttomark(void);
/*-- dictionary, array, list */
DLL_SCOPE P op_currentdict(void);
DLL_SCOPE P op_closelist(void); 
DLL_SCOPE P op_dict(void);
DLL_SCOPE P op_cleardict(void);
DLL_SCOPE P op_array(void);
DLL_SCOPE P op_list(void);
DLL_SCOPE P op_used(void);
DLL_SCOPE P op_length(void); 
DLL_SCOPE P op_last(void);
DLL_SCOPE P op_begin(void);
DLL_SCOPE P op_end(void);
DLL_SCOPE P op_def(void);
DLL_SCOPE P op_name(void);
DLL_SCOPE P op_find(void);
DLL_SCOPE P op_get(void);
DLL_SCOPE P op_put(void);
DLL_SCOPE P op_known(void);
DLL_SCOPE P op_getinterval(void);
DLL_SCOPE P op_countdictstack(void);
DLL_SCOPE P op_dictstack(void);
/*-- VM and miscellaneous */
DLL_SCOPE P op_save(void);
DLL_SCOPE P op_capsave(void);
DLL_SCOPE P op_restore(void);
DLL_SCOPE P op_vmstatus(void);
DLL_SCOPE P op_bind(void);
DLL_SCOPE P op_null(void);
/*-- control */
DLL_SCOPE P op_start(void);
DLL_SCOPE P op_exec(void);
DLL_SCOPE P op_if(void);
DLL_SCOPE P op_ifelse(void);
DLL_SCOPE P op_for(void);
DLL_SCOPE P op_repeat(void);
DLL_SCOPE P op_loop(void);
DLL_SCOPE P op_forall(void);
DLL_SCOPE P op_exit(void);
DLL_SCOPE P op_stop(void);
DLL_SCOPE P op_stopped(void);
DLL_SCOPE P op_countexecstack(void);
DLL_SCOPE P op_execstack(void);
/*-- math */
DLL_SCOPE P op_checkFPU(void);
DLL_SCOPE P op_neg(void);
DLL_SCOPE P op_abs(void);
DLL_SCOPE P op_thearc(void);
DLL_SCOPE P op_add(void);
DLL_SCOPE P op_mod(void);
DLL_SCOPE P op_sub(void);
DLL_SCOPE P op_mul(void);
DLL_SCOPE P op_div(void);
DLL_SCOPE P op_sqrt(void);
DLL_SCOPE P op_exp(void);
DLL_SCOPE P op_ln(void);
DLL_SCOPE P op_lg(void);
DLL_SCOPE P op_pwr(void);
DLL_SCOPE P op_cos(void);
DLL_SCOPE P op_sin(void);
DLL_SCOPE P op_tan(void);
DLL_SCOPE P op_atan(void);
DLL_SCOPE P op_floor(void);
DLL_SCOPE P op_ceil(void);
DLL_SCOPE P op_asin(void);
DLL_SCOPE P op_acos(void);
/*-- relational, boolean, bitwise */ 
DLL_SCOPE P op_eq(void);
DLL_SCOPE P op_ne(void);
DLL_SCOPE P op_ge(void);
DLL_SCOPE P op_gt(void);
DLL_SCOPE P op_le(void);
DLL_SCOPE P op_lt(void);
DLL_SCOPE P op_and(void);
DLL_SCOPE P op_not(void);
DLL_SCOPE P op_or(void);
DLL_SCOPE P op_xor(void);
DLL_SCOPE P op_bitshift(void);
/*-- conversion, string, attribute, class ,type */
DLL_SCOPE P op_class(void);
DLL_SCOPE P op_type(void);
DLL_SCOPE P op_readonly(void);
DLL_SCOPE P op_active(void);
DLL_SCOPE P op_tilde(void);
DLL_SCOPE P op_mkread(void);
DLL_SCOPE P op_mkact(void);
DLL_SCOPE P op_mkpass(void);
DLL_SCOPE P op_ctype(void);
DLL_SCOPE P op_parcel(void);
DLL_SCOPE P op_text(void);
DLL_SCOPE P op_number(void);
DLL_SCOPE P op_token(void);
DLL_SCOPE P op_search(void);
DLL_SCOPE P op_anchorsearch(void);

/*-- time/date and file access  */
DLL_SCOPE P op_gettime(void);
DLL_SCOPE P op_localtime(void);
DLL_SCOPE P op_getwdir(void);
DLL_SCOPE P op_setwdir(void);
DLL_SCOPE P op_writefile(void);
DLL_SCOPE P op_readfile(void);
DLL_SCOPE P op_findfiles(void);
DLL_SCOPE P op_findfile(void);
DLL_SCOPE P op_readboxfile(void);
DLL_SCOPE P op_writeboxfile(void); 
DLL_SCOPE P op_tosystem(void);
DLL_SCOPE P op_fromsystem(void);
DLL_SCOPE P op_transcribe(void);

/*-- more big operators.... */
DLL_SCOPE P op_fax(void);
DLL_SCOPE P op_merge(void);
DLL_SCOPE P op_nextobject(void);
DLL_SCOPE P op_interpolate(void);
DLL_SCOPE P op_integrateOH(void);
DLL_SCOPE P op_extrema(void);
DLL_SCOPE P op_solvetridiag(void);
DLL_SCOPE P op_integrateOHv(void);
DLL_SCOPE P op_tile(void);
DLL_SCOPE P op_ramp(void);
DLL_SCOPE P op_extract(void);
DLL_SCOPE P op_dilute(void);
DLL_SCOPE P op_ran1(void);
DLL_SCOPE P op_solve_bandmat(void);
DLL_SCOPE P op_complexFFT(void);
DLL_SCOPE P op_realFFT(void);
DLL_SCOPE P op_sineFFT(void);
DLL_SCOPE P op_decompLU(void);
DLL_SCOPE P op_backsubLU(void);
DLL_SCOPE P op_integrateRS(void);
DLL_SCOPE P op_bandLU(void);
DLL_SCOPE P op_bandBS(void);
DLL_SCOPE P op_invertLU(void);
DLL_SCOPE P op_matmul(void);
DLL_SCOPE P op_mattranspose(void);
DLL_SCOPE P op_dilute_add(void);
DLL_SCOPE P op_matvecmul(void);

#define DVTSTRINGBUFSIZE 8192
#define ERRLEN (1000)

#include "error-local.h"
#include "dm-snprintf.h"

#include "dm-types.h"
DM_INLINE_STATIC BOOLEAN PVALUE(B* frame, P* var) {
#if DM_HOST_IS_32_BIT
  LBIG v;
  if (!VALUE(frame, &v) || v > PMAX || v < -PMAX)
    return FALSE;
  *var = (P) v;
  return TRUE;
#else
  return VALUE(frame, var);
#endif // DM_HOST_IS_32_BIT
}

#if __cplusplus
}
#endif		

#endif //DM_H
