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

#define BOOL_VAL(frame)      (*((BOOLEAN *)((frame)+2)))
#define NAME_KEY(frame)      (*((W *)((frame)+2)))

#define NUM_VAL(frame)       ( ((B *)PF_PTR(frame,1)))
#define LONGBIG_VAL(frame)   (*((LBIG*)PF_PTR(frame,1)))

#define VALUE_BASE(frame)    (*((P *)PF_PTR(frame,1)))
#define VALUE_PTR(frame)     (*((B**)PF_PTR(frame,1)))

#define OP_CODE(frame)       (*((P *)PF_PTR(frame,1)))
#define OP_NAME(frame)       (*((P *)PF_PTR(frame,2)))
#define LIST_CEIL(frame)     (*((P *)PF_PTR(frame,2)))
#define ARRAY_SIZE(frame)    (*((P *)PF_PTR(frame,2)))
#define DICT_NB(frame)       (*((P *)PF_PTR(frame,2)))
#define DICT_CURR(frame)     (*((P *)PF_PTR(frame,2)))
#define BOX_NB(frame)        (*((P *)PF_PTR(frame,2)))
#define LIST_CEIL_PTR(frame) (*((B**)PF_PTR(frame,2)))

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

#define LIB_DATA(frame)       ((B*)(VALUE_BASE(frame) + DICT_NB(frame)))

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
#define SBOX_FLAGS(box)       (*(P *)  (box))
#define SBOX_DATA(box)        ( (P)    PF_PTR(box,1))
#define SBOX_CAP(box)         (*(B **) PF_PTR(box,3))

#define SBOX_FLAGS_CLEANUP    ((UP) 0x01)
#define SBOX_DATA_SIZE        (2*PACK_FRAME)
#define SBOXBYTES             DALIGN(4*PACK_FRAME)

/*--------------------------------------------- Internal message codes */

#define OK          0x00000000L /* o.k.                                  */
#define DONE        0x00000001L /* you got it                            */
#define TIMER       0x00000002L /*                                       */
#define WAIT        0x00000003L /*                                       */
#define MORE        0x00000004L /*                                       */
#define ABORT       0x00000005L /* ABORT signal sent from console        */
#define QUIT        0x00000006L /*                                       */

#define CORR_OBJ    0x00000101L /* corrupted object                      */
#define LOST_CONN   0x00000102L /* network connection lost               */

#define VM_OVF      0x00000200L /* VM overflow                           */
#define OPDS_OVF    0x00000201L /* operand stack overflow                */
#define EXECS_OVF   0x00000202L /* execution stack overflow              */
#define DICTS_OVF   0x00000203L /* dictionary stack overflow             */
#define OPDS_UNF    0x00000204L /* operand stack underflow               */
#define DICTS_UNF   0x00000205L /* dictionary stack underflow            */
#define EXECS_UNF   0x00000206L /* execution stack underflow             */
#define INV_EXT     0x00000207L /* invalid exit                          */
#define INV_STOP    0x00000208L /* invalid stop                          */
#define EXECS_COR   0x00000209L /* execution stack corrupted             */
#define SAVE_OVF    0x0000020AL /* save stack overflow                   */
#define INV_REST    0x0000020BL /* invalid restore                       */
#define SAVE_UNF    0x0000020DL /* save stack underflow                  */
#define ILL_OPAQUE  0x0000020EL /* Opaque dict type mismatch             */
#define FOLD_OPAQUE 0x0000020FL /* Illegal attempt to box opaque object  */

#define VMR_ERR     0x00000210L /* couldn't allocate memory              */
#define VMR_STATE   0x00000211L /* vm already tiny                       */
#define KILL_SOCKS  0x00000212L /* dvt must kill all non-server socks    */
#define MEM_OVF     0x00000213L /* failed memory allocation              */
#define CLOCK_ERR   0x00000214L /* failed to get epoch                  */

#define BAD_TOK     0x00000300L /* bad D token in source string          */
#define BAD_ASC     0x00000301L /* bad ASCII character in source string  */
#define ARR_CLO     0x00000302L /* unmatched array closure               */
#define CLA_ARR     0x00000303L /* illegal class in array                */
#define PRO_CLO     0x00000304L /* unmatched procedure closure           */

#define OPD_TYP     0x00000400L /* illegal operand type                  */
#define OPD_CLA     0x00000401L /* illegal operand class                 */
#define RNG_CHK     0x00000402L /* range check error                     */
#define OPD_ATR     0x00000403L /* illegal operand attribute             */
#define UNDF        0x00000404L /* undefined name                        */
#define OPD_ERR     0x00000405L /* wrong operand class or type           */
#define DICT_ATR    0x00000406L /* write attempt in read-only dictionary */
#define DICT_OVF    0x00000407L /* dictionary overflow                   */
#define DICT_USED   0x00000408L /* copying into used dictionary          */
#define UNDF_VAL    0x00000409L /* using undefined number (NAN)          */
#define DIR_NOSUCH  0x00000501L /* no such directory/volume              */

#define CORR_OP     0x00000700L /* OP: operator array is corrupted       */
#define BADBOX      0x00000701L /* file does not hold a box              */
#define BAD_MSG     0x00000703L /* bad message received via network      */
#define NOSYSTEM    0x00000704L /* 'system' call failed                  */
#define INV_MSG     0x00000705L /* operand constitutes invalid message   */
#define NOT_HOST    0x00000706L /* hostname not in hosts file            */
#define BAD_FMT     0x00000707L /* message not in native format          */
#define LONG_OVF    0x00000708L /* 64 bit long doesn't fit in 32 bit long*/

#define LIB_LOAD    0x00000807L /* unable to dlload                      */
#define LIB_EXPORT  0x00000808L /* unable to find object in lib          */
#define LIB_LINK    0x00000809L /* lib has not been loaded               */
#define LIB_ADD     0x0000090AL /* unable to add op to lib dict          */
#define LIB_LOADED  0x0000090BL /* lib already loaded                    */
#define LIB_OVF     0x0000090CL /* malloc in lib overflowed              */
#define LIB_MERGE   0x0000090DL /* out of space in sysdict for merge     */
#define LIB_INIT    0x0000090EL /* __init failed for lib */
#define NOPLUGINS   0x0000090FL /* compiled without plugins */

#define NO_XWINDOWS 0x00000A01L /* X windows unavailable                 */
#define X_ERR       0x00000A02L /* X lib error                           */
#define X_BADFONT   0x00000A03L /* X font does not exist                 */
#define X_BADHOST   0x00000A04L /* X server cannot be connected          */

#define BAD_ARR     0x00000B00L /* dmnuminc debug error */
#define SBOX_SET    0x00000B01L /* box already has a cleanup */		

#if DM_ENABLE_REGEX		
#define REGEX_BADPAT   0x00000C01L /* Invalid regular expression */
#define REGEX_ECOLLATE 0x00000C02L /* Invalid collating element */
#define REGEX_ECTYPE   0x00000C03L /* Invalid character class */
#define REGEX_EESCAPE  0x00000C04L /* `\' applied to unescapable character */
#define REGEX_ESUBREG  0x00000C05L /* invalid backreference number */
#define REGEX_EBRACK   0x00000C06L /* brackets `[]' not balanced*/
#define REGEX_EPAREN   0x00000C07L /* paranthesis `()' not balanced */
#define REGEX_EBRACE   0x00000C08L /* braces `{}' not balanced */
#define REGEX_BADBR    0x00000C09L /* invalid repetition count(s) in `{}' */
#define REGEX_ERANGE   0x00000C0AL /* invalid character rangin in `[]' */
#define REGEX_ESPACE   0x00000C0BL /* ran out of memory */
#define REGEX_BADRPT   0x00000C0CL /* `?', `*', or `+' operand invalid */
#define REGEX_UNKNOWN  0x00000C0DL /* Unknown error */
#endif // DM_ENABLE_REGEX

#define MATRIX_ERRS    0x00000D00L /* 0D00:0E00-1 for errors in matrix.h */

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

DLL_SCOPE B locked;
DLL_SCOPE B serialized;

DLL_SCOPE fd_set sock_fds;
DLL_SCOPE BOOLEAN timeout;             /* for I/O operations          */
DLL_SCOPE BOOLEAN abortflag;
DLL_SCOPE BOOLEAN numovf;             /* FPU overflow status            */
DLL_SCOPE BOOLEAN tinymemory;
DLL_SCOPE P recsocket;
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
DLL_SCOPE UW ascii[];

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

void makeDmemory(B *mem, L64 specs[5]);

/*--- DM1 */
P tokenize(B *stringframe);

/*--- DM2 */
P wrap_hi(B* hival);
P wrap_libnum(UP libnum);
B* nextlib(B* frame);
B* geterror(P e);
B *makedict(L32 n);
void cleardict(B *dict);
B *makeopdictbase(B *opdefs, P *errc, B **errm, L32 n1);
B *makeopdict(B *opdefs, P *errc, B **errm);
void d_reloc(B *dict, P oldd, P newd);
void d_rreloc(B *dict, P oldd, P newd);
void makename(B *namestring, B *nameframe);
void pullname(B *nameframe, B *namestring);
P compname(B *nameframe1, B *nameframe2);
BOOLEAN matchname(B *nameframe1, B *nameframe2);		
void moveframe(B *source, B *dest);
void moveframes(B *source, B *dest, P n);
void moveB(B *source, B *dest, P n);
void moveW(W *source, W *dest, P n);
void moveL32(L32 *source, L32 *dest, P n);
void moveL64(L64 *source, L64 *dest, P n);
void moveLBIG(LBIG *source, LBIG *dest, P n);
void moveS(S *source, S *dest, P n);
void moveD(D *source, D *dest, P n);
B *lookup(B *nameframe, B *dict);
BOOLEAN insert(B *nameframe, B *dict, B *framedef);
BOOLEAN mergedict(B *source, B *sink);
P exec(L32 turns);
P foldobj(B *frame, P base, W *depth);
P unfoldobj(B *frame, P base, B isnonnative);
P foldobj_ext(B* frame, P extra);
BOOLEAN foldobj_mem(B** base, B** top);
void foldobj_free(void);
P deendian_frame(B *frame, B isnonnative);
//L deendian_array(B* frame, B isnonnative);
//L deendian_list(B* frame, B isnonnative);
//L deendian_dict(B* dict, B isnonnative);
//L deendian_entries(B* doct, B isnonnative);
void setupdirs(void);

/*--- DM3 */
P make_socket(UW port);
P make_unix_socket(UW port);
P fromsocket(P socket, B *msf);
P tosocket(P socket, B *sf, B *cf);
P toconsole(B *string, P stringlength);

/*--- DMNUM */
void DECODE(B *frame, BOOLEAN fauto, W prec, B *buf);
B ENCODE(W type, B *string, B *dnum);
W VALUEBYTES(B type);
BOOLEAN VALUE(B *frame, LBIG *val);
BOOLEAN DVALUE(B *frame, D *val);
W TEST(B *frame);
W COMPARE(B *frame1, B *frame2);
void THEARC(B *dframe, B *sframe);
void MOD(B *dframe, B *sframe);
void MOVE(B *sframe, B *dframe);
void ADD(B *dframe, B *sframe);
void SUB(B *dframe, B *sframe);
void MUL(B *dframe, B *sframe);
void DIV(B *dframe, B *sframe);
void PWR(B *dframe, B *sframe);
void NEG(B *frame);
void ABS(B *frame);
void SQRT(B *frame);
void EXP(B *frame);
void LN(B *frame);
void LG(B *frame);
void FLOOR(B *frame);
void CEIL(B *frame);
void SIN(B *frame);
void COS(B *frame);
void TAN(B *frame);
void ASIN(B *frame);
void ACOS(B *frame);
void ATAN(B *frame);
void DECREMENT(B *frame);

/*----------------------- system operators */
P op_serialize(void);
P op_lock(void);
P op_syshi(void);
P op_syslibnum(void);
P op_error(void);
P op_errormessage(void);
P op_aborted(void);
P op_abort(void);
P op_halt(void);
P op_continue(void);
P op_quit(void);
P op_setconsole(void);
P op_console(void);
P op_toconsole(void);
P op_tostderr(void);
P op_connect(void);
P op_disconnect(void);
P op_send(void);
P op_flush(void);
P op_nextevent(void);
P op_vmresize(void);
P op_killsockets(void);
P op_getstartupdir(void);
P op_getconfdir(void);
P op_gethomedir(void);
P op_getsocket(void);
P op_getmyname(void);
P op_getmyport(void);

P op_pop(void);
P op_exch(void);
P op_dup(void);
P op_copy(void);
P op_index(void);
P op_roll(void);
P op_clear(void);
P op_count(void);
P op_cleartomark(void);
P op_counttomark(void);
/*-- dictionary, array, list */
P op_currentdict(void);
P op_closelist(void); 
P op_dict(void);
P op_cleardict(void);
P op_array(void);
P op_list(void);
P op_used(void);
P op_length(void); 
P op_begin(void);
P op_end(void);
P op_def(void);
P op_name(void);
P op_find(void);
P op_get(void);
P op_put(void);
P op_known(void);
P op_getinterval(void);
P op_countdictstack(void);
P op_dictstack(void);
/*-- VM and miscellaneous */
P op_save(void);
P op_capsave(void);
P op_restore(void);
P op_setcleanup(void);
P op_vmstatus(void);
P op_bind(void);
P op_null(void);
/*-- control */
P op_start(void);
P op_exec(void);
P op_if(void);
P op_ifelse(void);
P op_for(void);
P op_repeat(void);
P op_loop(void);
P op_forall(void);
P op_exit(void);
P op_stop(void);
P op_stopped(void);
P op_countexecstack(void);
P op_execstack(void);
/*-- math */
P op_checkFPU(void);
P op_neg(void);
P op_abs(void);
P op_thearc(void);
P op_add(void);
P op_mod(void);
P op_sub(void);
P op_mul(void);
P op_div(void);
P op_sqrt(void);
P op_exp(void);
P op_ln(void);
P op_lg(void);
P op_pwr(void);
P op_cos(void);
P op_sin(void);
P op_tan(void);
P op_atan(void);
P op_floor(void);
P op_ceil(void);
P op_asin(void);
P op_acos(void);
/*-- relational, boolean, bitwise */ 
P op_eq(void);
P op_ne(void);
P op_ge(void);
P op_gt(void);
P op_le(void);
P op_lt(void);
P op_and(void);
P op_not(void);
P op_or(void);
P op_xor(void);
P op_bitshift(void);
/*-- conversion, string, attribute, class ,type */
P op_class(void);
P op_type(void);
P op_readonly(void);
P op_active(void);
P op_tilde(void);
P op_mkread(void);
P op_mkact(void);
P op_mkpass(void);
P op_ctype(void);
P op_parcel(void);
P op_text(void);
P op_number(void);
P op_token(void);
P op_search(void);
P op_anchorsearch(void);

/*-- time/date and file access  */
P op_gettime(void);
P op_localtime(void);
P op_getwdir(void);
P op_setwdir(void);
P op_writefile(void);
P op_readfile(void);
P op_findfiles(void);
P op_findfile(void);
P op_readboxfile(void);
P op_writeboxfile(void); 
P op_tosystem(void);
P op_fromsystem(void);
P op_transcribe(void);

/*-- more big operators.... */
P op_fax(void);
P op_merge(void);
P op_nextobject(void);
P op_interpolate(void);
P op_integrateOH(void);
P op_extrema(void);
P op_solvetridiag(void);
P op_integrateOHv(void);
P op_tile(void);
P op_ramp(void);
P op_extract(void);
P op_dilute(void);
P op_ran1(void);
P op_solve_bandmat(void);
P op_complexFFT(void);
P op_realFFT(void);
P op_sineFFT(void);
P op_decompLU(void);
P op_backsubLU(void);
P op_integrateRS(void);
P op_bandLU(void);
P op_bandBS(void);
P op_invertLU(void);
P op_matmul(void);
P op_mattranspose(void);
P op_dilute_add(void);
P op_matvecmul(void);

#if DM_ENABLE_REGEX
P op_regex(void);
P op_regexi(void);
#endif //DM_ENABLE_REGEX

#define ERRLEN (1000)

#include "error-local.h"
#include "dm-snprintf.h"

#include "dm-types.h"

#if __cplusplus
}
#endif		

#endif //DM_H
