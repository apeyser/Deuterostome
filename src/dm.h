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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// Need to be here because we define things such as x1
#if ! X_DISPLAY_MISSING
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

#include <inttypes.h>
#include <sys/socket.h>
#include <sys/select.h>
#if ! NO_ENDIAN_HDR
#include ENDIAN_HDR
#endif //NO_ENDIAN_HDR

typedef int8_t B;
typedef int16_t W;
typedef int32_t L;
typedef uint8_t UB;
typedef uint16_t UW;
typedef uint32_t UL;

typedef W BOOLEAN;
typedef L INT;

typedef  L  (*OPER)(void);

#ifndef TRUE
#define TRUE -1
#endif
#ifndef FALSE
#define FALSE 0
#endif

typedef float S;
typedef double D;

#define BINF    ((B) 0x80)
#define WINF    ((W) 0x8000)
#define LINF    ((L) 0x80000000)
static const S SINF __attribute__ ((unused)) = 1.0/0.0;  
static const D DINF __attribute__ ((unused)) = 1.0/0.0;

#define BMAX 0x7F
#define WMAX 0x7FFF
#define LMAX 0x7FFFFFFF

/*-------------------------- VM alignment ----------------------------

NOTE: all objects that can populate the D machine's workspace must
      be aligned using the macro defined here.
*/

#define DALIGN(bytes)         ((((UL)(bytes)+7)>>3)<<3)  /* 8 bytes */

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
#define HANDLE                     ((UB) 0xA0)
	
#define COMPOSITE(frame) \
	((UB)(CLASS(frame)) > (UB)(MARK) && TAG(frame) != (HANDLE | SIMPLETYPE))

#define BYTETYPE                   ((UB) 0x00)       /* numeral types */
#define WORDTYPE                   ((UB) 0x01)
#define LONGTYPE                   ((UB) 0x02)
#define SINGLETYPE                 ((UB) 0x03)
#define DOUBLETYPE                 ((UB) 0x04)      
#define SOCKETTYPE                 ((UB) 0x01)      /* null types */
#define SIMPLETYPE                 ((UB) 0x00)      /* handle types */
#define COMPLEXTYPE                ((UB) 0x00)      
#define CMACHINE                   ((UB) 0x00)      /* operator types */
#define OPLIBTYPE                  ((UB) 0x01)      /* operator lib type */

/* attributes qualify a frame, not an object value: */

#define ACTIVE                     ((UB)0x01)  
#define READONLY                   ((UB)0x02)
#define PARENT                     ((UB)0x04)
#define TILDE                      ((UB)0x08)

/* Composite endianness */
#define BIGENDIAN                  ((UB) 0x00)
#define LITTLEENDIAN               ((UB) 0x01)
#define ENDIANMASK                 ((UB) 0x01)
/* Format specifier */
#define FORMAT32                   ((UB) 0x10)
#define FORMATMASK                 ((UB) 0xF0)

// make BSD and Linux look alike

#if ! NO_ENDIAN_HDR && ! BYTE_ORDER
#define BYTE_ORDER __BYTE_ORDER
#define LITTLE_ENDIAN __LITTLE_ENDIAN
#define BIG_ENDIAN __BIG_ENDIAN
#endif // ! NO_ENDIAN_HDR && ! BYTE_ORDER

#ifndef WORDS_BIGENDIAN
#define GETNATIVEENDIAN(frame)  \
  ((BOOLEAN) ((FORMAT(frame) & ENDIANMASK) == LITTLEENDIAN))
#define SETNATIVEENDIAN(frame)  FORMAT(frame) |= LITTLEENDIAN
#else //! WORDS_BIGENDIAN
#define GETNATIVEENDIAN(frame)  \
  ((BOOLEAN) ((FORMAT(frame) & ENDIANMASK) == BIGENDIAN))
#define SETNATIVEENDIAN(frame)  FORMAT(frame) |= BIGENDIAN
#endif //WORDS_BIGENDIAN

#if ! NO_ENDIAN_HDR && __FLOAT_WORD_ORDER
#if __FLOAT_WORD_ORDER != __BYTE_ORDER
#error "Can't handle float word order __FLOAT_WORD_ORDER"
#endif //__FLOAT_WORD_ORDER
//#else //! NO_ENDIAN_HDR
//#warning "Confirm that float word order = word order"
#endif //! NO_ENDIAN_HDR

#define GETNATIVEFORMAT(frame) \
  ((BOOLEAN) ((FORMAT(frame) & FORMATMASK) == FORMAT32))
#define SETNATIVEFORMAT(frame) FORMAT(frame) |= FORMAT32

#define GETNATIVE(frame) (GETNATIVEFORMAT(frame) && GETNATIVEENDIAN(frame))
#define SETNATIVE(frame) \
  FORMAT(frame) = 0; \
  SETNATIVEFORMAT(frame); \
  SETNATIVEENDIAN(frame)

#define GETNATIVEUNDEF(frame) \
  ((BOOLEAN) ! (FORMAT(frame) & ~(FORMATMASK | ENDIANMASK)))

#define EXITMARK                   ((UB)0x10)   /* execstack marks */
#define STOPMARK                   ((UB)0x20)
#define ABORTMARK                  ((UB)0x40)
#define XMARK                      ((UB)0x70)
#define BIND                       ((UB)0x80)   /* box op housekeeping */

#define NUM_VAL(frame)             ( ((B *)(((B*)(frame))+4)))
#define LONG_VAL(frame)            (*((L *)((frame)+4)))
#define BOOL_VAL(frame)            (*((BOOLEAN *)((frame)+2)))
#define NAME_KEY(frame)            (*((W *)((frame)+2)))

#define OP_CODE(frame)             (*((L *)(((B*)(frame))+4)))
#define OP_NAME(frame)             (*((L *)(((B*)(frame))+8)))
#define VALUE_BASE(frame)          (*((L *)(((B*)(frame))+4)))
#define ARRAY_SIZE(frame)          (*((L *)(((B*)(frame))+8)))
#define LIST_CEIL(frame)           (*((L *)(((B*)(frame))+8)))
#define DICT_NB(frame)             (*((L *)(((B*)(frame))+8)))
#define DICT_CURR(frame)           (*((L *)(((B*)(frame))+8)))
#define BOX_NB(frame)              (*((L *)(((B*)(frame))+8)))
#define VALUE_PTR(frame)           (*((B**)(((B*)(frame))+4)))
#define HANDLE_ID(frame)           (*((L *)(((B*)(frame))+12)))
#define HANDLE_CEIL(frame)         (*((B**)(((B*)(frame))+8)))

static void SET_HANDLE_ID(B* frame, B* string) {
	HANDLE_ID(frame) = *(L*)(string);
}
static void GET_HANDLE_ID(B* frame, B* string) {
	*(L*)(string) = HANDLE_ID(frame);
}
static BOOLEAN EQ_HANDLE_ID(B* frame1, B* frame2) {
	return HANDLE_ID(frame1) == HANDLE_ID(frame2);
}
static BOOLEAN EQ_HANDLE_ID_STRING(B* frame, B string[5]) {
	return HANDLE_ID(frame) == *(L*)string;
}

/* NB: Attention to moveframe & moveframes in dm2.c whenever
   framebytes is changed */
#define FRAMEBYTES                 16L

// NAMEBYTES is defined in ../config.h
// To change, update ../configure.ac
//#define NAMEBYTES                  18L //not including a terminating 0

/*-------------------------------------------- dictionary */
 
#define ASSOC_NAME(entry)          ( ((B *)(entry)))
#define ASSOC_NEXT(entry)          (*((L *)(((B*)(entry))+FRAMEBYTES)))
#define ASSOC_FRAME(entry)         (((B*)(entry))+ENTRYBYTES-FRAMEBYTES)

// keep frame on 64 bit boundaries, so that doubles will be so.
#define ENTRYBYTES  (8+2*FRAMEBYTES)

#define DICT_ENTRIES(dict)         (*((L *)(dict)))
#define DICT_FREE(dict)            (*((L *)(((B*)(dict))+4)))
#define DICT_CEIL(dict)            (*((L *)(((B*)(dict))+8)))
#define DICT_CONHASH(dict)         (*((W *)(((B*)(dict))+12)))
#define DICT_TABHASH(dict)         (*((L *)(((B*)(dict))+8)))


#define DICTBYTES                  16L

#define LIB_DATA(frame)            ((B*)(VALUE_BASE(frame) + DICT_NB(frame)))

#define LIB_TYPE(frame)            (*(L*) (LIB_DATA(frame)))
#define LIB_HANDLE(frame)          (*(L*) (LIB_DATA(frame) + 4))
#define LIB_ERRC(frame)            (*(L**)(LIB_DATA(frame) + 8))
#define LIB_ERRM(frame)            (*(B***)(LIB_DATA(frame) + 12))

#define LIBBYTES                   16L

/*---------------------------------- C Operator definition */

#define OPDEF_NAME(operator)       (*((L *)(operator)))
#define OPDEF_CODE(operator)       (*((L *)(((B*)operator)+4)))

#define OPDEFBYTES                 8L

/*---------------------------------------- save box */
#define SBOX_NOPDS(box)          (*(L *)(box))
#define SBOX_NDICTS(box)         (*(L *)(((B*)(box))+4))
#define SBOX_CAP(box)            (*(B **)(((B*)(box))+8))

#define SBOXBYTES                16

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
#define ILL_HANDLE  0x0000020EL /* handle type mismatch                  */

#define VMR_ERR     0x00000210L /* couldn't allocate memory              */
#define VMR_STATE   0x00000211L /* vm already tiny                       */
#define KILL_SOCKS  0x00000212L /* dvt must kill all non-server socks    */

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
#define BAD_FMT     0x00000707L /* message not in native format */

#define LIB_LOAD    0x00000807L /* unable to dlload                      */
#define LIB_EXPORT  0x00000808L /* unable to find object in lib          */
#define LIB_LINK    0x00000809L /* lib has not been loaded               */
#define LIB_ADD     0x0000090AL /* unable to add op to lib dict          */
#define LIB_LOADED  0x0000090BL /* lib already loaded                    */
#define LIB_OVF     0x0000090CL /* malloc in lib overflowed              */
#define LIB_MERGE   0x0000090DL /* out of space in sysdict for merge     */
#define LIB_INIT    0x0000090EL /* __init failed for lib */

#define NO_XWINDOWS 0x00000A01L /* X windows unavailable                 */
#define X_ERR       0x00000A02L /* X lib error                           */
#define X_BADFONT   0x00000A03L /* X font does not exist                 */
#define X_BADHOST   0x00000A04L /* X server cannot be connected          */

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

DLL_SCOPE fd_set sock_fds;
DLL_SCOPE BOOLEAN timeout;             /* for I/O operations          */
DLL_SCOPE BOOLEAN abortflag;
DLL_SCOPE BOOLEAN numovf;             /* FPU overflow status            */
DLL_SCOPE BOOLEAN tinymemory;
DLL_SCOPE L recsocket;
DLL_SCOPE L consolesocket;
DLL_SCOPE fd_set sock_fds;            /* active sockets                 */
DLL_SCOPE const char* startup_dir; // setup by makefile,
                                   // defines which directory
                                   // to use for the startup file
DLL_SCOPE B* startup_dir_frame; // points the frame holding ^^^,
                                // at the bottom of the vm
DLL_SCOPE B* home_dir_frame; //points to the frame holding $HOME
DLL_SCOPE UW ascii[];

/*----------------------- operator hands ------------------------------*/

#define o_1                  (FREEopds-FRAMEBYTES)
#define o_2                  (o_1-FRAMEBYTES)
#define o_3                  (o_2-FRAMEBYTES)
#define o_4                  (o_3-FRAMEBYTES)
#define o_5                  (o_4-FRAMEBYTES)
#define o_6                  (o_5-FRAMEBYTES)
#define o_7                  (o_6-FRAMEBYTES)
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

void makeDmemory(B *mem, L specs[5]);

/*--- DM1 */
L tokenize(B *stringframe);

/*--- DM2 */
L wrap_hi(B* hival);
L wrap_libnum(UL libnum);
B* nextlib(B* frame);
B* geterror(L e);
B *makedict(L n);
void cleardict(B *dict);
B *makeopdictbase(B *opdefs, L *errc, B **errm, L n1);
B *makeopdict(B *opdefs, L *errc, B **errm);
void d_reloc(B *dict, L oldd, L newd);
void d_rreloc(B *dict, L oldd, L newd);
void makename(B *namestring, B *nameframe);
void pullname(B *nameframe, B *namestring);
BOOLEAN matchname(B *nameframe1, B *nameframe2);
void moveframe(B *source, B *dest);
void moveframes(B *source, B *dest, L n);
void moveB(B *source, B *dest, L n);
void moveW(W *source, W *dest, L n);
void moveL(L *source, L *dest, L n);
void moveS(S *source, S *dest, L n);
void moveD(D *source, D *dest, L n);
B *lookup(B *nameframe, B *dict);
BOOLEAN insert(B *nameframe, B *dict, B *framedef);
BOOLEAN mergedict(B *source, B *sink);
L exec(L turns);
L foldobj(B *frame, L base, W *depth);
L unfoldobj(B *frame, L base, BOOLEAN isnative);
L deendian_frame(B *frame);
//L deendian_array(B* frame);
//L deendian_list(B* frame);
//L deendian_dict(B* dict);
//L deendian_entries(B* doct);
void setupdirs(void);

/*--- DM3 */
L make_socket(L port);
L fromsocket(L socket, B *msf);
L tosocket(L socket, B *sf, B *cf);
L toconsole( B *string, L stringlength);

/*--- DMNUM */
void DECODE(B *frame, BOOLEAN fauto, W prec, B *buf);
B ENCODE(W type, B *string, B *dnum);
W VALUEBYTES(B type);
BOOLEAN VALUE(B *frame, L *val);
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
L op_lock(void);
L op_syshi(void);
L op_syslibnum(void);
L op_error(void);
L op_errormessage(void);
L op_aborted(void);
L op_abort(void);
L op_halt(void);
L op_continue(void);
L op_quit(void);
L op_setconsole(void);
L op_console(void);
L op_toconsole(void);
L op_tostderr(void);
L op_connect(void);
L op_disconnect(void);
L op_send(void);
L op_flush(void);
L op_nextevent(void);
L op_vmresize(void);
L op_killsockets(void);
L op_getstartupdir(void);
L op_gethomedir(void);
L op_getsocket(void);
L op_getmyname(void);
L op_getmyport(void);

L op_pop(void);
L op_exch(void);
L op_dup(void);
L op_copy(void);
L op_index(void);
L op_roll(void);
L op_clear(void);
L op_count(void);
L op_cleartomark(void);
L op_counttomark(void);
/*-- dictionary, array, list */
L op_currentdict(void);
L op_closelist(void); 
L op_dict(void);
L op_cleardict(void);
L op_array(void);
L op_list(void);
L op_used(void);
L op_length(void); 
L op_begin(void);
L op_end(void);
L op_def(void);
L op_name(void);
L op_find(void);
L op_get(void);
L op_put(void);
L op_known(void);
L op_getinterval(void);
L op_countdictstack(void);
L op_dictstack(void);
/*-- VM and miscellaneous */
L op_save(void);
L op_capsave(void);
L op_restore(void);
L op_vmstatus(void);
L op_bind(void);
L op_null(void);
/*-- control */
L op_start(void);
L op_exec(void);
L op_if(void);
L op_ifelse(void);
L op_for(void);
L op_repeat(void);
L op_loop(void);
L op_forall(void);
L op_exit(void);
L op_stop(void);
L op_stopped(void);
L op_countexecstack(void);
L op_execstack(void);
/*-- math */
L op_checkFPU(void);
L op_neg(void);
L op_abs(void);
L op_thearc(void);
L op_add(void);
L op_mod(void);
L op_sub(void);
L op_mul(void);
L op_div(void);
L op_sqrt(void);
L op_exp(void);
L op_ln(void);
L op_lg(void);
L op_pwr(void);
L op_cos(void);
L op_sin(void);
L op_tan(void);
L op_atan(void);
L op_floor(void);
L op_ceil(void);
L op_asin(void);
L op_acos(void);
/*-- relational, boolean, bitwise */ 
L op_eq(void);
L op_ne(void);
L op_ge(void);
L op_gt(void);
L op_le(void);
L op_lt(void);
L op_and(void);
L op_not(void);
L op_or(void);
L op_xor(void);
L op_bitshift(void);
/*-- conversion, string, attribute, class ,type */
L op_class(void);
L op_handleid(void);
L op_type(void);
L op_readonly(void);
L op_active(void);
L op_tilde(void);
L op_mkread(void);
L op_mkact(void);
L op_mkpass(void);
L op_ctype(void);
L op_parcel(void);
L op_text(void);
L op_number(void);
L op_token(void);
L op_search(void);
L op_anchorsearch(void);

/*-- time/date and file access  */
L op_gettime(void);
L op_localtime(void);
L op_getwdir(void);
L op_setwdir(void);
L op_writefile(void);
L op_readfile(void);
L op_findfiles(void);
L op_readboxfile(void);
L op_writeboxfile(void); 
L op_tosystem(void);
L op_transcribe(void);

/*-- more big operators.... */
L op_fax(void);
L op_merge(void);
L op_nextobject(void);
L op_interpolate(void);
L op_integrateOH(void);
L op_extrema(void);
L op_solvetridiag(void);
L op_integrateOHv(void);
L op_tile(void);
L op_ramp(void);
L op_extract(void);
L op_dilute(void);
L op_ran1(void);
L op_solve_bandmat(void);
L op_complexFFT(void);
L op_realFFT(void);
L op_sineFFT(void);
L op_decompLU(void);
L op_backsubLU(void);
L op_integrateRS(void);
L op_bandLU(void);
L op_bandBS(void);
L op_invertLU(void);
L op_matmul(void);
L op_mattranspose(void);
L op_dilute_add(void);
L op_matvecmul(void);

#define ERRLEN (1000)

#include "error.h"

#endif //DM_H
