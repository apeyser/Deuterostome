#define OK          (P)0x00000000L /* o.k.                                  */
#define DONE        (P)0x00000001L /* you got it                            */
#define TIMER       (P)0x00000002L /*                                       */
#define WAIT        (P)0x00000003L /*                                       */
#define MORE        (P)0x00000004L /*                                       */
#define ABORT       (P)0x00000005L /* ABORT signal sent from console        */
#define QUIT        (P)0x00000006L /*                                       */

#define CORR_OBJ    (P)0x00000101L /* corrupted object                      */
#define LOST_CONN   (P)0x00000102L /* network connection lost               */
#define ILL_SOCK    (P)0x00000103L /* sent signal to non-signalling socket  */


#define VM_OVF      (P)0x00000200L /* VM overflow                           */
#define OPDS_OVF    (P)0x00000201L /* operand stack overflow                */
#define EXECS_OVF   (P)0x00000202L /* execution stack overflow              */
#define DICTS_OVF   (P)0x00000203L /* dictionary stack overflow             */
#define OPDS_UNF    (P)0x00000204L /* operand stack underflow               */
#define DICTS_UNF   (P)0x00000205L /* dictionary stack underflow            */
#define EXECS_UNF   (P)0x00000206L /* execution stack underflow             */
#define INV_EXT     (P)0x00000207L /* invalid exit                          */
#define INV_STOP    (P)0x00000208L /* invalid stop                          */
#define EXECS_COR   (P)0x00000209L /* execution stack corrupted             */
#define INV_REST    (P)0x0000020BL /* invalid restore                       */
#define ILL_OPAQUE  (P)0x0000020EL /* Opaque dict type mismatch             */
#define FOLD_OPAQUE (P)0x0000020FL /* Illegal attempt to box opaque object  */

#define VMR_ERR     (P)0x00000210L /* couldn't allocate memory              */
#define VMR_STATE   (P)0x00000211L /* vm already tiny                       */
#define KILL_SOCKETS (P)0x00000212L /* dvt must kill all non-server socks   */
#define MEM_OVF     (P)0x00000213L /* failed memory allocation              */
#define CLOCK_ERR   (P)0x00000214L /* failed to get epoch                   */
#define DEAD_SOCKET (P)0x00000215L /* socket died - no special handler      */
#define BUF_OVF     (P)0x00000216L /* overflow in internal buffer           */

#define BAD_TOK     (P)0x00000300L /* bad D token in socket string          */
#define BAD_ASC     (P)0x00000301L /* bad ASCII character in socket string  */
#define ARR_CLO     (P)0x00000302L /* unmatched array closure               */
#define CLA_ARR     (P)0x00000303L /* illegal class in array                */
#define PRO_CLO     (P)0x00000304L /* unmatched procedure closure           */

#define OPD_TYP     (P)0x00000400L /* illegal operand type                  */
#define OPD_CLA     (P)0x00000401L /* illegal operand class                 */
#define RNG_CHK     (P)0x00000402L /* range check error                     */
#define OPD_ATR     (P)0x00000403L /* illegal operand attribute             */
#define UNDF        (P)0x00000404L /* undefined name                        */
#define OPD_ERR     (P)0x00000405L /* wrong operand class or type           */
#define DICT_ATR    (P)0x00000406L /* write attempt in read-only dictionary */
#define DICT_OVF    (P)0x00000407L /* dictionary overflow                   */
#define DICT_USED   (P)0x00000408L /* copying into used dictionary          */
#define UNDF_VAL    (P)0x00000409L /* using undefined number (NAN)          */
#define ILL_RECAP   (P)0x0000040AL /* Double capsave                        */
#define DIR_NOSUCH  (P)0x00000501L /* no such directory/volume              */
#define SOCK_STATE  (P)0x00000502L /* illegal state change for fd           */


#define BADBOX      (P)0x00000701L /* file does not hold a box              */
#define BAD_MSG     (P)0x00000703L /* bad message received via network      */
#define NOSYSTEM    (P)0x00000704L /* 'system' call failed                  */
#define INV_MSG     (P)0x00000705L /* operand constitutes invalid message   */
#define BAD_FMT     (P)0x00000707L /* message not in native format          */
#define LONG_OVF    (P)0x00000708L /* 64 bit long doesn't fit in 32 bit long*/

#define LIB_LOAD    (P)0x00000807L /* unable to dlload                      */
#define LIB_EXPORT  (P)0x00000808L /* unable to find object in lib          */
#define LIB_LINK    (P)0x00000809L /* lib has not been loaded               */
#define LIB_ADD     (P)0x0000090AL /* unable to add op to lib dict          */
#define LIB_LOADED  (P)0x0000090BL /* lib already loaded                    */
#define LIB_OVF     (P)0x0000090CL /* malloc in lib overflowed              */
#define LIB_MERGE   (P)0x0000090DL /* out of space in sysdict for merge     */
#define LIB_INIT    (P)0x0000090EL /* __init failed for lib */

#define BAD_ARR     (P)0x00000B00L /* dmnuminc debug error */

#define REGEX_ERRS     (P)0x00000C00L /* 0C00:0D00-1 for errors in dregex.h */
#define REGEX_BADPAT   (REGEX_ERRS+0) /* Invalid regular expression */
#define REGEX_ECOLLATE (REGEX_ERRS+1) /* Invalid collating element */
#define REGEX_ECTYPE   (REGEX_ERRS+2) /* Invalid character class */
#define REGEX_EESCAPE  (REGEX_ERRS+3) /* `\' applied to unescapable character */
#define REGEX_ESUBREG  (REGEX_ERRS+4) /* invalid backreference number */
#define REGEX_EBRACK   (REGEX_ERRS+5) /* brackets `[]' not balanced*/
#define REGEX_EPAREN   (REGEX_ERRS+6) /* paranthesis `()' not balanced */
#define REGEX_EBRACE   (REGEX_ERRS+7) /* braces `{}' not balanced */
#define REGEX_BADBR    (REGEX_ERRS+8) /* invalid repetition count(s) in `{}' */
#define REGEX_ERANGE   (REGEX_ERRS+9) /* invalid character rangin in `[]' */
#define REGEX_ESPACE   (REGEX_ERRS+10) /* ran out of memory */
#define REGEX_BADRPT   (REGEX_ERRS+11) /* `?', `*', or `+' operand invalid */
#define REGEX_UNKNOWN  (REGEX_ERRS+12) /* Unknown error */

#define MATRIX_ERRS    (P)0x00000D00L /* 0D00:0E00-1 for errors in matrix.h */
#define MATRIX_UNDEF_CUT        (MATRIX_ERRS+0)
#define MATRIX_ILLEGAL_CUT      (MATRIX_ERRS+1)
#define MATRIX_UNDER_CUT        (MATRIX_ERRS+2)
#define MATRIX_NONMATCH_CUT     (MATRIX_ERRS+3)
#define MATRIX_NONMATCH_SHAPE   (MATRIX_ERRS+4)
#define MATRIX_PARAM_ERROR      (MATRIX_ERRS+5)
#define MATRIX_SINGULAR         (MATRIX_ERRS+6)
#define MATRIX_INT_ERR          (MATRIX_ERRS+7)

#define PLUGIN_ERRS    (P)0x00000E00L /* 0E00L:0F00-1 for errors in plugin.h */
#define NO_PLUGINS (PLUGIN_ERRS+0)

#define NEXTEVENT_ERRS (P)0x00000F00L /* 0F00L:1000-1 for errors in dm-nexteven.h*/
#define NEXTEVENT_NOEVENT (NEXTEVENT_ERRS+0) /* no event available */

#define MPI_ERRS       (P)0x00001000L /* 1000L:1100-1 for errors in dm-mpi.h */
#define MPI_NOMSG (MPI_ERRS+0) /* no event available */

#define RTHREADS_ERRS  (P)0x00001100L /* 1100L:1100-1 for errror in dqueen.h */
#define RTHREADS_UNSET    (RTHREADS_ERRS+0)
#define RTHREADS_NUMTYPE  (RTHREADS_ERRS+1)
#define RTHREADS_NUMUNDF  (RTHREADS_ERRS+2)
#define RTHREADS_NUMRNG   (RTHREADS_ERRS+3)
#define RTHREADS_DICTTYPE (RTHREADS_ERRS+4)
#define RTHREADS_VALTYPE  (RTHREADS_ERRS+5)
#define RTHREADS_VALSIZE  (RTHREADS_ERRS+6)
#define RTHREADS_VALEMPTY (RTHREADS_ERRS+7)
#define RTHREADS_KEYSIZE  (RTHREADS_ERRS+8)
#define RTHREADS_VALATR   (RTHREADS_ERRS+9)

#define UNDF_LAST   (P)0x00001100L

#define NO_XWINDOWS 0x00000A01L /* X windows unavailable                 */
#define X_ERR       0x00000A02L /* X lib error                           */
#define X_BADFONT   0x00000A03L /* X font does not exist                 */
#define X_BADHOST   0x00000A04L /* X server cannot be connected          */
#define X_SEC_MISS  0x00000A05L /* X security extension missing          */
#define X_SEC_GEN   0x00000A06L /* X generate failure                    */
#define X_SEC_REV   0x00000A07L /* X revoke failure                      */
#define X_SEC_LIB   0x00000A08L /* X security lib missing                */
