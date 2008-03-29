#include "dm.h"

B *FLOORopds;
B *FREEopds;
B* CEILopds;
B* FLOORdicts;
B* FREEdicts;
B* CEILdicts;
B* FLOORexecs;
B* FREEexecs;
B* CEILexecs;
B* FLOORvm;
B* FREEvm;
B* CEILvm;
B* TOPvm;
B* errsource;

B locked = FALSE;
B serialized = FALSE;

fd_set sock_fds;
BOOLEAN timeout;             /* for I/O operations          */
BOOLEAN abortflag;
BOOLEAN numovf;             /* FPU overflow status            */
BOOLEAN tinymemory;
P recsocket;
P consolesocket;
fd_set sock_fds;            /* active sockets                 */

const char* startup_dir; // setup by makefile in main
                         // defines which directory to use for
                         // the startup file
B* startup_dir_frame; // points the frame holding ^^^, at the bottom of the vm
                      // make server port accessible from outside of main
B* home_dir_frame; //points to the frame holding $HOME
B* plugin_dir_frame; //points to the frame holding the plugindir
B* conf_dir_frame; //points to the frame holding confdir


/*---------------------------- ASCII character classification table

Each 7-bit ASCII character is assigned a 16-bit classification key
that assigns it possible roles:

High byte                 Low byte
----------                --------
White space         80    00 no attention
                          01 start of comment (|)
                          02 end of comment (newline)
Numeral             40    00 no attention (digit or sign)
                          10 float characteristic (. or e/E)
                          20 'undefined' specifier (*)
                          3x type specifier (x=0-5 for b/w/l=32/x=64/s/d)
			     x is the offset into TYPEBYTES in dmnum.h
                          40 octal digit
Slash               10    00 /
                          01 \
                          02 ~
Name                08       letters, digits, underline
Special             04    00 <
                          01 >
                          02 {
                          03 }
                          04 [
                          05 ]
String              01    00 (
                          01 )
                    01    02 possible only in string, no attention
Garbage             00    00 none of the above                         */

UW ascii[128] = {
/*                                                                     */
     0x0000, 0x0000, 0x0000, 0x0000,  0x0000, 0x0000, 0x0000, 0x0000,
/*             TAB     LF                       CR                     */
     0x0000, 0x8000, 0x8002, 0x0000,  0x0000, 0x8002, 0x0000, 0x0000,
/*                                                                     */
     0x0000, 0x0000, 0x0000, 0x0000,  0x0000, 0x0000, 0x0000, 0x0000,
/*                                                                     */
     0x0000, 0x0000, 0x0000, 0x0000,  0x0000, 0x0000, 0x0000, 0x0000,
/*    space    !       "       #        $       %       &       '      */
     0x8000, 0x0102, 0x0102, 0x0102,  0x0102, 0x0102, 0x0102, 0x0102,
/*     (       )       *       +        ,       -       .       /      */
     0x0100, 0x0101, 0x4020, 0x4000,  0x0102, 0x4000, 0x4010, 0x1000,
/*     0       1       2       3        4       5       6       7      */
     0x4840, 0x4840, 0x4840, 0x4840,  0x4840, 0x4840, 0x4840, 0x4840,
/*     8       9       :       ;        <       =       >       ?      */
     0x4800, 0x4800, 0x0102, 0x0102,  0x0400, 0x0102, 0x0401, 0x0102,
/*     @       A       B       C        D       E       F       G      */
     0x0102, 0x0800, 0x4830, 0x0800,  0x4835, 0x4810, 0x0800, 0x0800,
/*     H       I       J       K        L       M       N       O      */
     0x0800, 0x0800, 0x0800, 0x0800,  0x4832, 0x0800, 0x0800, 0x0800,
/*     P       Q       R       S        T       U       V       W      */
     0x0800, 0x0800, 0x0800, 0x4834,  0x0800, 0x0800, 0x0800, 0x4831,
/*     X       Y       Z       [        \       ]       ^       _      */
     0x4833, 0x0800, 0x0800, 0x0404,  0x1001, 0x0405, 0x0102, 0x0800,
/*     `       a       b       c        d       e       f       g      */
     0x0102, 0x0800, 0x4830, 0x0800,  0x4835, 0x4810, 0x0800, 0x0800,
/*     h       i       j       k        l       m       n       o      */
     0x0800, 0x0800, 0x0800, 0x0800,  0x4832, 0x0800, 0x0800, 0x0800,
/*     p       q       r       s        t       u       v       w      */
     0x0800, 0x0800, 0x0800, 0x4834,  0x0800, 0x0800, 0x0800, 0x4831,
/*     x       y       z       {        |       }       ~              */
     0x4833, 0x0800, 0x0800, 0x0402,  0x8001, 0x0403, 0x1102, 0x0000
};

#if defined _WIN32 && defined DLL_EXPORT
char libDM_is_dll(void) {return 1;}
#endif

#if ! DM_X_DISPLAY_MISSING
#include "xhack.h"
jmp_buf xhack_buf;
char xhack_jmpd = 0;
#endif
