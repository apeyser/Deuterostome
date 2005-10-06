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

fd_set sock_fds;
BOOLEAN timeout;             /* for I/O operations          */
BOOLEAN abortflag;
BOOLEAN numovf;             /* FPU overflow status            */
BOOLEAN tinymemory;
L recsocket;
L consolesocket;
fd_set sock_fds;            /* active sockets                 */

const char* startup_dir; // setup by makefile in main
                         // defines which directory to use for
                         // the startup file
B* startup_dir_frame; // points the frame holding ^^^, at the bottom of the vm
                      // make server port accessible from outside of main
B* home_dir_frame; //points to the frame holding $HOME


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
                          3x type specifier (x=0-4 for b/w/l/s/d)
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

UW ascii[128] =
     {
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
     0x0102, 0x0800, 0x4830, 0x0800,  0x4834, 0x4810, 0x0800, 0x0800,
/*     H       I       J       K        L       M       N       O      */
     0x0800, 0x0800, 0x0800, 0x0800,  0x4832, 0x0800, 0x0800, 0x0800,
/*     P       Q       R       S        T       U       V       W      */
     0x0800, 0x0800, 0x0800, 0x4833,  0x0800, 0x0800, 0x0800, 0x4831,
/*     X       Y       Z       [        \       ]       ^       _      */
     0x0800, 0x0800, 0x0800, 0x0404,  0x1001, 0x0405, 0x0102, 0x0800,
/*     `       a       b       c        d       e       f       g      */
     0x0102, 0x0800, 0x4830, 0x0800,  0x4834, 0x4810, 0x0800, 0x0800,
/*     h       i       j       k        l       m       n       o      */
     0x0800, 0x0800, 0x0800, 0x0800,  0x4832, 0x0800, 0x0800, 0x0800,
/*     p       q       r       s        t       u       v       w      */
     0x0800, 0x0800, 0x0800, 0x4833,  0x0800, 0x0800, 0x0800, 0x4831,
/*     x       y       z       {        |       }       ~              */
     0x0800, 0x0800, 0x0800, 0x0402,  0x8001, 0x0403, 0x1102, 0x0000
     };

#if defined _WIN32 && defined DLL_EXPORT
char libDM_is_dll(void) {return 1;}
#endif
