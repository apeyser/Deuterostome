/*===================== D machine Rev3.0: part 1 ========================
     - tokenizer

*/

#include "dm.h"
#include <math.h>

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
/*                     LF                       CR                     */
     0x0000, 0x0000, 0x8002, 0x0000,  0x0000, 0x8002, 0x0000, 0x0000,
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

/* NOTE: the '~' enjoys some fortuitous encoding */

/*--------------- temporary VM space management shared by scanner and
 tokenizer; plus other shared variables  */

static B *vm_token;   /* ->freespace                                 */
static L vm_free;     /* # of free bytes available                   */
static L vm_bytes;    /* length of returned token string including 0 */
static B *sframe;     /* ->source string frame                       */

/*--------------- GETC, UNGETC for feeding string object to scanner */

static B GETC(void)
{
if (ARRAY_SIZE(sframe) <= 0L) return(0);
ARRAY_SIZE(sframe)--; 
return((*(B *)((VALUE_BASE(sframe))++)) & 0x7F);
}

static void UNGETC(void)
{
ARRAY_SIZE(sframe)++; VALUE_BASE(sframe)--;
}

/*----------------------------- scanner ---------------------------------

searches the source string for the next token. Returns a classification
code and places a null-terminated string representing the body of the
token into VM temporary space: 

   - skip leading white characters and comment
   - classify first non-white character
   - on numeral (leading digit, sign, or *):
      - type = default to integer
      - accept numeral characters with attention to:
         - decimal point and/or e/E: type = default to float
         - *: numeral to be undefined
         - b/B, w/W, l/L,  f/F, s/S, d/D: type = as given
      - termination: non-numeral character (unget)
                     type specifier (consume)
   - on name:
      - leading slash or tilde: mark as passive or tilded, next character
        must be nametype
      - accept name characters
      - regular termination: white space or special character (unget)
   - on string:
      - accept all subsequent legal characters up to closing bracket,
        interpreting control sequences
   - classify other special character, take appropriate return
        - < necessarily followed by type specifier
        - note that tilde attribute can accompany [

The return value specifies in its lower byte one of:
     0 - end of stream (no token)
     1 - VM overflow
     2 - token format error
     3 - non-D character
     4 - numeral (type in upper byte)
     5 - passive or tilded name (distinguished in upper byte)
     6 - active name
     7 - string
     8 - <X - start of array (numeral type X in upper byte)
     9 - >  - end of array
    10 - {  - start of procedure
    11 - }  - end of procedure
    12 - [  - mark (normal or tilded, specified in upper byte)
    13 - ]  - endmark

In the upper (numeral type) byte, the upper nipple bits encode for :
     1 - to be undefined
     2 - string has float characteristics
     4 - final type is specified in lower nipple
In the upper (name type) byte, 
     0 - /name
     1 - ~name
In the upper ([ type) byte,
     0 - [
     1 - ~[

The lower nipple of a numeral type specifies one of:
     0 - byte integer
     1 - word integer
     2 - long integer
     3 - single float
     4 - double float
*/

static W scan(void)
{
W kk,type,num;  UW k;
B c;

vm_bytes = 0;
do {                                              /* white space      */
                      
   if (0 == (c = GETC())) return(0);
   k = ascii[c];
   if ((k & 0x8001) == 0x8001)                    /* comment          */
     {
     do {
        if (0 == (c = GETC())) return(0);
        k = ascii[c];
        } while ((k & 0x8002) != 0x8002);         /* til newline      */
     }
   }  while (k & 0x8000);
if ((k & 0x401E) == 0x4000)                       /* numeral          */
     {
     type = 0; 
     do {
        switch(k & 0x30)
          {
          case 0x00: break;
          case 0x10: type |= 0x2000; break;
          case 0x20: type |= 0x1000; break;
          case 0x30: type = (type & 0x30FF) | ((k<<8) & 0xF00) | 0x4000;
                     goto num_1;
          }
        if (vm_free-- == 0) return(1);
        *(vm_token++) = c; vm_bytes++;
        if (0 == (c = GETC())) goto num_1;
        k = ascii[c];
        if (k == 0) return(3);
        } while (k & 0x4000);
     UNGETC();
num_1:
     if (vm_free-- == 0) return(1);
     *(vm_token++) = '\000'; vm_bytes++;
     return(type | 0x4);
     }
else if ((k & 0x1003) == 0x1000)                 /* /name            */
     {
     type = 0x05;
     if (0 == (c = GETC())) return(2);
     k = ascii[c];
     if (k == 0) return(3);
     if (((k & 0x0800) == 0) || ((k & 0x403F) == 4000)) return(2);
     goto name_1; 
     }
else if ((k & 0x1003) == 0x1002)                 /* ~                */
     {
     type = 0x0105;
     if (0 == (c = GETC())) return(2);
     k = ascii[c];
     if (k == 0) return(3);
     if ((k & 0x0404) == 0x0404) return(0x010C); /* ~[               */
     if (((k & 0x0800) == 0) || ((k & 0x403F) == 4000)) return(2);
     goto name_1;                                /* ~name            */
     }
else if (k & 0x0800)                             /* name             */
     {
     type = 0x06;
name_1:
     do {
        if (vm_free-- == 0) return(1);
        *(vm_token++) = c; vm_bytes++;
        if (0 == (c = GETC())) goto name_2;;
        k = ascii[c];
        if (k == 0) return(3);
        } while (k & 0x0800);
     UNGETC();
name_2:
     if (vm_free-- == 0) return(1);
     *(vm_token++) = '\000'; vm_bytes++;
     return(type);
     }
else if (k & 0x0400)                           /* special            */
     {
     kk = (k & 0x7);
     if (kk == 0)           /* < needs by followed by type specifier */
          {
          if (0 == (c = GETC())) return(2);
          if (((k = ascii[c]) & 0x4030) != 0x4030) return(2);
          kk = (k<<8) & 0x0F00;
          }
     return(kk + 8);
     }
else if ((k & 0x0103)  == 0x0100)               /* string  */
     {
     if ((vm_free -= FRAMEBYTES) < 0) return(1);
     vm_token += FRAMEBYTES; vm_bytes += FRAMEBYTES;
string_1:
     if (0 == (c = GETC())) return(2);
     k = ascii[c];
     if (k == 0) return(3);
     if (k == 0x0101) goto string_2;
     if ((k & 0x1001) == 0x1001)              /* control sequence   */
        {
        if (0 == (c = GETC())) return(2);
        k = ascii[c];
        if (k == 0) return(3);
        if ((k & 0x4040) == 0x4040)
          {
          num = c - '0';
          if (0 == (c = GETC())) return(2);
          k = ascii[c];
          if (k == 0) return(3);
          if ((k & 0x4040) == 0x4040)
               {
               num = 8 * num + c - '0';
               if (0 == (c = GETC())) return(2);
               k = ascii[c];
               if (k == 0) return(3);
               if ((k & 0x4040) == 0x4040)
                  num = 8 * num + c - '0';
                  else  UNGETC();
               } else UNGETC();
          c = num; 
          }
        else switch(c)
          {
          case 'n':  c = '\n'; break;
          case 'r':  c = '\r'; break;
          case '(':  c = '('; break;
          case ')':  c = ')'; break;
          case '\\': c = '\\'; break;
          case '\n': goto string_1;
          default: if (vm_free-- == 0) return(1);
                   *(vm_token++) = '\\'; vm_bytes++;
                   break;
          }
        }      /* of control sequence */
     if (vm_free-- == 0) return(1);
     *(vm_token++) = c; vm_bytes++;
     goto string_1;
string_2:
     if (vm_free-- == 0) return(1);
     *(vm_token++) = '\000'; vm_bytes++;
     return(7);
     }
else return(3);                               /* garbage            */     
}  /* of scanner */

/*--------------------------- Tokenizer ---------------------------------

     L tokenize(string_frame)

Receives a string frame representing the source. The source string length
is defined by a terminating zero byte or the size of the string object.
Source characters are stripped to 7 bits before use.

The returned value indicates a standard return condition.

A frame representing the scanned token is pushed onto the operand stack,
and the body of a composite object is appended to the virtual memory.

Procedure bodies are intermediately stored on the operand stack
while being assembled (cave stack overflow!). This does not apply
to strings or arrays, which are assembled in VM.

   - save old operand stack top level
   - establish level 0 (no procedure in progress)
   - re-iterate scanner from here
   - set up temporary space for token in VM (leaving space for master
     frame)
   - make sure there is space for one frame on operand stack
   - scan for next token
   - on numeral:
      - build numeral frame on operand stack
      - send frame out for conversion
      - level 0: return, else re-iterate
   - on name:
      - create name frame on operand stack
      - insert passive or tilde attribute
      - level 0: return, else re-iterate
   - on string:
      - create byte array frame on operand stack, finalize VM
      - level 0: return, else iterate
   - on <:
      - set array type, make numeral frame on operand stack
      - for all numeral tokens scanned up to >:
          - get next token
          - send frame out for conversion and push numeral onto VM
      - replace numeral frame by array frame on operand stack,
        finalize VM
      - level 0: return, else re-iterate
   - on {:
      - push a preliminary procedure frame onto the operand stack
      - increment to next procedure level
      - re-iterate
   - on }:
      - search back for latest preliminary procedure frame on operand
        stack
      - transfer the procedure body to VM, finalize VM
      - complete the procedure frame on the operand stack
      - drop to the next lower procedure level
      - level 0: return, else re-iterate
   - on [:
      - push mark object onto operand stack, include TILDE attribute if
        it is specified
        level 0: return, else re-iterate
   - on ]:
      - push name frame with all packed name bytes zero
        level 0: return, else re-iterate 
*/

L tokenize(B *stringframe)
{

L nframes, nb, arrnum;
W level, t;
B *frame, *vm_stoken,  *oldfree;

sframe = stringframe; ATTR(sframe) &= (~PARENT);
level = 0; oldfree = FREEopds;
iterate:
vm_free = CEILvm - FREEvm;
vm_stoken = vm_token = FREEvm;
if (FREEopds >= CEILopds) { FREEopds = oldfree; return(OPDS_OVF); }
frame = FREEopds;
switch((t = scan()) & 0xFF)
     {
     case 0:   if (level == 0)   return(DONE); 
                  else { FREEopds = oldfree; return(PRO_CLO); }   
     case 1:   FREEopds = oldfree; return(VM_OVF); 
     case 2:   FREEopds = oldfree; return(BAD_TOK);
     case 3:   FREEopds = oldfree; return(BAD_ASC);
     case 4:   goto numeral;
     case 5:   goto passname;
     case 6:   goto actname;
     case 7:   goto string;
     case 8:   goto array;
     case 9:   FREEopds = oldfree; return(ARR_CLO);
     case 10:  goto procon;
     case 11:  goto procoff;
     case 12:  goto mark;
     case 13:  goto endmark;
     }

/*------------------------------ mark */
mark:
TAG(frame) = MARK; ATTR(frame) = (t & 0x0100)? TILDE : 0;
FREEopds += FRAMEBYTES;
goto next;

/*------------------------------ endmark */
endmark:
makename("]",frame);
ATTR(frame) = ACTIVE;
FREEopds += FRAMEBYTES;
goto next;

/*------------------------------ numeral */
numeral:
t = ENCODE((t>>8) & 0xFF,vm_stoken,NUM_VAL(frame));
TAG(frame) = NUM | t; ATTR(frame) = 0;
FREEopds += FRAMEBYTES;
goto next;

/*------------------------------- name */
passname:
makename(vm_stoken,frame);
FREEopds += FRAMEBYTES;
if (t & 0x0100) ATTR(frame) = TILDE;
goto next;

actname:
makename(vm_stoken,frame);
FREEopds += FRAMEBYTES;
ATTR(frame) = ACTIVE;
goto next;

/*---------------------------- string -> byte array
 NB: string value starts above freespace for master frame 
*/

string:
TAG(frame) = ARRAY | BYTETYPE; ATTR(frame) = PARENT;
VALUE_BASE(frame) = (L)(vm_stoken + FRAMEBYTES);
vm_bytes--;                              /* strip terminating null */
ARRAY_SIZE(frame) = vm_bytes - FRAMEBYTES;
moveframes(FREEopds,FREEvm,1L);      /* master frame */
ARRAY_SIZE(FREEvm) = (vm_bytes = (L)(DALIGN(vm_bytes))) - FRAMEBYTES;
if ((FREEvm + vm_bytes) >= CEILvm) { FREEopds = oldfree; return(VM_OVF); }
FREEvm += vm_bytes;
FREEopds += FRAMEBYTES;
goto next;

/*---------------------------- <> array */
array:
TAG(frame) = ARRAY | ((t>>8) & 0x0F); ATTR(frame) = PARENT;
arrnum = VALUE_BASE(frame) = (L)(FREEvm + FRAMEBYTES); ARRAY_SIZE(frame) = 0;
nb = VALUEBYTES(TYPE(frame));

arrnext:
switch((t = scan()) & 0xFF)
     {
     case 1:   FREEopds = oldfree; return(VM_OVF);   /* VM overflow     */
     case 2:   FREEopds = oldfree; return(BAD_TOK);  /* scrambled token */
     case 3:   FREEopds = oldfree; return(BAD_ASC);  /* garbage stream  */
     case 4:   goto arrapp;
     case 9:   goto arrend;
     case 0:   FREEopds = oldfree; return(ARR_CLO);  /* incomplete      */
     default:  FREEopds = oldfree; return(CLA_ARR);  /* not in array!   */
     }

arrapp:
t = 0x40 | TYPE(frame) | ((t>>8) & 0x30);
ENCODE(t,vm_stoken,(B *)arrnum);
if ((arrnum + nb) >= (L)CEILvm) { FREEopds = oldfree; return(VM_OVF); }

ARRAY_SIZE(frame) += 1;
arrnum += nb;
vm_free = (L)CEILvm - arrnum;
vm_stoken = vm_token = (B *)arrnum;
goto arrnext;

arrend:
moveframes(FREEopds,FREEvm,1L);              /* master frame */
vm_bytes = (L)(DALIGN(nb * ARRAY_SIZE(FREEopds))) + FRAMEBYTES; 
if ((FREEvm + vm_bytes) >= CEILvm) { FREEopds = oldfree; return(VM_OVF); }
ARRAY_SIZE(FREEvm) = (vm_bytes - FRAMEBYTES) / nb;
FREEvm += vm_bytes;                          /* update VM */
FREEopds += FRAMEBYTES;
goto next;

/*---------------------------------------- { - start of procedure */
procon:
TAG(frame) = PROC; ATTR(frame) = ACTIVE | PARENT;
LIST_CEIL(frame) = -1L;        /* mark as preliminary proc object */
FREEopds += FRAMEBYTES;
level++;
goto iterate;

/*----------------------------------------- } - end of procedure */
procoff:
if (level == 0) return(PRO_CLO);
nframes = 0; frame = FREEopds;
do { frame -= FRAMEBYTES; if (frame < oldfree) return(PRO_CLO);
     nframes++;
   } while (!((TAG(frame) == PROC) && (LIST_CEIL(frame) == (-1L))));
nb = FREEopds - frame - FRAMEBYTES;
if ((FREEvm + nb + FRAMEBYTES) >= CEILvm)
    { FREEopds = oldfree; return(VM_OVF); }
moveframes(frame+FRAMEBYTES, FREEvm + FRAMEBYTES, nframes-1);
VALUE_BASE(frame) = (L)(FREEvm + FRAMEBYTES);
LIST_CEIL(frame) = (L)(VALUE_BASE(frame) + nb);
moveframe(frame,FREEvm);         /* master frame */
FREEvm += nb + FRAMEBYTES;
FREEopds = frame + FRAMEBYTES;
level--;

/*---------------------------------- next iteration or exit */
next:
if (level == 0) return(OK);
goto iterate;

} /* of tokenizer */

/*--------------------------------------------startupdir
 *  --- | (directory)
 *  returns the hardcoded path (hidden at bottom of vm)
 *  to the startup directory for the node
 */
L op_startupdir(void)
{    
    extern B* startup_dir_frame;
    
    if (CEILopds < o2) return OPDS_OVF;
    moveframe(startup_dir_frame, o1);
    FREEopds = o2;
    return OK;
}

