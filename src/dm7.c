

/*==================== D machine Rev. 3.0 (Linux): dm7.c ======================

     - time and file operators
          - gettime
          - localtime
          - getwdir
          - setwdir
          - readfile
          - writefile
          - writeboxfile
          - readboxfile
          - findfiles
          - tosystem
          - transcribe
*/

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <fnmatch.h>
#include <math.h>
#include "dm.h"

extern BOOLEAN timeout;
extern BOOLEAN abortflag;

/*--------------------------------------------------- gettime
   -- | time

 - returns compacted time as long numeral (seconds since something)
 - use 'localtime' to convert into date and time
*/

L op_gettime(void)
{
  if (o1 > CEILopds) return(OPDS_OVF);
  TAG(o1) = NUM | LONGTYPE; ATTR(o1) = 0;
  if (time((time_t*) &LONG_VAL(o1)) == -1) LONG_VAL(o1) = 0;
  FREEopds = o2;
  return(OK);
}

/*--------------------------------------------------- localtime
  time  <L of >=6 > | <L year month day hour min sec >

 - converts compacted 'time' into local time

NOTE: month[1...12], day[1...31], hour[0...23], min,sec[0...59]
*/

L op_localtime(void)
{
time_t dt;
struct tm *ldt;
L *p;

if (o_2 < FLOORopds) return(OPDS_UNF);
if (CLASS(o_2) != NUM) return(OPD_CLA);
if (!VALUE(o_2,(L*) &dt)) return(UNDF_VAL);
if (TAG(o_1) != (ARRAY | LONGTYPE)) return(OPD_ERR);
if (ATTR(o_1) & READONLY) return(OPD_ATR);
if (ARRAY_SIZE(o_1) < 6) return(RNG_CHK);
ldt = localtime(&dt);
p = (L *)VALUE_BASE(o_1);
p[0] = ldt->tm_year + 1900; p[1] = ldt->tm_mon + 1; p[2] = ldt->tm_mday;
p[3] = ldt->tm_hour; p[4] = ldt->tm_min; p[5] = ldt->tm_sec;
ARRAY_SIZE(o_1) = 6;
moveframe(o_1,o_2);
FREEopds = o_1;
return(OK);
}

/*---------------------------------------------------- gwdir
   string | substring

  - returns in 'substring' of 'string' the absolute filename of
    the current working directory
*/

L op_getwdir(void)
{
  B *p; L nb;
  if (o_1 < FLOORopds) return (OPDS_UNF);
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if (ATTR(o_1) & READONLY) return(OPD_ATR);
  if ((p = getcwd(0L,0L)) == 0L) return(-errno);
  nb = strlen(p);
  if (nb > ARRAY_SIZE(o_1)) return(RNG_CHK);
  moveB(p, (B *)VALUE_BASE(o_1),nb);
  free(p);
  ARRAY_SIZE(o_1) = nb;
  return(OK);
}

/*---------------------------------------------------- setwdir
     string | --

  - sets the current working directory to 'string'
*/

L op_setwdir(void)
{
  L nb;
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  nb = ARRAY_SIZE(o_1) + 1;
  if (nb > (CEILvm - FREEvm)) return(VM_OVF);
  moveB((B *)VALUE_BASE(o_1),FREEvm, nb-1);
  FREEvm[nb-1] = '\000';
  if (chdir(FREEvm)) return(-errno);
  FREEopds = o_1;
  return(OK);
}

/*---------------------------------------------------- tosystem
     string | --

  - executes 'string' as a shell command
*/

L op_tosystem(void)
{
  L nb, retc;
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  nb = ARRAY_SIZE(o_1) + 1;
  if (nb > (CEILvm - FREEvm)) return(VM_OVF);
  moveB((B *)VALUE_BASE(o_1),FREEvm, nb-1);
  FREEvm[nb-1] = '\000';
  retc = system(FREEvm);
  if (retc != 0) return(NOSYSTEM);
  FREEopds = o_1;
  return(OK);
}

/*---------------------------------------------------- readfile
   dir filename string | substring

  - dir is a string specifying the directory
  - filename is a string
  - the file contents are saved in substrings of 'string'
  - when the file contents exceed the capacity of 'string',
    a 'range check' error is reported
*/

L op_readfile(void)
{

int fd;
L nb, atmost, npath;
B *p;

if (o_3 < FLOORopds) return(OPDS_UNF);
if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (ATTR(o_1) & READONLY) return(OPD_ATR);
if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (TAG(o_3) != (ARRAY | BYTETYPE)) return(OPD_ERR);
npath = ARRAY_SIZE(o_3) + ARRAY_SIZE(o_2) + 1;
if (FREEvm + npath > CEILvm) return(VM_OVF);
moveB((B *)VALUE_BASE(o_3), FREEvm, ARRAY_SIZE(o_3));
moveB((B *)VALUE_BASE(o_2), FREEvm + ARRAY_SIZE(o_3), ARRAY_SIZE(o_2));
FREEvm[npath-1] = '\000';

alarm(30);
timeout = FALSE;
rf1:
if (timeout) return(TIMER); if (abortflag) return(ABORT);
  fd = open(FREEvm, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {if ((errno == EINTR) || (errno == EAGAIN))
      goto rf1; else return(-errno);}
  p = (B *)VALUE_BASE(o_1); atmost = ARRAY_SIZE(o_1);
rf2:
 if (timeout) return(TIMER); if (abortflag) return(ABORT);
 nb = read(fd, p, atmost);
 if (nb == -1) {if ((errno == EAGAIN) || (errno == EINTR)) goto rf2;
                else return(-errno);}
 if (nb == 0) goto rf3;
 p += nb; atmost -= nb;
 if (atmost == 0) return(RNG_CHK);
 goto rf2;
rf3:
 if (timeout) return(TIMER); if (abortflag) return(ABORT);
 if (close(fd) == -1) {if (errno == EINTR) goto rf3; else return(-errno);}
 
ARRAY_SIZE(o_1) = p - (B *)VALUE_BASE(o_1);
moveframe(o_1, o_3);
FREEopds = o_2;
return(OK);
}

/*---------------------------------------------------- writefile
   string dir filename | --

  - dir is a string specifying the directory
  - filename is a string
  - 'string' contains the byte array to be written
*/

L op_writefile(void)
{

int fd;
L nb, atmost, npath;
B *p;

if (o_3 < FLOORopds) return(OPDS_UNF);
if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (TAG(o_3) != (ARRAY | BYTETYPE)) return(OPD_ERR);
npath = ARRAY_SIZE(o_2) + ARRAY_SIZE(o_1) + 1;
if (FREEvm + npath > CEILvm) return(VM_OVF);
moveB((B *)VALUE_BASE(o_2), FREEvm, ARRAY_SIZE(o_2));
moveB((B *)VALUE_BASE(o_1), FREEvm + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
FREEvm[npath-1] = '\000';

alarm(30);
timeout = FALSE;
wf1:
if (timeout) return(TIMER); if (abortflag) return(ABORT);
  fd = open(FREEvm, O_CREAT | O_RDWR | O_TRUNC | O_NONBLOCK,
	    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
  if (fd == -1) {if ((errno == EINTR) || (errno == EAGAIN))
     goto wf1; else return(-errno);}
  p = (B *)VALUE_BASE(o_3); atmost = ARRAY_SIZE(o_3);
wf2:
 if (timeout) return(TIMER); if (abortflag) return(ABORT);
 nb = write(fd, p, atmost);
 if (nb == -1) {if ((errno == EAGAIN) || (errno == EINTR)) goto wf2;
   else return(-errno);}
  p += nb; atmost -= nb;
 if (atmost > 0) goto wf2;
wf3:
 if (timeout) return(TIMER); if (abortflag) return(ABORT);
 if (close(fd) == -1) {if (errno == EINTR) goto wf3; else return(-errno);}
FREEopds = o_3;
return(OK);
}

/******************************************* some macros for readboxfile
 * and writeboxfile
 * double time loops
 * they do declarations, returns and access local variables,
 * so they need to be macros rather than functions.
 */

#define DECLARE_ALARM \
  clock_t endclock = clock() + 180*CLOCKS_PER_SEC; \
  L chunk_size; UL ai = 0

#define START_ALARM {timeout = FALSE;}

#define MAX_CHUNK (32000)
//100mbit/s*1/8mbyte/mbit*1024byte/mbyte*5s*1/2minrate*/

#define CHECK_ALARM {\
  alarm(0); \
  if (clock() > endclock) {return(TIMER);}; \
  if (timeout) {return(TIMER);}; \
  chunk_size = MAX_CHUNK < atmost ? MAX_CHUNK : atmost; \
  alarm(10); \
  timeout = FALSE; ai++; \
  if (abortflag) return ABORT;}

#define END_ALARM { \
  alarm(0); \
  timeout = FALSE;}
      

/*---------------------------------------------------- readboxfile
   dir filename | root

  - reads the contents of the file specified by the strings 'dir' and
    'filename' into VM
  - unfolds the tree of objects in the box
  - pushes root object of the tree on operand stack
*/

L op_readboxfile(void)
{

int fd;
L nb, atmost, npath, retc;
B *p;
DECLARE_ALARM;

if (o_2 < FLOORopds) return(OPDS_UNF);
if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
npath = ARRAY_SIZE(o_1) + ARRAY_SIZE(o_2) + 1;
if (FREEvm + npath > CEILvm) return(VM_OVF);
moveB((B *)VALUE_BASE(o_2), FREEvm, ARRAY_SIZE(o_2));
moveB((B *)VALUE_BASE(o_1), FREEvm + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
FREEvm[npath-1] = '\000';
atmost = CEILvm - FREEvm;   
START_ALARM;
rb1:
  CHECK_ALARM;
  fd = open(FREEvm, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    if ((errno == EINTR) || (errno == EAGAIN)) goto rb1; 
    else {END_ALARM; return(-errno);};
  }
  p = FREEvm; 

rb2:
 CHECK_ALARM;
 nb = read(fd, p, chunk_size);
 if (nb == -1) {if ((errno == EAGAIN) || (errno == EINTR)) goto rb2;
   else {END_ALARM; return(-errno);};}
 if (nb == 0) goto rb3;
 p += nb; atmost -= nb;
 if (atmost == 0) {END_ALARM; return(VM_OVF);};
 goto rb2;
 
rb3:
 CHECK_ALARM;
 END_ALARM;
 if (close(fd) == -1) {if ((errno == EINTR) || (errno == EAGAIN)) goto rb3;
   else return(-errno);}
 
 nb = DALIGN(p - FREEvm);
 if (! GETNATIVEFORMAT(FREEvm) || ! GETNATIVEUNDEF(FREEvm)) return BAD_FMT;
 if (! GETNATIVEENDIAN(FREEvm) && (retc = deendian_frame(FREEvm) != OK))
     return retc;
 if ((retc = unfoldobj(FREEvm,(L)FREEvm, GETNATIVE(FREEvm))) != OK) 
   return(retc);
 FORMAT(FREEvm) = 0;
 moveframe(FREEvm,o_2);
 FREEvm += nb;
 FREEopds = o_1;
 return(OK);
}

/*---------------------------------------------------- writeboxfile
   root dir filename | --
  
  - the object tree originating from 'root' is folded into a box
  - a file is created to hold the box and the box is written into the file
    specified by the strings 'dir' and 'filename'
*/

L op_writeboxfile(void)
{

int fd;
L nb, atmost, retc, npath; W depth;
B *p, *oldFREEvm;
DECLARE_ALARM;

 if (o_3 < FLOORopds) return(OPDS_UNF);
 if (!((CLASS(o_3) == ARRAY) || (CLASS(o_3) == LIST) || (CLASS(o_3) == DICT)))
   return(OPD_ERR);
 if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
 if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);

 oldFREEvm = FREEvm; p = FREEvm; depth = 0;
 if ((retc = foldobj(o_3,(L)p,&depth)) != OK)
   { FREEvm = oldFREEvm; return(retc); }
 SETNATIVE(oldFREEvm);
 atmost = FREEvm - p; p = FREEvm; FREEvm = oldFREEvm;
 npath = ARRAY_SIZE(o_2) + ARRAY_SIZE(o_1) + 1;
 if (p + npath > CEILvm) return(VM_OVF);
 moveB((B *)VALUE_BASE(o_2), p, ARRAY_SIZE(o_2));
 moveB((B *)VALUE_BASE(o_1), p + ARRAY_SIZE(o_2), ARRAY_SIZE(o_1));
 p[npath-1] = '\000';

 START_ALARM;
wb1:
 CHECK_ALARM;
 fd = open(p, O_CREAT | O_NONBLOCK | O_RDWR | O_TRUNC,
	   S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);
 if (fd == -1) {if ((errno == EINTR) || (errno == EAGAIN))
     goto wb1; else {END_ALARM; return(-errno);};}
 p = oldFREEvm;
  
wb2:
 CHECK_ALARM;
 nb = write(fd, p, chunk_size);
 if (nb == -1) {if ((errno == EAGAIN) || (errno == EINTR)) goto wb2;
   else {END_ALARM; return(-errno);};}
 p += nb; atmost -= nb;
 if (atmost > 0) goto wb2;
 
wb3:
 CHECK_ALARM;
 END_ALARM;
 if (close(fd) == -1) {if (errno == EINTR) goto wb3; else return(-errno);}
 
 FREEopds = o_3;
 return(OK);
}

/*---------------------------------------------- findfiles
   dir | filelist 

 - the string 'dir' specifies the directory in which the search is
   to be performed
 - filelist is a list of file entries
 - each file entry is a list of the following objects:
    0 - filename (string)
    1 - file size, in bytes
    2 - (compact) datetime last modified
    3 - file attribute bits TTTT---ooogggaaa:
         TTTT = 1100  socket            *
                1010  symbolic link     *
                1000  regular file
                0110  block device      *
                0100  directory
                0010  character device  *
                0001  fifo              *
		* files with this attribute are not included into
                  filelist, but might be in the future
         ooo - owner privileges (read/write/execute)
         ggg - group privileges
         aaa - all privileges

 - directories are listed first in filelist, followed by regular files;
   directories and regular files are sorted alphabetically in each
   of the two subsets     
*/

/*-- compare filelist entries for sorting by dir/reg attribute and
    name
*/

static int compare(const void *a, const void *b)
{
  B *fpa, *fpb, *sa, *sb; L ta, tb, na, nb;

  fpa = (B *)(VALUE_BASE(a) + 3 * FRAMEBYTES);
  fpb = (B *)(VALUE_BASE(b) + 3 * FRAMEBYTES);
  VALUE(fpa,&ta); VALUE(fpb,&tb);
  fpa = (B *)(VALUE_BASE(a)); fpb = (B *)(VALUE_BASE(b));
  sa = (B *)(VALUE_BASE(fpa)); sb = (B *)(VALUE_BASE(fpb));
  na = ARRAY_SIZE(fpa); nb = ARRAY_SIZE(fpb);

  if (S_ISDIR(ta)) 
    {
      if (S_ISDIR(tb))
	  return(strncmp(sa,sb,(na <= nb)? na : nb));
      else return(-1);
    }
  else
    {
      if (S_ISDIR(tb)) return(1);
      else return(strncmp(sa,sb,(na <= nb)? na : nb));
    }
}

L op_findfiles(void)
{
  L ndirn, nentries, k, nname; 
  B *dirn, *vmp, *lp, *fnp, *fp;
  struct stat sb;
  struct dirent *ep;
  DIR *dp;

 if (o_1 < FLOORopds) return(OPDS_UNF);
 if (TAG(o_1) != (ARRAY | BYTETYPE)) return(OPD_ERR);
 if ((dirn = malloc(1024)) == 0) perror("Memory exhausted");

 /*-- we need dirname as null-terminated string */
 ndirn = ARRAY_SIZE(o_1);
 if (ndirn > 1023) { free(dirn); return(RNG_CHK); }
 moveB((B *)VALUE_BASE(o_1),dirn,ndirn); dirn[ndirn] = '\000';

 /*-- open the directory */
 if ((dp = opendir(dirn)) == NULL) { free(dirn); return(-errno); }
 vmp = FREEvm;

 /*-- scan file by file and save filenames and status info in VM
      skipping files other than regular files or directories
 */
 nentries = 0;
 while ((ep = readdir(dp)))
   {
   nname = strlen(ep->d_name);
   if ((ndirn + nname) > 1023) { closedir(dp); free(dirn); return(RNG_CHK); }
   moveB(ep->d_name, dirn+ndirn, nname); dirn[ndirn + nname] = '\000'; 
   if (stat(dirn, &sb) == 0)
     {
     if ((S_ISDIR(sb.st_mode) || (S_ISREG(sb.st_mode))))
       {
       if ((vmp + 6 * FRAMEBYTES + DALIGN(nname)) > CEILvm)
	 { closedir(dp); free(dirn); return(VM_OVF); }
       TAG(vmp) = LIST; ATTR(vmp) = 0;
       lp = (B*)(VALUE_BASE(vmp) = (L)(vmp + FRAMEBYTES));
       fnp = (B*)(LIST_CEIL(vmp) = (L)(lp + 4 * FRAMEBYTES));
       TAG(lp) = (ARRAY | BYTETYPE); ATTR(lp) = 0;
       VALUE_BASE(lp) = (L)(fnp + FRAMEBYTES);
       ARRAY_SIZE(lp) = nname;
       moveB(ep->d_name, (B *)VALUE_BASE(lp), ARRAY_SIZE(lp));
       moveframe(lp,fnp);
       lp += FRAMEBYTES;
       TAG(lp) = (NUM | LONGTYPE); ATTR(lp) = 0;
       LONG_VAL(lp) = sb.st_size;
       lp += FRAMEBYTES;
       TAG(lp) = (NUM | LONGTYPE); ATTR(lp) = 0;
       LONG_VAL(lp) = sb.st_mtime;
       lp += FRAMEBYTES;
       TAG(lp) = (NUM | LONGTYPE); ATTR(lp) = 0;
       LONG_VAL(lp) = sb.st_mode;
       vmp = fnp + FRAMEBYTES + DALIGN(nname);
       nentries++;
       }
     }
   }

 if (closedir(dp) != 0) return(-errno);
 /*-- build the 'filelist' master list */
 if ((vmp + (nentries + 1) * FRAMEBYTES) > CEILvm)
   { free(dirn); return(VM_OVF); }
 TAG(vmp) = LIST; ATTR(vmp) = READONLY;
 VALUE_BASE(vmp) = (L)(vmp + FRAMEBYTES);
 LIST_CEIL(vmp) = (L)(vmp + (nentries + 1) * FRAMEBYTES);
 lp = vmp + FRAMEBYTES; fp = FREEvm;
 for (k=0; k<nentries; k++)
   {
     moveframe(fp,lp);
     fp += 5 * FRAMEBYTES;
     fp += FRAMEBYTES + DALIGN(ARRAY_SIZE(fp));
     lp += FRAMEBYTES;
   }

 /*-- sort into directories and regular files */

 qsort((void *)VALUE_BASE(vmp), nentries, FRAMEBYTES, compare);
 
 FREEvm = lp;    
 moveframe(vmp, o_1);
 free(dirn);
 return(OK);
}

/*------------------------------------- transcribe
   original | replica

 - creates a replica of the given original object
 - applies itself recursively to objects nested in the original,
   thus replicating the object tree; the nes root object is returned
   as the replica
*/

L op_transcribe(void)
{
L retc; W depth;
B *p;

if (o_1 < FLOORopds) return(OPDS_UNF);
if (!((CLASS(o_1) == ARRAY) || (CLASS(o_1) == LIST) || (CLASS(o_1) == DICT)))
   return(OPD_ERR);
p = FREEvm; depth = 0;
if ((retc = foldobj(o_1,0L ,&depth)) != OK)
  { FREEvm = p; return(retc); }
moveframe(p,o_1);
return(OK);
}

