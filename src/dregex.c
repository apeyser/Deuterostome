#include <sys/types.h>
#include <regex.h>
#include <stdlib.h>

#include "dm.h"
#include "dregex.h"

#define REGEX_ERR(err) case REG_##err: return REGEX_##err

DM_INLINE_STATIC P int_regex_error(int e) 
{
  switch (e) {
    REGEX_ERR(BADPAT);
    REGEX_ERR(ECOLLATE);
    REGEX_ERR(ECTYPE);
    REGEX_ERR(EESCAPE);
    REGEX_ERR(ESUBREG);
    REGEX_ERR(EBRACK);
    REGEX_ERR(EPAREN);
    REGEX_ERR(EBRACE);
    REGEX_ERR(BADBR);
    REGEX_ERR(ERANGE);
    REGEX_ERR(ESPACE);
    REGEX_ERR(BADRPT);
  default:
    return REGEX_UNKNOWN;
  };
}

DM_INLINE_STATIC P int_regex(BOOLEAN case_sensitive) 
{
  char* string;
  regex_t preg;
  regmatch_t* pmatch;
  int r;
  unsigned int i;
  B* lframe;
  P retc = OK;
	
  if (o_2 < FLOORopds) return OPDS_UNF;
  if ((TAG(o_2) != (ARRAY | BYTETYPE)) ||
      (TAG(o_1) != (ARRAY | BYTETYPE)))
    return OPD_ERR;
  if (FREEvm + ARRAY_SIZE(o_1)+2 > CEILvm) return VM_OVF;
  if (CEILopds < o4) return OPDS_OVF;
  
  moveB(VALUE_PTR(o_1), FREEvm, ARRAY_SIZE(o_1));
  FREEvm[ARRAY_SIZE(o_1)] = '\0';
  
  if ((r = regcomp(&preg, (char*)FREEvm,
                   REG_EXTENDED|(case_sensitive ? 0 : REG_ICASE))))
    return int_regex_error(r);

  pmatch = (regmatch_t*) (FREEvm+FRAMEBYTES*(preg.re_nsub+1));
  string = ((char*) pmatch)+sizeof(regmatch_t)*(preg.re_nsub+1);
  if ((B*) string + ARRAY_SIZE(o_2)+2 > CEILvm) {
    retc = VM_OVF;
    goto EXIT;
  }

  moveB(VALUE_PTR(o_2), (B*)string, ARRAY_SIZE(o_2));
  string[ARRAY_SIZE(o_2)] = '\0';
  switch (r = regexec(&preg, string, preg.re_nsub+1, pmatch, 0)) {
    case 0:
      TAG(FREEvm) = LIST;
      ATTR(FREEvm) = PARENT;
      VALUE_PTR(FREEvm) = FREEvm+FRAMEBYTES;
      LIST_CEIL_PTR(FREEvm) = FREEvm+FRAMEBYTES*(preg.re_nsub+1);
      for (i = 1, lframe = FREEvm+FRAMEBYTES;
           i < preg.re_nsub+1;
           ++i, lframe += FRAMEBYTES) {
        if (pmatch[i].rm_so == -1) {
          TAG(lframe) = NULLOBJ;
          ATTR(lframe) = 0;
        }
        else {
          TAG(lframe) = ARRAY | BYTETYPE;
          VALUE_PTR(lframe) = VALUE_PTR(o_2) + pmatch[i].rm_so;
          ARRAY_SIZE(lframe) = pmatch[i].rm_eo - pmatch[i].rm_so;
          ATTR(lframe) = ATTR(o_2) & READONLY;
        }
      }
      
      TAG(o3) = BOOL;
      ATTR(o3) = 0;
      BOOL_VAL(o3) = TRUE;
      
      moveframe(FREEvm, o2);
      ATTR(o2) = 0;
			
      TAG(o1) = ARRAY | BYTETYPE;
      VALUE_PTR(o1) = VALUE_PTR(o_2);
      ARRAY_SIZE(o1) = pmatch[0].rm_so;
      ATTR(o1) = ATTR(o_2) & READONLY;
			
      TAG(o_1) = ARRAY | BYTETYPE;
      VALUE_PTR(o_1) = VALUE_PTR(o_2) + pmatch[0].rm_so;
      ARRAY_SIZE(o_1) = pmatch[0].rm_eo - pmatch[0].rm_so;
      ATTR(o_1) = ATTR(o_2) & READONLY;
			
      VALUE_PTR(o_2) += pmatch[0].rm_eo;
      ARRAY_SIZE(o_2) -= pmatch[0].rm_eo;
      ATTR(o_2) &= READONLY;
			
      FREEvm = (B*) pmatch;
      FREEopds = o4;
      break;
						
    case REG_NOMATCH:
      TAG(o_1) = BOOL;
      ATTR(o_1) = 0;
      BOOL_VAL(o_1) = FALSE;
      break;
						
    default:
      retc = int_regex_error(r);
      break;
  }

 EXIT:
  regfree(&preg);
  return retc;
}

/*------------------------------ op_regex
 * (string) (pattern) |
 * if found: post match pre [submatches] true
 * else: (string) false
 */

P op_regex(void) 
{
  return int_regex(TRUE);
}

/*------------------------------ op_regexi
 * (string) (pattern) |
 * if found: post match pre [submatches] true
 * else: (string) false
 * Done case-insensitively
 */

P op_regexi(void) 
{
  return int_regex(FALSE);
}
