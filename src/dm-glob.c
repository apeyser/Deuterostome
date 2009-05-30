#include "dm.h"

#include <glob.h> // after, to get all glob features
#include "dm-glob.h"
#include <string.h>
#include <errno.h>

// (pattern) | [ (path)...]
P op_glob(void) {
  glob_t gl;
  size_t i;
  B* curr = FREEvm;
  B* top;
  P retc = OK;

  if (o_1 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY|BYTETYPE)) return OPD_CLA;
  if (curr + ARRAY_SIZE(o_1) + 1 >= CEILvm) 
    return VM_OVF;
  moveB(VALUE_PTR(o_1), curr, ARRAY_SIZE(o_1));
  curr += ARRAY_SIZE(o_1);
  curr[0] = '\0';
  
  curr = FREEvm;
  switch (glob((char*) curr, GLOB_BRACE|GLOB_TILDE, NULL, &gl)) {
    case 0: break;

    case GLOB_NOMATCH:
      if (curr + FRAMEBYTES >= CEILvm) return VM_OVF;
      TAG(curr) = LIST;
      ATTR(curr) = PARENT;
      VALUE_PTR(curr) = LIST_CEIL_PTR(curr) = curr + FRAMEBYTES;
      curr += FRAMEBYTES;
      moveframe(FREEvm, o_1);
      FREEvm = curr;
      return OK;

    case GLOB_NOSPACE: return MEM_OVF;
    case GLOB_ABORTED: return errno ? -errno : READ_ERROR;
    default:           return UNKNOWN_ERR;
  };

  if (curr + FRAMEBYTES + FRAMEBYTES * gl.gl_pathc >= CEILvm)
    goto vm_ovf;

  TAG(curr) = LIST;
  ATTR(curr) = PARENT;
  VALUE_PTR(curr) = curr + FRAMEBYTES;
  top = LIST_CEIL_PTR(curr) = curr + FRAMEBYTES + FRAMEBYTES * gl.gl_pathc;
  curr += FRAMEBYTES;

  for (i = 0; i < gl.gl_pathc; ++i) {
    size_t len = strlen(gl.gl_pathv[i]);
    if (top + FRAMEBYTES + DALIGN(len) >= CEILvm) goto vm_ovf;

    TAG(top) = (ARRAY|BYTETYPE);
    ATTR(top) = PARENT;
    VALUE_PTR(top) = top + FRAMEBYTES;
    ARRAY_SIZE(top) = len;
    moveframe(top, curr);

    curr += FRAMEBYTES;
    top += FRAMEBYTES;
    moveB((B*) gl.gl_pathv[i], top, len);
    top += DALIGN(len);
  }

  moveframe(FREEvm, o_1);
  FREEvm = top;
  goto exit;

 vm_ovf:
  retc = VM_OVF;

 exit:
  globfree(&gl);
  return retc;
}
