#include <stdarg.h>
#define PLUGIN_ONLY_LIB
#include "plugin.h"

/* extensions for opaque frames, declared in plugin.h */
BOOLEAN check_opaque_name(B* nameframe, B* dict) {
  B* frame = lookup(opaquename, dict);
  if (! frame || TAG(frame) != NAME) return FALSE;
  return matchname(frame, nameframe);
} 

B* make_opaque_frame(B* pluginnameframe, ...) {
  int len = 0;
  B* nameframe;
  va_list nameframes;
  B* dict;
  B nullframe[FRAMEBYTES];
  TAG(nullframe) = NULLOBJ; ATTR(nullframe) = 0;

  va_start(nameframes, pluginnameframe);
  while (nameframe = va_arg(nameframes, B*)) ++len;
  va_end(nameframes);
  
  if (op_save() != OK) return NULL;
  if ((dict = makedict(len+2)) == (B*) -1L) {
	FREEvm = VALUE_PTR(o_1)-FRAMEBYTES;
	FREEopds = o_1;
	return NULL;
  }
  
  insert(saveboxname, dict, o_1);
  insert(opaquename, dict, pluginnameframe);
  va_start(nameframes, pluginnameframe);
  while (nameframe = va_arg(nameframes, B*))
	insert(nameframe, dict, nullframe);
  va_end(nameframes);

  if (op_capsave() != OK) {
	FREEvm = VALUE_PTR(o_1)-FRAMEBYTES;
	FREEopds = o_1;
	return NULL;
  }

  ATTR(dict-FRAMEBYTES) |= READONLY;
  TAG(dict-FRAMEBYTES) |= OPAQUETYPE;
  return dict-FRAMEBYTES;
}

