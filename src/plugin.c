#include "dm.h"

#include <stdarg.h>
#include <string.h>

#include "pluginlib.h"
#include "dm2.h"
#include "dm6.h"
#include "dm8.h"
#include "error-local.h"

#if ! ENABLE_PLUGINS_SUPPORT

P op_nextlib(void) {return NO_PLUGINS;}
P op_loadlib(void) {return NO_PLUGINS;}
void closealllibs(void) {}
void initialize_plugins(void) {}
P _check_plugins(B* floor, B* top) {return OK;}

#else //ENABLE_PLUGINS_SUPPORT

#include <ltdl.h>
#include <stdio.h>
#include <stdlib.h>

B opaquename[FRAMEBYTES];
B saveboxname[FRAMEBYTES];
B buffernameframe[FRAMEBYTES];
B fininame[FRAMEBYTES];
B initname[FRAMEBYTES];
B activename[FRAMEBYTES];
static B nullframe[FRAMEBYTES];
static B activeframe[FRAMEBYTES];

void initialize_plugins(void) {
  if (lt_dlinit()) {
    const char* e = lt_dlerror();
    fprintf(stderr, "dlinit: %s\n", e ? e: "--");
    error_local(EXIT_FAILURE, 0, "Can't dlinit");
  }

  makename((B*)"OPAQUENAME", opaquename);
  makename((B*)"SAVEBOX", saveboxname);
  makename((B*)"BUFFER", buffernameframe);
  makename((B*)"INIT_", initname);
  makename((B*)"FINI_", fininame);
  makename((B*)"ACTIVE", activename);
  TAG(nullframe) = NULLOBJ; 
  ATTR(nullframe) = READONLY;
  TAG(activeframe) = BOOL;
  ATTR(activeframe) = READONLY;
  BOOL_VAL(activeframe) = FALSE;
}

/*------------------------------------------------closealllibs
 * walks through the libs at over the vm ceil and closes
 * the associated shared library handle
 */
void closealllibs(void)
{
  const char * e;
  void* handle;  
  B* lib = NULL;
  B* frame;
  
  while((lib = nextlib(lib)) !=  NULL)
    if ((handle = (void*) LIB_HANDLE(lib)) != NULL) {
      if ((frame = lookup(fininame, VALUE_PTR(lib))))
        OP_CODE(frame)();

      if (lt_dlclose((lt_dlhandle)handle)) {
        e = lt_dlerror();
        fprintf(stderr, "dlclose: %s\n", e ? e : "--");
      }
    }
  
  if (lt_dlexit()) {
    e = lt_dlerror();
    fprintf(stderr, "dlexit: %s\n", e ? e : "--");
  }
}

/***************************************************nextlib
 * dict or null | nextdict true
 *              | false
 * takes a dictionary (of a lib or sysdict) that lives
 * over the ceiling and walks up the tree searching for the next
 * one.  False if no further dicts, and null finds the first
 * one over ceiling.  Dicts are ordered in reverse of loading,
 * i.e., sysdict is last, null returns last lib loaded.
 * */
P op_nextlib(void)
{
  B* lastlib;
    
  if (o_1 < FLOORopds) return OPDS_UNF;
    
  switch (TAG(o_1)) {
    case (DICT | OPLIBTYPE):
      lastlib = VALUE_PTR(o_1) - FRAMEBYTES;
      if (! LIB_TYPE(lastlib)) {
        TAG(o_1) = BOOL;
        ATTR(o_1) = 0;
        BOOL_VAL(o_1) = FALSE;
	return OK;
      }
      break;
            
    case NULLOBJ: lastlib = NULL; break;
    default: return OPD_TYP;
  }

  if (CEILopds < o2) return OPDS_OVF;
  if (! (lastlib = nextlib(lastlib))) return CORR_OBJ;
  if (! LIB_TYPE(lastlib)) {
    TAG(o_1) = BOOL;
    ATTR(o_1) = 0;
    BOOL_VAL(o_1) = FALSE;
    return OK;
  }

  moveframe(lastlib, o_1);

  TAG(o1) = BOOL;
  ATTR(o1) = 0;
  BOOL_VAL(o1) = TRUE;
  FREEopds = o2;
  
  return OK;
}

    
    
/******************************************************loadlib
 * The library loading mechanism
 * (dir) (file) | library-dict
 * loads the shared library, via its ll_export variable
 * and creates an opdict containing all exported ops
 * placed above the vm ceiling
 * a library can only be loaded once between vmresize's
 * or an error will be signalled
 * */

#define LIB_IMPORT(var, type, name)				\
  if (! (var = (type) lt_dlsym((lt_dlhandle)handle, #name))) {	\
    const char* e = lt_dlerror();                               \
    if (e) fprintf(stderr, "%s\n", e);					\
    fprintf(stderr, "Symbol not found: %s in %s\n",			\
	    #name, FREEvm);						\
    return LIB_EXPORT;							\
  }

// (dir) (file) | lib
P op_loadlib(void)
{
  UP type;
  void *handle;
  B** ops;
  P* errc;
  B** errm;
  UP* libtype;
  B* oldCEILvm;
  B* oldFREEvm;
  B* sysdict;
  B* eOPDS;
  
  B* frame;
  B* dict;
  P retc;
  
  oldCEILvm = CEILvm;
  oldFREEvm = FREEvm;
  
  if (o_2 < FLOORopds) return OPDS_UNF;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_ERR;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_ERR;
  
  if (FREEvm + ARRAY_SIZE(o_1) + ARRAY_SIZE(o_2) + 2 > CEILvm)
    return VM_OVF;
  
  strncpy((char*)FREEvm, (char*)VALUE_PTR(o_2), ARRAY_SIZE(o_2));
  FREEvm += ARRAY_SIZE(o_2);
  if (FREEvm[-1] != '/') FREEvm++[0] = '/';
  strncpy((char*)FREEvm, (char*)VALUE_PTR(o_1), ARRAY_SIZE(o_1));
  FREEvm[ARRAY_SIZE(o_1)] = '\0';
  FREEvm = oldFREEvm;
  
  if (! (handle = (void*) lt_dlopen((char*)FREEvm))) {
    const char* e;                         
    fprintf(stderr, "%s\n", (e = lt_dlerror()) ? e : "??");
    return LIB_LOAD;
  }

  // loop over super ceil region, looking for first dict for type
  // and looking for sysop to stop
  // check handles on all libs but sysdict
  // assumed that sysdict has already been placed
  type = 0;
  frame = NULL;
  while (1) {
    if (! (frame = nextlib(frame))) {
      lt_dlclose((lt_dlhandle) handle);
      return CORR_OBJ;
    }
    if (! type) type = LIB_TYPE(frame) + 1;
    if (! LIB_TYPE(frame)) break;
    if ((P) handle == LIB_HANDLE(frame)) {
      lt_dlclose((lt_dlhandle) handle);
      moveframe(frame, o_2);
      FREEopds = o_1;
      return OK;
    }
  }
    
  LIB_IMPORT(ops, B**, ll_export);
  LIB_IMPORT(errc, P*, ll_errc);
  LIB_IMPORT(errm, B**, ll_errm);
  LIB_IMPORT(libtype, UP*, ll_type);
  
  *libtype = type << 16;
  if ((dict = makeopdict((B*) ops, errc, errm)) == (B*) -1L) {
    lt_dlclose((lt_dlhandle)handle);
    FREEvm = oldFREEvm;
    CEILvm = oldCEILvm;
    return VM_OVF;
  }

  LIB_TYPE(dict - FRAMEBYTES) = type;
  LIB_HANDLE(dict - FRAMEBYTES) = (P) handle;
  moveframe(dict - FRAMEBYTES, o_2);
  FREEopds = o_1;
  eOPDS = o_2;

  if ((frame = lookup(initname, dict))
      && (retc = OP_CODE(frame)()) != OK) {
    lt_dlclose((lt_dlhandle) handle);
    FREEvm = oldFREEvm;
    CEILvm = oldCEILvm;
    if (FREEopds > eOPDS) FREEopds = eOPDS;
    return LIB_INIT;
  }  
  
  sysdict = VALUE_PTR(FLOORdicts);
  if (! mergedict(dict, sysdict)) return LIB_MERGE;
  
  return OK;
}


/* extensions for opaque frames, declared in plugin.h */
BOOLEAN check_opaque_name(B* nameframe, B* dict) {
  B* frame = lookup(opaquename, dict);
  if (! frame || TAG(frame) != NAME) return FALSE;
  return matchname(frame, nameframe);
}

B* make_opaque_frame(P n, B* pluginnameframe, ...) {
  int len = 0;
  B* nameframe;
  va_list nameframes;
  B* dict;
  B* oldFREEvm = FREEvm;
  B* buffer = NULL;

  va_start(nameframes, pluginnameframe);
  while ((nameframe = va_arg(nameframes, B*))) ++len;
  va_end(nameframes);
  
  if (op_save() != OK) return NULL;

  if ((dict = makedict(len + 3 + (n ? 1 : 0))) == (B*) -1L) {
    FREEvm = oldFREEvm;
    FREEopds = o_1;
    return NULL;
  }
  
  if (n) {
    n = DALIGN(n);
    if (FREEvm + FRAMEBYTES + n > CEILvm) {
      FREEvm = oldFREEvm;
      FREEopds = o_1;
      return NULL;
    }

    buffer = FREEvm;
    TAG(FREEvm) = (ARRAY | BYTETYPE); 
    ATTR(FREEvm) = READONLY;
    VALUE_PTR(FREEvm) = FREEvm + FRAMEBYTES;
    ARRAY_SIZE(FREEvm) = n;
    FREEvm += FRAMEBYTES + n;
  }
  
  ATTR(o_1) |= READONLY;
  insert(saveboxname, dict, o_1);
  ATTR(pluginnameframe) |= READONLY;
  insert(opaquename, dict, pluginnameframe);
  insert(activename, dict, activeframe);
  if (buffer) insert(buffernameframe, dict, buffer);

  va_start(nameframes, pluginnameframe);
  while ((nameframe = va_arg(nameframes, B*)))
    insert(nameframe, dict, nullframe);
  va_end(nameframes);
  
  if (op_capsave() != OK) {
    FREEvm = oldFREEvm;
    FREEopds = o_1;
    return NULL;
  }
  
  ATTR(dict-FRAMEBYTES) |= READONLY;
  TAG(dict-FRAMEBYTES) |= OPAQUETYPE;
  return dict-FRAMEBYTES;
}

P wrap_readcode(const char* file) {
  P ret;
  if ((ret = op_getplugindir()) != OK) return ret;
  if (x2 > CEILexecs) return EXECS_OVF;
  if (o2 > CEILopds) return OPDS_OVF;
  
  TAG(x1) = (ARRAY | BYTETYPE); 
  ATTR(x1) = (ACTIVE | READONLY);
  ARRAY_SIZE(x1) =  strlen((char*)(VALUE_PTR(x1) = (B*)"fromfiles"));
  FREEexecs = x2;

  TAG(o1) = (ARRAY | BYTETYPE); ATTR(o1) = READONLY;
  ARRAY_SIZE(o1) = strlen((char*)(VALUE_PTR(o1) = (B*) file));
  FREEexecs = o2;
  return OK;
}

P _check_plugins(B* floor, B* top) {
  P retc;

  if (o2 > CEILopds) return OPDS_OVF;
  moveframe(floor, o1);
  FREEopds = o2;

 again:
  if (TAG(o_1) == (DICT|OPAQUETYPE)
      && BOOL_VAL(lookup(activename, VALUE_PTR(o_1))))
      return ACTIVE_OPAQUE;

  if ((retc = op_nextobject())) return retc;
  FREEopds = o_1;
  if (BOOL_VAL(o1)) {
    if (VALUE_PTR(o_1) - FRAMEBYTES < top) goto again;
    FREEopds = o_1;
  }

  return OK;
}      

#endif //ENABLE_PLUGINS_SUPPORT
