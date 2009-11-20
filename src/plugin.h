#ifndef PLUGIN_H
#define PLUGIN_H

#ifndef PLUGIN_NAME
#error "PLUGIN_NAME must be defined before it is included, " \
  "to the plugins' base name"
#endif

#ifdef _WIN32
#ifdef DLL_EXPORT
#define PLUGIN_SCOPE __declspec(dllexport)
#else //! DLL_EXPORT
#ifdef LIBPLUGIN_DLL_IMPORT
#define PLUGIN_SCOPE extern __declspec(dllimport)
#endif //LIBPLUGIN_DLL_EXPORT
#endif //DLL_EXPORT
#endif //_WIN32

#ifndef PLUGIN_SCOPE
#define PLUGIN_SCOPE extern
#endif

#if defined _WIN32 && defined DLL_EXPORT
#if __cplusplus
extern "C" {
#endif
char libPLUGIN_is_dll(void) {return 1;}
#if __cplusplus
}
#endif
#endif

#include "dm.h"
#include "dm6.h"

#if __cplusplus
extern "C" {
#endif

P wrap_readcode(const char* file);

#define EXPORTNAME(name) PLUGIN_JOIN(PLUGIN_NAME, _LTX_##name)

#define PRIVATEPLUGIN PLUGIN_JOIN(_, PLUGIN_NAME)
#define PRIVATENAME(name) PLUGIN_JOIN(PRIVATEPLUGIN,_##name)

#define PLUGIN_JOIN(name1,name2) PREPLUGIN_JOIN(name1,name2)
#define PREPLUGIN_JOIN(name1,name2) name1##name2

#define RETURN_ERROR(err) return (ll_type | (err))

#define ll_type   EXPORTNAME(ll_type)
#define ll_errc   EXPORTNAME(ll_errc)
#define ll_errm   EXPORTNAME(ll_errm)
#define ll_export EXPORTNAME(ll_export)
#define op_hi     EXPORTNAME(op_hi)
#define op_libnum EXPORTNAME(op_libnum)
#define op_INIT_  EXPORTNAME(op_INIT_)
#define op_FINI_  EXPORTNAME(op_FINI_)

PLUGIN_SCOPE UP ll_type;
PLUGIN_SCOPE P ll_errc[];
PLUGIN_SCOPE B* ll_errm[];
PLUGIN_SCOPE B* ll_export[];

P op_hi(void);
P op_libnum(void);
P op_INIT_(void);
P op_FINI_(void);

#define opaquename PRIVATENAME(opaquename)
extern B opaquename[FRAMEBYTES];
extern B buffernameframe[FRAMEBYTES];

#define TEST_OPAQUE(frame, nameframe)	do {				\
    if (TAG(frame) != (DICT | OPAQUETYPE)) return OPD_TYP;              \
    if (! check_opaque_name(nameframe, VALUE_PTR(frame)))		\
      return ILL_OPAQUE;						\
  } while (0)

#define OPAQUE_MEM(frame, nameframe) (lookup(nameframe, VALUE_PTR(frame)))

#define OPAQUE_MEM_SET(frame, nameframe, newframe)	\
  moveframe(newframe, OPAQUE_MEM(frame, nameframe))
  
#define OPAQUE_BUFFER_GET(frame)  (OPAQUE_MEM(frame, buffernameframe))
#define OPAQUE_BUFFER_SIZE(frame) (ARRAY_SIZE(OPAQUE_BUFFER_GET(frame)))
#define OPAQUE_BUFFER_PTR(frame)  (VALUE_PTR(OPAQUE_BUFFER_GET(frame)))

#define OPAQUE_WIRED(frame) (BOOL_VAL(OPAQUE_MEM(frame, wiredname)))

#define MAKE_OPAQUE_DICT(n, ...)					\
  (make_opaque_frame(n, opaquename, __VA_ARGS__, NULL))

// dict should be o_1
#define KILL_OPAQUE() do {						\
    P ret;								\
    OPAQUE_WIRED(o_1) = FALSE;						\
    moveframe(OPAQUE_MEM(o_1, saveboxname), o_1);			\
    if ((ret = op_restore()) != OK) return ret;				\
  } while (0)

#define PLUGIN_INTRO(version, name)                 \
  UP ll_type = 0; \
  P op_hi(void) {return wrap_hi(#name " V" #version);} \
  P op_libnum(void) {return wrap_libnum(ll_type);}

#define PLUGIN_OP(name) (B*) #name, (B*) op_##name

#define PLUGIN_OPS PLUGIN_OP(hi), PLUGIN_OP(libnum)

#define PLUGIN_DEC_NAME(name) \
  static B name##_frame[FRAMEBYTES]; \
  static B* name##_string = #name

#define PLUGIN_DEF_NAME(name) \
  makename(name##_string, name##_frame)

#if __cplusplus
}
#endif		
		
#include "pluginlib.h"

#endif //PLUGIN_H
