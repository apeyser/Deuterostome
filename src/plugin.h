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
char libPLUGIN_is_dll(void) {return 1;}
#endif

#include "dm.h"

L wrap_readcode(const char* file);

#define EXPORTNAME(name) PLUGIN_JOIN(PLUGIN_NAME, _LTX_##name)

#define PRIVATEPLUGIN PLUGIN_JOIN(_, PLUGIN_NAME)
#define PRIVATENAME(name) PLUGIN_JOIN(PRIVATEPLUGIN,_##name)

#define PLUGIN_JOIN(name1,name2) PREPLUGIN_JOIN(name1,name2)
#define PREPLUGIN_JOIN(name1,name2) name1##name2

PLUGIN_SCOPE UL ll_type;
PLUGIN_SCOPE L ll_errc[];
PLUGIN_SCOPE B* ll_errm[];
PLUGIN_SCOPE B* ll_export[];

#define RETURN_ERROR(err) return (ll_type | (err))

#define ll_type   EXPORTNAME(ll_type)
#define ll_errc   EXPORTNAME(ll_errc)
#define ll_errm   EXPORTNAME(ll_errm)
#define ll_export EXPORTNAME(ll_export)
#define op_hi     EXPORTNAME(op_hi)
#define op_libnum EXPORTNAME(op_libnum)
#define op_INIT_  EXPORTNAME(op_INIT_)
#define op_FINI_  EXPORTNAME(op_FINI_)

L op_hi(void);
L op_libnum(void);
L op_INIT_(void);
L op_FINI_(void);

#define opaquename PRIVATENAME(opaquename)
extern B opaquename[FRAMEBYTES];
extern B buffernameframe[FRAMEBYTES];

#define TEST_OPAQUE(frame)	do {										\
	if (TAG(frame) != (DICT | OPAQUETYPE)) return OPD_TYP;				\
	if (! check_opaque_name(opaquename, VALUE_PTR(frame))) return ILL_OPAQUE; \
  } while (0)

#define OPAQUE_MEM(frame, nameframe) (lookup(nameframe, VALUE_PTR(frame)))
#define OPAQUE_MEM_SET(frame, nameframe, newframe) do {	\
	ATTR(newframe) |= READONLY;							\
	moveframe(newframe, OPAQUE_MEM(frame, nameframe));	\
  } while (0)
	
  
#define MAKE_OPAQUE_DICT(n, ...) \
  (make_opaque_frame(n, opaquename, __VA_ARGS__, NULL))

// frame must be removed from the stack before call
#define KILL_OPAQUE(frame) do {					   \
	L ret;										   \
	if (o1 >= CEILopds) return OPDS_OVF;		   \
	moveframe(OPAQUE_MEM(frame, saveboxname), o1); \
	FREEopds = o2;								   \
	if ((ret = op_restore()) != OK) return ret;	   \
  } while (0)

#define PLUGIN_INTRO(version) PLUGIN_INTRO_(version, PLUGIN_NAME)
#define PLUGIN_INTRO_(version, name)                 \
  UL ll_type = 0; \
  L op_hi(void) {return wrap_hi(#name " V" #version);} \
  L op_libnum(void) {return wrap_libnum(ll_type);}

#define PLUGIN_OP(name) #name, (B*) op_##name

#define PLUGIN_OPS PLUGIN_OP(hi), PLUGIN_OP(libnum)

#define PLUGIN_DEC_NAME(name) \
  static B name##_frame[FRAMEBYTES]; \
  static B* name##_string = #name

#define PLUGIN_DEF_NAME(name) \
  makename(name##_string, name##_frame)

#include "pluginlib.h"

#endif //PLUGIN_H
