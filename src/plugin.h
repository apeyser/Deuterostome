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

#define EXPORTNAME(name) PLUGIN_JOIN(PLUGIN_NAME, _LTX_##name)

#define PRIVATEPLUGIN PLUGIN_JOIN(_, PLUGIN_NAME)

//#define PRIVATENAME(name) PLUGIN_JOIN(_, PLUGIN_JOIN(PLUGIN_NAME,_##name))
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



#endif //PLUGIN_H
