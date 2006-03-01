AM_CFLAGS =  $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
#@LIBPLUGIN_DLL_IMPORT@
AM_CPPFLAGS = @INCLTDL@ -I"$(top_srcdir)/src" -I"$(top_builddir)/src"
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

if ENABLE_PROCESS
PROCESS_LIB=process.la
process_la_SOURCES = process.c process.h
endif ENABLE_PROCESS

if ENABLE_DMFLEX
DMFLEX_LIB=dmflex.la
dmflex_la_SOURCES = dmflex.l dmflex.h
endif ENABLE_DMFLEX

pkglib_LTLIBRARIES = test.la test2.la $(PROCESS_LIB) throw.la $(DMFLEX_LIB)

test_la_SOURCES = test.c test.h
test2_la_SOURCES = test2.c test2.h
throw_la_SOURCES = throw.c throw.h

