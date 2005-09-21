AM_CPPFLAGS = -DSTARTUP_DIR="\"$(dmstartdir)\"" $(GCC_OPT)
AM_CFLAGS =  $(GCC_CPU) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
#@LIBPLUGIN_DLL_IMPORT@
INCLUDE = @INCLTDL@ 

pkglib_LTLIBRARIES = test.la test2.la

test_la_SOURCES = test.c test.h
test_la_LIBADD = $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

test2_la_SOURCES = test2.c test2.h
test2_la_LIBADD = $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

