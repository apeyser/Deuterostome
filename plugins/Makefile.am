AM_CPPFLAGS = -DSTARTUP_DIR="\"$(dmstartdir)\"" $(GCC_OPT)
AM_CFLAGS =  $(GCC_CPU) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
#@LIBPLUGIN_DLL_IMPORT@
INCLUDE = @INCLTDL@ 
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

pkglib_LTLIBRARIES = test.la test2.la process.la

test_la_SOURCES = test.c test.h
test2_la_SOURCES = test2.c test2.h
process_la_SOURCES = process.c process.h


