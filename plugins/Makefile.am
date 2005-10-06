AM_CPPFLAGS = -DSTARTUP_DIR="\"$(dmstartdir)\"" $(GCC_OPT)
AM_CFLAGS =  $(GCC_CPU) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
#@LIBPLUGIN_DLL_IMPORT@
INCLUDE = @INCLTDL@ 
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

if ENABLE_process
PROCESS_LIB=process.la
process_la_SOURCES = process.c process.h
endif ENABLE_process

pkglib_LTLIBRARIES = test.la test2.la $(PROCESS_LIB) throw.la

test_la_SOURCES = test.c test.h
test2_la_SOURCES = test2.c test2.h
throw_la_SOURCES = throw.c throw.h

