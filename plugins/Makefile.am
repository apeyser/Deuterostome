AM_CFLAGS =  $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
#@LIBPLUGIN_DLL_IMPORT@
AM_CPPFLAGS = @INCLTDL@ -I"$(top_srcdir)/src" -I"$(top_builddir)/src"
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

EXTRA_DIST = plugin.m4

if ENABLE_PROCESS

PROCESS_LIB=process.la process2.la
endif ENABLE_PROCESS
BUILT_SOURCES = process2.c process2.h
process_la_SOURCES = process.c process.h
process2_la_SOURCES = process2.plugin

include $(top_srcdir)/m4/Makefile.plugin

# .plugin.c: 
# 	cd $(top_builddir) \
# 	  && $(SHELL) ./config.status $(subdir)/$@ \
# 		$(am__depfiles_maybe)

# .plugin.h: 
# 	cd $(top_builddir) \
# 	  && $(SHELL) ./config.status $(subdir)/$@ \
# 		$(am__depfiles_maybe)



if ENABLE_DMFLEX
DMFLEX_LIB=dmflex.la
endif ENABLE_DMFLEX
BUILT_SOURCES += dmflex.c
dmflex_la_SOURCES = dmflex.l dmflex.h

pkglib_LTLIBRARIES = test.la test2.la $(PROCESS_LIB) throw.la $(DMFLEX_LIB) 

test_la_SOURCES = test.c test.h
test2_la_SOURCES = test2.c test2.h
throw_la_SOURCES = throw.c throw.h

CLEANFILES = $(BUILT_SOURCES)

