AM_CFLAGS =  $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
AM_CPPFLAGS = @INCLTDL@ -I"$(top_srcdir)/src" -I"$(top_builddir)/src"
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la
BUILT_SOURCES =

if ENABLE_PLUGINS_SUPPORT

if ENABLE_PLUGINS

if ENABLE_PROCESS
PROCESS_LIB=process.la process2.la
endif ENABLE_PROCESS

process2.c: process2.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "process2.d" "$@" true true

process2.h: process2.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "process2.d" "$@" false true

BUILT_SOURCES += process2.c process2.h

process_la_SOURCES = process.c process.h
process_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
process2_la_SOURCES = process2.c process2.h
dist_noinst_DATA = process2.d

stats.c: stats.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "stats.d" "$@" true true

stats.h: stats.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "stats.d" "$@" false true

BUILT_SOURCES += stats.c stats.h
stats_la_SOURCES = stats.h stats.c
stats_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
dist_noinst_DATA += stats.d

if ENABLE_CPPPLUGIN
CPPPLUGIN_LIB=cpptester.la
endif ENABLE_CPPPLUGIN

cpptester.c: cpptester.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "cpptester.d" "$@" true true


cpptester.h: cpptester.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "cpptester.d" "$@" false true

BUILT_SOURCES += cpptester.c cpptester.h
cpptester_la_SOURCES = cpptester.c cpptester.h \
	cpptestercode.cpp cpptestercode.h
cpptester_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
dist_noinst_DATA += cpptester.d

if ENABLE_RTHREADS
RTHREADS_LIB=rthreads.la
endif ENABLE_RTHREADS

dm-rthreads-main.h: new-plugin.d rthreads.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "rthreads.d" "$@" true true

dm-rthreads-header.h: new-plugin.d rthreads.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "rthreads.d" "$@" false true

BUILT_SOURCES += dm-rthreads-main.h dm-rthreads-header.h
rthreads_la_SOURCES = dm-rthreads-main.h dm-rthreads-header.h rthreads.h rthreads.h
rthreads_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
dist_noinst_DATA += rthreads.d

if ENABLE_DMFLEX
DMFLEX_LIB=dmflex.la
endif ENABLE_DMFLEX
BUILT_SOURCES += dmflex.c
dmflex_la_SOURCES = dmflex.l dmflex.h
dmflex_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@

else !ENABLE_PLUGINS
.plugins:
	touch "$@"

BUILT_SOURCES += .plugins
endif !ENABLE_PLUGINS

if ENABLE_PLUGINS
PLUGINS = test.la test2.la $(PROCESS_LIB) throw.la \
	$(DMFLEX_LIB) $(RTHREADS_LIB) \
	stats.la $(CPPPLUGIN_LIB)
else !ENABLE_PLUGINS
PLACEHOLDER = .plugins
endif !ENABLE_PLUGINS

if ENABLE_PLUGINS
test_la_SOURCES = test.c test.h
test_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
test2_la_SOURCES = test2.c test2.h
test2_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
throw_la_SOURCES = throw.c throw.h
throw_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@
endif ENABLE_PLUGINS

pkglib_LTLIBRARIES = $(PLUGINS)
pkglib_DATA = $(PLACEHOLDER)

CLEANFILES = $(BUILT_SOURCES)

endif ENABLE_PLUGINS_SUPPORT
