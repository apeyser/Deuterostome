AM_CFLAGS =  $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined
AM_LIBS = $(top_builddir)/src/libdmglobals.la $(X_LDFLAGS) 
AM_CPPFLAGS = $(INCLTDL) -I"$(top_srcdir)/src" -I"$(top_builddir)/src" \
	-I"$(top_builddir)/lib" -I"$(top_srcdir)/lib"
AM_LIBADD = $(AM_LIBS) $(LTLIBOBJS) @RELEASE_INFO@ @VERSION_INFO@ \
	$(top_builddir)/lib/libgnu.la
INCLUDES = $(INCLTDL)

INFILES = dmpetsc.c petsc.d new-plugin.d
EXTRA_DIST = dmpetsc.cin petsc.din
SUFFIXES =

if ENABLE_PLUGINS_SUPPORT

edit = 	sed \
	  -e 's,[@]PETSC_DIR[@],$(PETSC_DIR),g' \
	  -e 's,[@]PETSC_ARCH[@],$(PETSC_ARCH),g' \
	  -e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g'

BUILT_SOURCES = errors.d
nodist_noinst_DATA = errors.d

errors.d: $(builddir)/../dcode/errors.d
	rm -f $@
	$(LN_S) $< $@

BUILT_SOURCES += new-plugin.d

if ENABLE_PETSC
IPETSC_LIB=dmpetsc.la
IPETSC_D=petsc.d
BUILT_SOURCES += dm-dmpetsc-main.h dm-dmpetsc-header.h dmpetsc.c

dm-dmpetsc-main.h: new-plugin.d dmpetsc.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"dmpetsc.d" "$@" true true

dm-dmpetsc-header.h: new-plugin.d dmpetsc.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
	"dmpetsc.d" "$@" false true

dmpetsc_la_SOURCES = dmpetsc.h dmpetsc-bug.c
nodist_dmpetsc_la_SOURCES = dm-dmpetsc-main.h dm-dmpetsc-header.h dmpetsc.c
dmpetsc_la_LIBADD = $(AM_LIBADD) $(PETSC_LIB)
dmpetsc_la_CPPFLAGS = $(PETSC_CCPPFLAGS) $(AM_CPPFLAGS)
dist_noinst_DATA = dmpetsc.d

endif ENABLE_PETSC

if ENABLE_PLUGINS

if ENABLE_PROCESS
PROCESS_LIB=process.la process2.la
endif ENABLE_PROCESS

process2.c: process2.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"process2.d" "$@" true true

process2.h: process2.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"process2.d" "$@" false true

BUILT_SOURCES += process2.c process2.h

process_la_SOURCES = process.c process.h
process_la_LIBADD = $(AM_LIBADD)
nodist_process2_la_SOURCES = process2.c process2.h
dist_noinst_DATA = process2.d

dm-stats-main.h: stats.d new-plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"stats.d" "$@" true true

dm-stats-header.h: stats.d new-plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"stats.d" "$@" false true

BUILT_SOURCES += dm-stats-main.h dm-stats-header.h
stats_la_SOURCES = stats.c
nodist_stats_la_SOURCES = dm-stats-main.h dm-stats-header.h
stats_la_LIBADD = $(AM_LIBADD)
dist_noinst_DATA += stats.d

if ENABLE_CPPPLUGIN
CPPPLUGIN_LIB=cpptester.la
endif ENABLE_CPPPLUGIN

cpptester.c: cpptester.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"cpptester.d" "$@" true true


cpptester.h: cpptester.d plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
		"cpptester.d" "$@" false true

BUILT_SOURCES += cpptester.c cpptester.h
cpptester_la_SOURCES = cpptestercode.cpp cpptestercode.h
nodist_cpptester_la_SOURCES = cpptester.c cpptester.h
cpptester_la_LIBADD = $(AM_LIBADD)
dist_noinst_DATA += cpptester.d

if ENABLE_DMFLEX
DMFLEX_LIB = dmflex.la
dmflex_la_SOURCES = dmflex.h
nodist_dmflex_la_SOURCES = dmflex.c
BUILT_SOURCES += dmflex.c
dmflex_la_LIBADD = $(AM_LIBADD)
endif ENABLE_DMFLEX

else !ENABLE_PLUGINS

if !ENABLE_PETSC
.plugins: ; touch "$@"
BUILT_SOURCES += .plugins
endif !ENABLE_PETSC

endif !ENABLE_PLUGINS

if ENABLE_PLUGINS
PLUGINS = test.la test2.la $(PROCESS_LIB) throw.la \
	$(DMFLEX_LIB) $(IPETSC_LIB) \
	stats.la $(CPPPLUGIN_LIB)
else !ENABLE_PLUGINS
PLUGINS =  $(IPETSC_LIB)
if !ENABLE_PETSC
PLACEHOLDER = .plugins
endif !ENABLE_PETSC
endif !ENABLE_PLUGINS

if ENABLE_PLUGINS
test_la_SOURCES = test.c test.h
test_la_LIBADD = $(AM_LIBADD)
test2_la_SOURCES = test2.c test2.h
test2_la_LIBADD = $(AM_LIBADD)
throw_la_SOURCES = throw.c throw.h
throw_la_LIBADD = $(AM_LIBADD)
endif ENABLE_PLUGINS

pkglib_LTLIBRARIES = $(PLUGINS)
pkglib_DATA = $(PLACEHOLDER) 
nodist_dmstart_DATA = $(IPETSC_D)
BUILT_SOURCES += $(IPETSC_D)

CLEANFILES = $(BUILT_SOURCES) $(PETSC_D)

endif ENABLE_PLUGINS_SUPPORT

include $(top_srcdir)/m4/cf_in.make

#################################################
# Keep automake from trying to save the .c file
#
.l.c:
	$(am__skiplex) $(SHELL) $(YLWRAP) \
		$< $(LEX_OUTPUT_ROOT).c $@ -- $(LEXCOMPILE)
