AM_CFLAGS =  $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
AM_CPPFLAGS = @INCLTDL@ -I"$(top_srcdir)/src" -I"$(top_builddir)/src"
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

BUILT_SOURCES = new-plugin.d
new-plugin.d: $(srcdir)/Makefile new-plugin.d.in
	rm -f $@ $@.tmp
	$(edit) new-plugin.d.in >$@.tmp
	mv $@.tmp $@

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

dm-stats-main.h: stats.d new-plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "stats.d" "$@" true true

dm-stats-header.h: stats.d new-plugin.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "stats.d" "$@" false true

BUILT_SOURCES += dm-stats-main.h dm-stats-header.h
stats_la_SOURCES = dm-stats-main.h dm-stats-header.h stats.c
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

if ENABLE_PETSC
IPETSC_LIB=dmpetsc.la
IPETSC_D=petsc.d
endif

dm-dmpetsc-main.h: new-plugin.d dmpetsc.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dmpetsc.d" "$@" true true

dm-dmpetsc-header.h: new-plugin.d dmpetsc.d
	"$(top_srcdir)/src/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dmpetsc.d" "$@" false true

BUILT_SOURCES += dm-dmpetsc-main.h dm-dmpetsc-header.h dmpetsc.c
dmpetsc_la_SOURCES = dm-dmpetsc-main.h dm-dmpetsc-header.h dmpetsc.c dmpetsc.h
dmpetsc_la_LIBADD = @RELEASE_INFO@ @VERSION_INFO@ 
dmpetsc_la_LDFLAGS=$(AM_LDFLAGS) $(PETSC_LIB)
dmpetsc_la_CPPFLAGS = $(PETSC_CCPPFLAGS) $(AM_CPPFLAGS)
dist_noinst_DATA += dmpetsc.d dmpetsc.c.in

edit = 	sed \
	  -e 's,[@]PETSC_DIR[@],$(PETSC_DIR),g' \
	  -e 's,[@]PETSC_ARCH[@],$(PETSC_ARCH),g' \
	  -e 's,[@]dpawn[@],$(dpawn),g' \
	  -e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g'

dmpetsc.c: Makefile $(srcdir)/dmpetsc.c.in
	-rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dmpetsc.c.in" > $@.tmp
	mv $@.tmp $@

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
	$(DMFLEX_LIB) $(IPETSC_LIB) \
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
pkglib_DATA = $(PLACEHOLDER) $(IPETSC_D)

CLEANFILES = $(BUILT_SOURCES)

endif ENABLE_PLUGINS_SUPPORT
