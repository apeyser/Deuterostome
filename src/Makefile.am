SUBDIRS = codegen

edit = sed -e 's,[@]bindir[@],$(bindir),g' -e 's,[@]srcdir[@],$(srcdir),g'

if ENABLE_SETUID

nodist_dnode_nice_SOURCES = dnode-nice.c
DNODENICEBIN = dnode-nice

install-exec-local:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	$(INSTALL_PROGRAM_ENV) $(LIBTOOL) --mode=install \
		$(INSTALL) -m 4755 "dnode-nice$(EXEEXT)" \
			"$(DESTDIR)$(bindir)/dnode-nice$(EXEEXT)" 

uninstall-local:
	-rm -f "$(DESTDIR)$(bindir)/dnode-nice$(EXEEXT)"

endif ENABLE_SETUID

dnode-nice.c: Makefile $(srcdir)/dnode-nice.c.in
	rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dnode-nice.c.in" > $@.tmp
	mv $@.tmp $@

if ENABLE_DAEMON

nodist_dnode_daemon_SOURCES = dnode-daemon.c
DNODEDAEMONBIN = dnode-daemon

endif ENABLE_DAEMON

dnode-daemon.c: Makefile $(srcdir)/dnode-daemon.c.in
	rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dnode-daemon.c.in" > $@.tmp
	mv $@.tmp $@



if ENABLE_ATLAS
ATLAS_ACTIVATE = -DATLAS_LIB
endif ENABLE_ATLAS

if ENABLE_THREADS
  THREAD_HDR=threads.h
  THREADSRC=threads.c
endif ENABLE_THREADS

if ENABLE_REGEX
  REGEX_SRC=regex.c
endif ENABLE_REGEX

EXTRA_DIST = dnode-nice.c.in dnode-daemon.c.in paths.h.in

DISTCLEANFILES = dm-config.h

AM_CPPFLAGS = -DDM_HAVE_CONFIG_H
AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_CXXFLAGS = $(GCC_CXXFLAGS)
AM_LDFLAGS = -no-undefined $(PTHREAD_LIBS) $(X_LDFLAGS)
INCLUDE = @INCLTDL@

bin_PROGRAMS = dvt dnode dgen $(DNODEDAEMONBIN)
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS_SUPPORT
if ENABLE_CPPPLUGIN
CPPPLUGIN_HDR = cppplugin.h
endif ENABLE_CPPPLUGIN
PLUGIN_HDR = plugin.h pluginlib.h $(CPPPLUGIN_HDR)
PLUGIN_LD = @LIBLTDL@
endif ENABLE_PLUGINS_SUPPORT

pkginclude_HEADERS = dm.h $(PLUGIN_HDR) dmx.h $(THREAD_HDR) \
	error-local.h dm-snprintf.h srandomdev-local.h dmnum.h \
	basic-defs.h dm-types.h
nodist_pkginclude_HEADERS = dm-config.h paths.h

dvt_SOURCES = dvt.c dvt_0.h dvt_1.h
dvt_LDADD = libdmglobals.la
dvt_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS)
dvt_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_FLAGS)
dvt_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

dnode_SOURCES = dnode.c dnode_0.h dnode_1.h dmx.h
dnode_LDADD = libdmglobals.la 
dnode_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS)
dnode_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@ 
dnode_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS)

testwordorder_SOURCES = test-word-order.c

globsrcs = dmglobals.c \
	dmnuminc.h dmnum.c dsp1f.h dsp1.c dsp2.c \
	paths.h dm1.c dm2.c dm3.c dm4.c dm5.c dm6.c dm7.c dm8.c dm9.c \
	pluginlib.h plugin.c \
	dm-types.c xhack.h

BUILT_SOURCES = paths.h dnode-nice.c dnode-daemon.c
CLEANFILES = paths.h dnode-nice.c dnode-daemon.c
lib_LTLIBRARIES = libdmglobals.la libdmbootstrap.la
libdmglobals_la_LIBADD = $(PLUGIN_LD) $(ATLAS_LIBS)
libdmglobals_la_SOURCES = $(globsrcs) \
	error-local.h matrix.c matrix.h \
	$(THREADSRC) $(REGEX_SRC) dm-convert.h dm-convert.c
libdmglobals_la_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS)
libdmglobals_la_CFLAGS = $(AM_CFLAGS)

editpaths = sed \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],$(dmstartdir),g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g'

paths.h: paths.h.in Makefile
	-rm -f $@ $@.tmp
	$(editpaths) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

.PHONY: gencode-all gencode
gencode-all: gencode all
gencode:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) gencode

nodist_pkgconfiglib_DATA = dm.pc

dist_pkgdata_SCRIPTS = dgen-build

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h 
dgen_CPPFLAGS = $(AM_CPPFLAGS) -DBOOTSTRAP_PLUGIN=1 -DDM_DISABLE_REGEX=1 \
	@LIBDM_DLL_IMPORT@ 
dgen_LDADD = $(PLUGIN_LD) libdmbootstrap.la 

libdmbootstrap_la_LIBADD = $(PLUGIN_LD)
libdmbootstrap_la_SOURCES = $(globsrcs) 
libdmbootstrap_la_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_THREADS=1 -DDM_DISABLE_REGEX=1
libdmbootstrap_la_CFLAGS = $(AM_CFLAGS)
