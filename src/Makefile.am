SUBDIRS = codegen

dpawn = $(bindir)/dpawn
drook = $(bindir)/drook

edit = sed \
	-e 's,[@]bindir[@],$(bindir),g' \
	-e 's,[@]srcdir[@],$(srcdir),g' \
	-e 's,[@]dpawn[@],$(dpawn),g' \
	-e 's,[@]drook[@],$(drook),g'

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
	-rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dnode-nice.c.in" > $@.tmp
	mv $@.tmp $@

dqueen.c: Makefile $(srcdir)/dqueen.c.in
	-rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dqueen.c.in" >$@.tmp
	mv $@.tmp $@

drook.c: Makefile $(srcdir)/drook.c.in
	-rm -f $@ $@.tmp
	$(edit) "$(srcdir)/drook.c.in" >$@.tmp
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
  REGEX_SRC=dregex.c dregex.h
endif ENABLE_REGEX

if ENABLE_RTHREADS
MPI_PROGS = drook dpawn
DNODE_MPI_SRCS = dqueen.c dqueen.h
endif ENABLE_RTHREADS

EXTRA_DIST =    dnode-nice.c.in dnode-daemon.c.in paths.h.in \
	drook.c.in dqueen.c.in 
BUILT_SOURCES = dnode-nice.c    dnode-daemon.c    paths.h    \
	drook.c    dqueen.c
CLEANFILES = $(BUILT_SOURCES)

DISTCLEANFILES = dm-config.h

AM_CPPFLAGS = -DDM_HAVE_CONFIG_H
AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_CXXFLAGS = $(GCC_CXXFLAGS)
AM_LDFLAGS = $(PTHREAD_LIBS) $(X_LDFLAGS) -no-undefined 
INCLUDE = @INCLTDL@

bin_PROGRAMS = dvt dnode dgen $(DNODEDAEMONBIN) $(MPI_PROGS)
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS_SUPPORT
if ENABLE_CPPPLUGIN
CPPPLUGIN_HDR = cppplugin.h
endif ENABLE_CPPPLUGIN
PLUGIN_HDR = plugin.h pluginlib.h $(CPPPLUGIN_HDR)
PLUGIN_LD = @LIBLTDL@
endif ENABLE_PLUGINS_SUPPORT

pkginclude_HEADERS = dm.h dm3.h $(PLUGIN_HDR) dmx.h $(THREAD_HDR) \
	error-local.h dm-snprintf.h srandomdev-local.h dmnum.h \
	basic-defs.h dm-types.h dm-nextevent.h
nodist_pkginclude_HEADERS = dm-config.h paths.h

dvt_SOURCES = dvt.c dvt_0.h dm-nextevent.c dm-nextevent.h dm3.c \
	dm-dvt.h dm-dvt.c
dvt_LDADD = libdmglobals.la
dvt_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS)
dvt_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_FLAGS)
dvt_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

dnode_SOURCES = dnode.c dnode_0.h dmx.h \
	dm-nextevent.c dm-nextevent.h \
	dm3.c dm-dnode.h dm-dnode.c \
	$(DNODE_MPI_SRCS)

dnode_LDADD = libdmglobals.la 
dnode_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS) $(MPI_LIBS)
dnode_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@ 
dnode_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS) $(MPI_FLAGS)

dpawn_SOURCES = dpawn.c dpawn_0.h dm-dpawn.h dm-dpawn.c dm-mpi.h dm-mpi.c
dpawn_LDADD = libdmglobals.la
dpawn_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS) $(MPI_LIBS)
dpawn_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dpawn_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS) $(MPI_FLAGS)

testwordorder_SOURCES = test-word-order.c

globsrcs = dmglobals.c \
	dmnuminc.h dmnum.c dsp1f.h dsp1.c dsp2.c \
	paths.h dm1.c dm2.c dm2.h dm-conv.c dm-swapbytes.h\
	dm4.c dm5.c dm6.c dm7.c dm8.c dm9.c \
	pluginlib.h plugin.c \
	dm-types.c xhack.h \
	dm-vm.h dm-vm.c

lib_LTLIBRARIES = libdmglobals.la libdmbootstrap.la
libdmglobals_la_LIBADD = $(PLUGIN_LD) $(ATLAS_LIBS) @VERSION_INFO@ @RELEASE_INFO@
libdmglobals_la_SOURCES = $(globsrcs) \
	matrix.c matrix.h \
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
	$(editpaths) "$(srcdir)/$@.in" >$@.tmp
	mv $@.tmp $@

.PHONY: gencode-all gencode
gencode-all: gencode all
gencode:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) gencode

nodist_pkgconfiglib_DATA = dm.pc

dist_pkgdata_SCRIPTS = dgen-build

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h dm-nextevent.c dm-nextevent.h dm3.c
dgen_CPPFLAGS = $(AM_CPPFLAGS) \
	-DBOOTSTRAP_PLUGIN=1 -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	@LIBDM_DLL_IMPORT@ 
dgen_LDADD = $(PLUGIN_LD) libdmbootstrap.la 

drook_SOURCES = drook.c
drook_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_REGEX=1 -DDM_DISABLE_XDISPLAY \
	$(MPI_FLAGS) \
	@LIBDM_DLL_IMPORT@ 
drook_LDFLAGS = $(AM_LDFLAGS) $(MPI_LIBS)
drook_LDADD = libdmbootstrap.la

libdmbootstrap_la_LIBADD = $(PLUGIN_LD) @VERSION_INFO@ @RELEASE_INFO@
libdmbootstrap_la_SOURCES = $(globsrcs) 
libdmbootstrap_la_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_THREADS=1 -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1
libdmbootstrap_la_CFLAGS = $(AM_CFLAGS)
libdmbootstrap_la_LDFLAGS = $(AM_LDFLAGS)
