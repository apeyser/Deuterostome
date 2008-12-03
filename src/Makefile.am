DIST_SUBDIRS = codegen

all-local:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) dgen

clean-local:
	! test -e codegen/Makefile \
	|| (cd codegen && $(MAKE) $(AM_MAKEFLAGS) clean)

dpawn = $(bindir)/dpawn
drook = $(bindir)/drook
dmprop = $(bindir)/dmprop

edit = sed \
	-e 's,[@]bindir[@],$(bindir),g' \
	-e 's,[@]srcdir[@],$(srcdir),g' \
	-e 's,[@]drook[@],$(drook),g' \
	-e "s,[@]DPAWN_EXEC[@],`for i in $(DPAWN_COMMAND) $(dpawn); do echo \\\"$$i\\\"; break; done | tr -d \"\n\"`,g" \
	-e "s|[@]DPAWN_ARGS[@]|`first=:; for i in $(DPAWN_COMMAND) $(dpawn); do if $$first; then first=false; else echo \\\"$$i\\\",; fi ; done | tr -d \"\n\"`|g" \
	-e 's,[@]DPAWN_COMMAND[@],$(DPAWN_COMMAND) $(dpawn),g' \
	-e 's,[@]dmprop[@],$(dmprop),g'

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

dm-prop.c: Makefile $(srcdir)/dm-prop.c.in
	-rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dm-prop.c.in" > $@.tmp
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
	drook.c.in dqueen.c.in dm-prop.c.in
BUILT_SOURCES = dnode-nice.c    dnode-daemon.c    paths.h    \
	drook.c    dqueen.c    dm-prop.c
CLEANFILES = $(BUILT_SOURCES)

DISTCLEANFILES = dm-config.h

AM_CPPFLAGS = -DDM_HAVE_CONFIG_H
AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_CXXFLAGS = $(GCC_CXXFLAGS)
AM_LDFLAGS = $(PTHREAD_LIBS) $(X_LDFLAGS) -no-undefined 
INCLUDE = @INCLTDL@

bin_PROGRAMS = dvt dnode dgen $(DNODEDAEMONBIN) $(MPI_PROGS) dmprop
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS_SUPPORT
if ENABLE_CPPPLUGIN
CPPPLUGIN_HDR = cppplugin.h
endif ENABLE_CPPPLUGIN
PLUGIN_HDR = plugin.h pluginlib.h $(CPPPLUGIN_HDR)
PLUGIN_SRC = plugin.c
PLUGIN_LD = @LIBLTDL@
endif ENABLE_PLUGINS_SUPPORT

pkginclude_HEADERS = dm.h dm-errs.h dm3.h $(PLUGIN_HDR) dmx.h $(THREAD_HDR) \
	error-local.h dm-snprintf.h srandomdev-local.h dmnum.h \
	basic-defs.h dm-types.h dm-nextevent.h dm2.h cppplugin.h
nodist_pkginclude_HEADERS = dm-config.h paths.h

socksrcs = \
	dm-nextevent.c dm-nextevent.h \
	dm3.c dm3.h \
	dm-proc.c dm-proc.h dm-glob.c dm-glob.h \
	dm-prop.c dm-prop.h dm-dvt-vm.h 

dvt_SOURCES = dvt.c dvt_0.h dm-dvt.h dm-dvt.c \
	$(socksrcs)

dvt_LDADD = $(builddir)/libdmglobals.la $(top_builddir)/lib/libgnu.la
dvt_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS)
dvt_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_FLAGS) \
	-I$(top_builddir)/lib -I$(top_srcdir)/lib
dvt_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

dnode_SOURCES = dnode.c dnode_0.h dm-dnode.h dm-dnode.c \
	$(socksrcs) \
	$(DNODE_MPI_SRCS)

dnode_LDADD = $(builddir)/libdmglobals.la \
	$(builddir)/libdmnodal.la \
	$(top_builddir)/lib/libgnu.la
dnode_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS) $(MPI_LIBS)
dnode_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@ 
dnode_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) \
	$(ATLAS_FLAGS) $(MPI_FLAGS) \
	-I$(top_builddir)/lib -I$(top_srcdir)/lib

dpawn_SOURCES = dpawn.c dpawn_0.h dm-dpawn.h dm-dpawn.c dm-mpi.h dm-mpi.c
dpawn_LDADD = $(builddir)/libdmglobals.la \
	$(builddir)/libdmnodal.la \
	$(LDADD)
dpawn_LDFLAGS = $(AM_LDFLAGS) $(ATLAS_LIBS) $(MPI_LIBS) #-rdynamic
dpawn_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dpawn_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS) $(MPI_FLAGS)

testwordorder_SOURCES = test-word-order.c

globsrcs = dmglobals.c \
	dmnuminc.h dmnum.c dsp1f.h dsp1.c dsp2.c \
	paths.h dm2.c dm2.h dm-conv.c dm-swapbytes.h\
	dm4.c dm5.c dm6.c dm7.c dm7.h dm8.c dm9.c \
	dm-types.c xhack.h \
	dm1.c \
	dm-signals.c dm-signals.h

lib_LTLIBRARIES = libdmglobals.la libdmbootstrap.la libdmnodal.la
libdmglobals_la_LIBADD = $(ATLAS_LIBS) @VERSION_INFO@ @RELEASE_INFO@
libdmglobals_la_SOURCES = $(globsrcs) \
	matrix.c matrix.h \
	$(THREADSRC) $(REGEX_SRC) dm-convert.h dm-convert.c

libdmglobals_la_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS)
libdmglobals_la_CFLAGS = $(AM_CFLAGS)

libdmnodal_la_SOURCES = dm-vm.c dm-vm.h dm-dvt-vm.h \
	$(PLUGIN_HDR) $(PLUGIN_SRC)
libdmnodal_la_LIBADD = $(PLUGIN_LD) \
	$(builddir)/libdmglobals.la \
	@VERSION_INFO@ @RELEASE_INFO@

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
	cd codegen && $(MAKE) $(AM_MAKEFLAGS)

nodist_pkgconfiglib_DATA = dm.pc

dist_pkgdata_SCRIPTS = dgen-build

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h dm-nextevent.c \
	dm-nextevent.h dm3.c dm3.h dm-prop.c dm-prop.h
dgen_CPPFLAGS = $(AM_CPPFLAGS) \
	-DBOOTSTRAP_PLUGIN=1 -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	@LIBDM_DLL_IMPORT@ 
dgen_LDADD = $(builddir)/libdmbootstrap.la $(LDADD)

drook_SOURCES = drook.c
drook_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_REGEX=1 -DDM_DISABLE_XDISPLAY \
	$(MPI_FLAGS) \
	@LIBDM_DLL_IMPORT@ 
drook_LDFLAGS = $(AM_LDFLAGS) $(MPI_LIBS)
drook_LDADD = $(builddir)/libdmbootstrap.la $(LDADD)

dmprop_SOURCES = \
	dm-prop.c dm-prop.h \
	dm-signals.c dm-signals.h 
dmprop_CPPFLAGS = $(AM_CPPFLAGS) -DDM_PROP_MAIN

libdmbootstrap_la_LIBADD = @VERSION_INFO@ @RELEASE_INFO@
libdmbootstrap_la_SOURCES = $(globsrcs) 
libdmbootstrap_la_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_THREADS=1 -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1
libdmbootstrap_la_CFLAGS = $(AM_CFLAGS)
libdmbootstrap_la_LDFLAGS = $(AM_LDFLAGS)
