AM_CPPFLAGS = -DDM_HAVE_CONFIG_H -I"$(top_builddir)/lib" -I"$(top_srcdir)/lib"
AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_CXXFLAGS = $(GCC_CXXFLAGS)
AM_LDFLAGS = -no-undefined 
AM_LIBS = $(PTHREAD_LIBS) $(X_LDFLAGS) 
AM_LIBADD = $(AM_LIBS) @RELEASE_INFO@ @VERSION_INFO@ \
	$(top_builddir)/lib/libgnu.la
AM_LDADD = $(AM_LIBS) $(top_builddir)/lib/libgnu.la
INCLUDES =

DIST_SUBDIRS = codegen
BUILT_SOURCES = dnode-nice.c dnode-daemon.c paths.h \
		drook.c dqueen.c dm-prop.c dm-sem.c
INFILES = $(BUILT_SOURCES)
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
DISTCLEANFILES = dm-config.h
SUFFIXES =

all-local:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) dgen

clean-local:
	! test -e codegen/Makefile \
	|| (cd codegen && $(MAKE) $(AM_MAKEFLAGS) clean)

dpawn = $(bindir)/dpawn
drook = $(bindir)/drook
dmprop = $(bindir)/dmprop

edit = -e 's,[@]bindir[@],$(bindir),g' \
       -e 's,[@]srcdir[@],$(srcdir),g' \
       -e 's,[@]drook[@],$(drook),g' \
       -e "s,[@]DPAWN_EXEC[@],`for i in $(DPAWN_COMMAND) $(dpawn); do echo \\\"$$i\\\"; break; done | tr -d \"\n\"`,g" \
       -e "s|[@]DPAWN_ARGS[@]|`first=:; for i in $(DPAWN_COMMAND) $(dpawn); do if $$first; then first=false; else echo \\\"$$i\\\",; fi ; done | tr -d \"\n\"`|g" \
       -e 's,[@]DPAWN_COMMAND[@],$(DPAWN_COMMAND) $(dpawn),g' \
       -e 's,[@]dmprop[@],$(dmprop),g' \
       -e 's,[@]dmsockdir[@],$(dmsockdir),g' \
       -e 's,[@]dmstartdir[@],$(dmstartdir),g' \
       -e 's,[@]pkglibdir[@],$(pkglibdir),g' \
       -e 's,[@]dmconfdir[@],$(dmconfdir),g' \
       -e 's,[@]SEM_DNODE_NAME[@],$(SEM_DNODE_NAME),g'


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

if ENABLE_DAEMON

nodist_dnode_daemon_SOURCES = dnode-daemon.c
DNODEDAEMONBIN = dnode-daemon

endif ENABLE_DAEMON

if ENABLE_ATLAS
  ATLAS_ACTIVATE = -DATLAS_LIB
endif ENABLE_ATLAS

if ENABLE_THREADS
  THREAD_HDR=threads.h
  THREADSRC=threads.c
endif ENABLE_THREADS

if ENABLE_SEM
  SEM_HDR=dm-sem.h
  SEM_SRC=dm-sem.c
endif ENABLE_SEM

if ENABLE_REGEX
  REGEX_SRC=dregex.c dregex.h
endif ENABLE_REGEX

if ENABLE_RTHREADS
MPI_PROGS = drook dpawn
DNODE_MPI_SRCS = dqueen.h
NODIST_DNODE_MPI_SRCS = dqueen.c
endif ENABLE_RTHREADS

if ENABLE_X11
DVTXSETINPUTFOCUSBIN = dvt-x-set-input-focus
dvt_x_set_input_focus_SOURCES = x-set-input-focus.c
dvt_x_set_input_focus_LDADD = $(X_LDFLAGS)
endif ENABLE_X11

bin_PROGRAMS = dvt dnode dgen \
	$(DNODEDAEMONBIN) $(MPI_PROGS) dmprop \
	$(DVTXSETINPUTFOCUSBIN)
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS_SUPPORT
if ENABLE_CPPPLUGIN
CPPPLUGIN_HDR = cppplugin.h
endif ENABLE_CPPPLUGIN
PLUGIN_HDR = plugin.h $(CPPPLUGIN_HDR) $(top_srcdir)/libltdl/ltdl.h
PLUGIN_LD = @LIBLTDL@
INCLUDES += @INCLTDL@
endif ENABLE_PLUGINS_SUPPORT

pkginclude_HEADERS = dm.h dm-errs.h pluginlib.h $(PLUGIN_HDR) \
	dmx.h $(THREAD_HDR) $(SEM_HDR) \
	error-local.h dm-snprintf.h srandomdev-local.h dmnum.h \
	basic-defs.h dm-types.h dm-nextevent.h \
	dm2.h dm3.h dm4.h dm5.h dm6.h \
	cppplugin.h
nodist_pkginclude_HEADERS = dm-config.h paths.h

# These source are only for the dvt and dnode, since they are
#  dependent on sockets, file-descriptors and gnulib-tool
#  libraries
SOCKSRCS = \
	dm-nextevent.c dm-nextevent.h \
	dm3.c dm3.h \
	dm-proc.c dm-proc.h dm-glob.c dm-glob.h \
	dm-prop.h dm-dvt-vm.h 
NODIST_SOCKSRCS = dm-prop.c

dvt_SOURCES = dvt.c dvt_0.h dm-dvt.h dm-dvt.c $(SOCKSRCS)
nodist_dvt_SOURCES = $(NODIST_SOCKSRCS)
dvt_LDADD = $(builddir)/libdmglobals.la \
	$(AM_LDADD) $(ATLAS_LIBS)
dvt_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_FLAGS)
dvt_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

dnode_SOURCES = dnode.c dnode_0.h dm-dnode.h dm-dnode.c \
	$(SOCKSRCS) \
	$(DNODE_MPI_SRCS)
nodist_dnode_SOURCES = $(NODIST_SOCKSRCS) $(NODIST_DNODE_MPI_SRCS) $(SEM_SRC)
dnode_LDADD = $(builddir)/libdmglobals.la \
	$(builddir)/libdmnodal.la \
	$(AM_LDADD) $(ATLAS_LIBS) $(MPI_LIBS)
dnode_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@ 
dnode_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) \
	$(ATLAS_FLAGS) $(MPI_FLAGS)

dpawn_SOURCES = dpawn.c dpawn_0.h dm-dpawn.h dm-dpawn.c dm-mpi.h dm-mpi.c
dpawn_LDADD = $(builddir)/libdmglobals.la \
	$(builddir)/libdmnodal.la \
	$(AM_LDADD) $(ATLAS_LIBS) $(MPI_LIBS)
dpawn_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dpawn_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS) $(MPI_FLAGS)

testwordorder_SOURCES = test-word-order.c

GLOBSRCS = dmglobals.c \
	dmnuminc.h dmnum.c dsp1f.h dsp1.c dsp2.c \
	dm2.c dm2.h dm-conv.c dm-swapbytes.h\
	dm4.c dm4.h dm5.c dm5.h dm6.c dm6.h dm7.c dm7.h dm8.c dm9.c \
	dm-types.c xhack.h \
	dm1.c \
	dm-signals.c dm-signals.h
NODIST_GLOBSRCS = paths.h
dm2.c dm3.c: paths.h

lib_LTLIBRARIES = libdmglobals.la libdmbootstrap.la libdmnodal.la
libdmglobals_la_LIBADD = $(ATLAS_LIBS) $(AM_LIBADD)
libdmglobals_la_SOURCES = $(GLOBSRCS) \
	matrix.c matrix.h \
	$(THREADSRC) $(REGEX_SRC) dm-convert.h dm-convert.c
nodist_libdmglobals_la_SOURCES = $(NODIST_GLOBSRCS)
libdmglobals_la_CPPFLAGS = $(AM_CPPFLAGS) $(ATLAS_ACTIVATE) $(ATLAS_FLAGS)

libdmnodal_la_SOURCES = dm-vm.c dm-vm.h dm-dvt-vm.h \
	$(PLUGIN_HDR) pluginlib.h plugin.c
libdmnodal_la_LIBADD = $(builddir)/libdmglobals.la \
	$(AM_LIBADD) $(PLUGIN_LD) 

.PHONY: gencode-all gencode
gencode-all: gencode all
gencode:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS)

nodist_pkgconfiglib_DATA = dm.pc

dist_pkgdata_SCRIPTS = dgen-build

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h dm-nextevent.c \
	dm-nextevent.h dm3.c dm3.h dm-prop.h
nodist_dgen_SOURCES = dm-prop.c
dgen_CPPFLAGS = $(AM_CPPFLAGS) \
	-DBOOTSTRAP_PLUGIN=1 -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 @LIBDM_DLL_IMPORT@ 
dgen_LDADD = $(builddir)/libdmbootstrap.la $(AM_LDADD)

nodist_drook_SOURCES = drook.c
drook_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_REGEX=1 -DDM_DISABLE_XDISPLAY \
	$(MPI_FLAGS) @LIBDM_DLL_IMPORT@ 
drook_LDADD = $(builddir)/libdmbootstrap.la $(AM_LDADD) $(MPI_LIBS)

dmprop_SOURCES = dm-prop.h dm-signals.c dm-signals.h 
nodist_dmprop_SOURCES = dm-prop.c
dmprop_CPPFLAGS = $(AM_CPPFLAGS) -DDM_PROP_MAIN
dmprop_LDADD = $(AM_LDADD)

libdmbootstrap_la_LIBADD = $(AM_LIBADD)
libdmbootstrap_la_SOURCES = $(GLOBSRCS)
nodist_libdmbootstrap_la_SOURCES = $(NODIST_GLOBSRCS)
libdmbootstrap_la_CPPFLAGS = $(AM_CPPFLAGS) \
	-DDM_DISABLE_THREADS=1 -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	-DDM_DISABLE_SEM=1

include $(top_srcdir)/m4/cf_in.make
