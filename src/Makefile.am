if ENABLE_SETUID

nodist_dnode_nice_SOURCES = dnode-nice.c
DNODENICEBIN = dnode-nice

edit = sed \
	-e 's,[@]bindir[@],$(bindir),g'

dnode-nice.c: Makefile $(srcdir)/dnode-nice.c.in
	rm -f $@ $@.tmp
	$(edit) "$(srcdir)/dnode-nice.c.in" > $@.tmp
	mv $@.tmp $@

install-exec-local:
	$(INSTALL_PROGRAM_ENV) $(LIBTOOL) --mode=install \
		$(INSTALL) -m 4755 "dnode-nice$(EXEEXT)" \
			"$(DESTDIR)$(bindir)/dnode-nice$(EXEEXT)" 

uninstall-local:
	-rm -f "$(DESTDIR)$(bindir)/dnode-nice$(EXEEXT)"

endif ENABLE_SETUID

if ENABLE_THREADS
  THREAD_HDR=threads.h
  THREADSRC=threads.c
endif ENABLE_THREADS

EXTRA_DIST = codegen/buildh codegen/dgen_0.h codegen/dgen_1.h codegen/dgen.c \
	codegen/dmnuminc.d.in codegen/dmnuminc.h codegen/dmnuminc.h.built \
	codegen/dsp1f.d codegen/dsp1f.h codegen/dsp1f.h.built \
	codegen/Makefile.am codegen/Makefile.in codegen/startup_dgen.d.in \
	dnode-nice.c.in paths.h.in

DISTCLEANFILES = dm-config.h

distclean-local:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) distclean

clean-local:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) clean

AM_CPPFLAGS = -DDM_HAVE_CONFIG_H
AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_LDFLAGS = -no-undefined $(PTHREAD_LIBS) $(X_LDFLAGS)
INCLUDE = @INCLTDL@

bin_PROGRAMS = dvt dnode 
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS
PLUGIN_HDR = plugin.h pluginlib.h
PLUGIN_LD = @LIBLTDL@
endif ENABLE_PLUGINS

pkginclude_HEADERS = dm.h $(PLUGIN_HDR) dmx.h $(THREAD_HDR) \
	error-local.h
nodist_pkginclude_HEADERS = dm-config.h  paths.h

dvt_SOURCES = dvt.c dvt_0.h dvt_1.h
dvt_LDADD = libdmglobals.la
dvt_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

dnode_SOURCES = dnode.c dnode_0.h dnode_1.h dmx.h
dnode_LDADD = libdmglobals.la
dnode_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

testwordorder_SOURCES = test-word-order.c

BUILT_SOURCES = paths.h dnode-nice.c
CLEANFILES = paths.h dnode-nice.c
lib_LTLIBRARIES = libdmglobals.la
libdmglobals_la_LIBADD = $(PLUGIN_LD)
libdmglobals_la_SOURCES = dmglobals.c \
	dm1.c dm2.c dm3.c dm4.c dm5.c dm6.c dm7.c dm8.c dm9.c \
	dmnum.c dsp1.c dsp2.c error-local.h \
	plugin.c pluginlib.h $(THREADSRC)

editpaths = sed \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],$(dmstartdir),g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g'

paths.h: paths.h.in Makefile
	-rm -f $@ $@.tmp
	$(editpaths) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

gencode-all: gencode all
gencode:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) all

pgkconfiglibdir=@pkgconfiglibdir@
PCCFLAGS = $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
PCLIBS=-ldmglobals
PCLDFLAGS=-module -no-undefined

editpkgconfig = sed \
	-e 's,[@]prefix[@],$(prefix),g' \
	-e 's,[@]exec_prefix[@],$(exec_prefix),g' \
	-e 's,[@]pkgincludedir[@],$(pkgincludedir),g' \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],$(dmstartdir),g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g' \
	-e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g' \
	-e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
	-e 's,[@]LIBS[@],$(PCLIBS),g' \
	-e 's,[@]CFLAGS[@],$(PCCFLAGS),g' \
	-e 's,[@]LDFLAGS[@],$(PCLDFLAGS),g' \
	-e 's,[@]pkgdatadir[@],$(pkgdatadir),g' \
	-e 's,[@]m4datadir[@],$(m4datadir),g' \
	-e 's,[@]plugindatadir[@],$(plugindatadir),g' 

nodist_pkgconfiglib_DATA = dm.pc
EXTRA_DIST += dm.pc.in
CLEANFILES += dm.pc

dm.pc: dm.pc.in Makefile
	-rm -f $@ $@.tmp
	$(editpkgconfig) $(srcdir)/$@.in > $@.tmp
	mv $@.tmp $@
