SUBDIRS = codegen

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

EXTRA_DIST = dnode-nice.c.in paths.h.in

DISTCLEANFILES = dm-config.h

AM_CPPFLAGS = -DDM_HAVE_CONFIG_H
AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_LDFLAGS = -no-undefined $(PTHREAD_LIBS) $(X_LDFLAGS)
INCLUDE = @INCLTDL@

bin_PROGRAMS = dvt dnode dgen
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS
PLUGIN_HDR = plugin.h pluginlib.h
PLUGIN_LD = @LIBLTDL@
endif ENABLE_PLUGINS

pkginclude_HEADERS = dm.h $(PLUGIN_HDR) dmx.h $(THREAD_HDR) \
	error-local.h dm-snprintf.h
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
#libdmbootstrap.la
libdmglobals_la_LIBADD = $(PLUGIN_LD)
libdmglobals_la_SOURCES = dmglobals.c \
	dm1.c dm2.c dm3.c dm4.c dm5.c dm6.c dm7.c dm8.c dm9.c \
	dmnum.c dsp1.c dsp2.c error-local.h \
	plugin.c pluginlib.h $(THREADSRC) 
libdmglobals_la_CPPFLAGS = $(AM_CPPFLAGS)
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

gencode-all: gencode all
gencode:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) gencode

nodist_pkgconfiglib_DATA = dm.pc

dist_pkgdata_SCRIPTS = dgen-build

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h \
	dmglobals.c \
	dm1.c dm2.c dm3.c dm4.c dm5.c dm6.c dm7.c dm8.c \
	dmnum.c dsp1.c dsp2.c plugin.c  \
	dmnuminc.h dsp1f.h  paths.h pluginlib.h
dgen_CPPFLAGS = $(AM_CPPFLAGS) -DDM_DISABLE_THREADS=1
dgen_CFLAGS = $(AM_CPPFLAGS) @LIBDM_DLL_IMPORT@ 
dgen_LDADD = $(PLUGIN_LD) 
# libdmbootstrap.la 

#libdmbootstrap_la_LIBADD = $(PLUGIN_LD)
#libdmbootstrap_la_SOURCES = dmglobals.c \
#	dm1.c dm2.c dm3.c dm4.c dm5.c dm6.c dm7.c dm8.c \
#	dmnum.c dsp1.c dsp2.c plugin.c  \
#	dmnuminc.h dsp1f.h  paths.h pluginlib.h
#libdmbootstrap_la_CPPFLAGS = $(AM_CPPFLAGS) -DDM_DISABLE_THREADS=1
#libdmbootstrap_la_CFLAGS = $(AM_CFLAGS)
