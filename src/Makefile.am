#DIST_SUBDIRS = codegen

if ENABLE_SETUID

dnode_nice_SOURCE = dnode-nice.c
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
	codegen/dm1.c codegen/dm2.c codegen/dm3.c codegen/dm4.c codegen/dm5.c \
	codegen/dm6.c codegen/dm7.c codegen/dm8.c codegen/dmglobals.c \
	codegen/dm.h codegen/dmnum.c codegen/dsp1.c codegen/dsp2.c error.h\
	codegen/dmnuminc.d.in codegen/dmnuminc.h codegen/dmnuminc.h.built \
	codegen/dsp1f.d codegen/dsp1f.h codegen/dsp1f.h.built \
	codegen/Makefile.am codegen/Makefile.in codegen/startup_dgen.d.in \
	dnode-nice.c.in

distclean-local:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) distclean

clean-local:
if ENABLE_SETUID
	-rm -f dnode-nice.c
endif ENABLE_SETUID
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) clean


AM_CFLAGS = $(GCC_OPT) $(X_CFLAGS)
AM_CPPFLAGS = \
	-DSTARTUP_DIR="\"$(dmstartdir)\"" \
	-DPLUGIN_DIR="\"$(pkglibdir)\"" \
	-DCONF_DIR="\"$(dmconfdir)\""
AM_LDFLAGS = -no-undefined $(PTHREAD_LIBS) $(X_LDFLAGS)
INCLUDE = @INCLTDL@

bin_PROGRAMS = dvt dnode 
noinst_PROGRAMS = testwordorder $(DNODENICEBIN)

if ENABLE_PLUGINS
PLUGIN_HDR = plugin.h
PLUGIN_LD = @LIBLTDL@
endif ENABLE_PLUGINS

pkginclude_HEADERS = dm.h $(PLUGIN_HDR) dmx.h $(THREAD_HDR)

dvt_SOURCES = dvt.c dvt_0.h dvt_1.h
dvt_LDADD = libdmglobals.la
dvt_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

dnode_SOURCES = dnode.c dnode_0.h dnode_1.h dmx.h
dnode_LDADD = libdmglobals.la
dnode_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@

testwordorder_SOURCES = test-word-order.c

lib_LTLIBRARIES = libdmglobals.la
libdmglobals_la_LIBADD = $(PLUGIN_LD)
libdmglobals_la_SOURCES = dmglobals.c \
	dm1.c dm2.c dm3.c dm4.c dm5.c dm6.c dm7.c dm8.c dm9.c \
	dmnum.c dsp1.c dsp2.c dmnuminc.h dsp1f.h error.h \
	plugin.c pluginlib.h $(THREADSRC)


@SET_MAKE@

gencode-all: gencode all
gencode:
	cd codegen && $(MAKE) $(AM_MAKEFLAGS) all
