dist_noinst_DATA = dmnuminc.d dsp1f.d dsp2def.d
dist_noinst_SCRIPTS = dgen-build

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = \
	-DDM_DISABLE_THREADS=1 -DDM_HAVE_CONFIG_H -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	-I"$(top_srcdir)/src" -I"$(top_builddir)/src"
AM_LDFLAGS = -no-undefined
INCLUDE = @INCLTDL@
MAINTAINERCLEANFILES = *.built
BUILT_SOURCES = paths.h
CLEANFILES = $(BUILT_SOURCES)
DISTCLEANFILES = dm-config.h

noinst_LTLIBRARIES = libdmbootstrap.la
noinst_PROGRAMS = dgen

PRODSRCS = dmnuminc.h.built dsp1f.h.built dsp2def.h.built \
	dgen_0.h.built dnode_0.h.built dvt_0.h.built dpawn_0.h.built

EXTRA_DIST = dmnuminc.d.in paths.h.in dsp2def.d.in

gencode: all $(PRODSRCS)

libdmbootstrap_la_SOURCES = dmnum.c dsp1.c dsp2.c dmnuminc.h dsp1f.h paths.h \
	dmglobals.c dm1.c dm2.c dm-conv.c dm-swapbytes.h \
	dm4.c dm5.c dm6.c dm7.c dm8.c dm-types.c 

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h dm-nextevent.c dm-nextevent.h dm3.c
dgen_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dgen_LDADD = libdmbootstrap.la

dsp2def.h.built: dsp2def.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dsp2def.d" "$@" 
	(cd "$(srcdir)/.." ; \
	 rm dsp2def.h ; \
	 $(LN_S) codegen/dsp2def.h.built dsp2def.h)

dmnuminc.h.built: dmnuminc.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dmnuminc.d" "$@" 
	(cd "$(srcdir)/.." ; \
	 rm dmnuminc.h ; \
	 $(LN_S) codegen/dmnuminc.h.built dmnuminc.h)

dsp1f.h.built: dsp1f.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dsp1f.d" "$@"
	(cd "$(srcdir)/.." ; \
	 rm dsp1f.h ; \
	 $(LN_S) codegen/dsp1f.h.built dsp1f.h)

edit = sed \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],.,g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g'

paths.h: paths.h.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/paths.h.in >$@.tmp
	mv $@.tmp $@


dgen_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dgen"
	(cd "$(srcdir)/.." ; \
	rm dgen_0.h ; \
	$(LN_S) codegen/dgen_0.h.built dgen_0.h)

dvt_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dvt"
	(cd "$(srcdir)/.." ; \
	rm dvt_0.h ; \
	$(LN_S) codegen/dvt_0.h.built dvt_0.h)

dnode_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dnode"
	(cd "$(srcdir)/.." ; \
	rm dnode_0.h ; \
	$(LN_S) codegen/dnode_0.h.built dnode_0.h)

dpawn_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dpawn"
	(cd "$(srcdir)/.." ; \
	rm dpawn_0.h ; \
	$(LN_S) codegen/dpawn_0.h.built dpawn_0.h)
