dist_noinst_DATA = dmnuminc.d dsp1f.d
dist_noinst_SCRIPTS = buildh

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = -DDM_DISABLE_THREADS=1 -DDM_HAVE_CONFIG_H \
	-I"$(top_srcdir)/src" -I"$(top_builddir)/src"
AM_LDFLAGS = -no-undefined
INCLUDE = @INCLTDL@
MAINTAINERCLEANFILES = *.built
BUILT_SOURCES = paths.h
CLEANFILES = paths.h

noinst_LTLIBRARIES = libdmbootstrap.la
noinst_PROGRAMS = dgen
noinst_HEADERS = dmnuminc.h.built dsp1f.h.built

libdmbootstrap_la_SOURCES = dmglobals.c dm1.c dm2.c dm3.c dm4.c dm5.c \
	dm6.c dm7.c dm8.c dmnum.c dsp1.c dsp2.c dmnuminc.h dsp1f.h 

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h
dgen_CPPFLAGS = $(AM_CPPFLAGS)
dgen_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dgen_LDADD = libdmbootstrap.la

dmnuminc.h.built: dmnuminc.d dgen buildh
	"$(srcdir)/buildh" \
		"$(top_builddir)/src/codegen" "$(srcdir)" "dmnuminc.d" "$@"
	(cd "$(srcdir)/.." ; \
	 rm dmnuminc.h ; \
	 $(LN_S) codegen/dmnuminc.h.built dmnuminc.h)

dsp1f.h.built: dsp1f.d dgen buildh
	"$(srcdir)/buildh" "\
		$(top_builddir)/src/codegen" "$(srcdir)" "dsp1f.d" "$@"
	(cd "$(srcdir)/.." ; \
	 rm dsp1f.h ; \
	 $(LN_S) codegen/dsp1f.h.built dsp1f.h)


edit = sed \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],$(srcdir),g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g'

paths.h: paths.h.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@
