dist_noinst_DATA = dmnuminc.d dsp1f.d
dist_noinst_SCRIPTS = buildh

AM_CPPFLAGS = $(GCC_OPT) -DSTARTUP_DIR="\"$(top_builddir)/src/codegen/\""
AM_LDFLAGS = -no-undefined
INCLUDE = @INCLTDL@
MAINTAINERCLEANFILES = *.built

noinst_LTLIBRARIES = libdmbootstrap.la
noinst_PROGRAMS = dgen
noinst_HEADERS = dmnuminc.h.built dsp1f.h.built dm.h

libdmbootstrap_la_SOURCES = dmglobals.c dm1.c dm2.c dm3.c dm4.c dm5.c \
	dm6.c dm7.c dm8.c dmnum.c dsp1.c dsp2.c dmnuminc.h dsp1f.h \
	error.h

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h
dgen_CPPFLAGS = $(AM_CPPFLAGS)
dgen_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dgen_LDADD = libdmbootstrap.la

dmnuminc.h.built: dmnuminc.d dgen buildh
	"$(srcdir)/buildh" \
		"$(top_builddir)/src/codegen" "$(srcdir)" "dmnuminc.d" "$@"

dsp1f.h.built: dsp1f.d dgen buildh
	"$(srcdir)/buildh" "\
		$(top_builddir)/src/codegen" "$(srcdir)" "dsp1f.d" "$@"


