PRODSRCS = \
	dmnuminc.h.built \
	dsp1f.h.built \
	dsp2def.h.built \
	dgen_0.h.built \
	dnode_0.h.built \
	dvt_0.h.built \
	dpawn_0.h.built \
	errors.din.built \
	d-mode-ops.el \
	dm-errs.h.built

MAINTAINERCLEANFILES = $(PRODSRCS)

EDITSRCS = dm-prop.c paths.h
DISTEDITSRCS = dcoder.c dmnuminc.d dsp2def.d ops.d
BUILT_SOURCES = $(EDITSRCS) $(DISTEDITSRCS)
DISTCLEANFILES = dm-config.h
INFILES = $(DISTEDITSRCS) $(EDITSRCS) errors.d
CLEANFILES = $(INFILES)
EXTRA_DIST = $(DISTEDITSRCS:=in) $(PRODSRCS)
SUFFIXES =

dist_noinst_DATA = dsp1f.d startup_dgen.d
nodist_noinst_DATA = $(MAINTAINERCLEANFILES)
dist_noinst_SCRIPTS = dgen-build
dgen-build: startup_dgen.d errors.d

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = \
	-DDM_DISABLE_THREADS=1 -DDM_HAVE_CONFIG_H -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	-DDM_NO_PROP=1 \
	-I"$(top_srcdir)/src" -I"$(top_builddir)/src" \
	-I"$(top_builddir)/lib" -I"$(top_srcdir)/lib"
AM_LDFLAGS = -no-undefined
AM_LIBADD = @RELEASE_INFO@ @VERSION_INFO@ $(top_builddir)/lib/libgnu.la
AM_LDADD = $(top_builddir)/lib/libgnu.la
INCLUDE = @INCLTDL@

noinst_LTLIBRARIES = libdmbootstrap.la
noinst_PROGRAMS = dgen dcoder

libdmbootstrap_la_SOURCES = dmnum.c dsp1.c dsp2.c dmnuminc.h dsp1f.h \
	dmglobals.c dm1.c dm2.c dm-conv.c dm-swapbytes.h \
	dm4.c dm5.c dm6.c dm7.c dm8.c dm-types.c \
	dm-signals.c dm-signals.h
nodist_libdmbootstrap_la_SOURCES = paths.h
libdmbootstrap_la_LIBADD = $(AM_LIBADD)
dm2.c dm3.c: paths.h


dgen_SOURCES = dgen.c dgen_0.h dgen_1.h dm-nextevent.c dm-nextevent.h \
	dm3.c dm3.h dm-prop.h dgen-dm3.c dgen-dm3.h
nodist_dgen_SOURCES = dm-prop.c paths.h

dgen_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dgen_LDADD = $(builddir)/libdmbootstrap.la $(AM_LDADD)

dcoder_SOURCES = dcoder.h
nodist_dcoder_SOURCES = dcoder.c dm-errs.h.built
dcoder_CPPFLAGS = -I"$(top_srcdir)/src" -I"$(top_builddir)/src" \
	-DDM_HAVE_CONFIG_H
dcoder_LDADD = $(AM_LDADD)

$(srcdir)/dsp2def.h.built: $(dsp2defd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dsp2def.d" "$@" 
	(cd "$(srcdir)/.." ; \
	 rm dsp2def.h ; \
	 $(LN_S) codegen/dsp2def.h.built dsp2def.h)

$(srcdir)/dmnuminc.h.built: $(dmnuminc) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dmnuminc.d" "$@" 
	(cd "$(srcdir)/.." ; \
	 rm dmnuminc.h ; \
	 $(LN_S) codegen/dmnuminc.h.built dmnuminc.h)

$(srcdir)/dsp1f.h.built: dsp1f.d $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dsp1f.d" "$@"
	(cd "$(srcdir)/.." ; \
	 rm dsp1f.h ; \
	 $(LN_S) codegen/dsp1f.h.built dsp1f.h)

edit = sed \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],.,g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g' \
	-e 's,[@]ROLLBITS[@],$(ROLLBITS),g' \
	-e 's,[@]BYTECORRECT[@],$(BYTECORRECT),g' \
	-e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g' \
	-e 's,[@]srcdir[@],$(srcdir),g'

$(srcdir)/dgen_0.h.built: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dgen"
	(cd "$(srcdir)/.." ; \
	rm dgen_0.h ; \
	$(LN_S) codegen/dgen_0.h.built dgen_0.h)

$(srcdir)/dvt_0.h.built: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dvt"
	(cd "$(srcdir)/.." ; \
	rm dvt_0.h ; \
	$(LN_S) codegen/dvt_0.h.built dvt_0.h)

$(srcdir)/dnode_0.h.built: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dnode"
	(cd "$(srcdir)/.." ; \
	rm dnode_0.h ; \
	$(LN_S) codegen/dnode_0.h.built dnode_0.h)

$(srcdir)/dpawn_0.h.built: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dpawn"
	(cd "$(srcdir)/.." ; \
	rm dpawn_0.h ; \
	$(LN_S) codegen/dpawn_0.h.built dpawn_0.h)

$(srcdir)/dcoder.h: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dcoder"

$(srcdir)/dm-errs.h.built: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/derr"

$(srcdir)/d-mode-ops.el: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/demacs"

$(srcdir)/errors.din.built: $(dcoder)
	"$(top_builddir)/src/codegen/dcoder" \
		"$(srcdir)/errors.din.built"
	( \
	  cd "$(srcdir)/../../dcode" \
	  && rm -f errors.d.in \
	  && $(LN_S) ../src/codegen/errors.d.in errors.d.in \
	)

include $(top_srcdir)/m4/cf_in.make

###############################################################
# For getting around automake && make stupidity to distribute
# this directory
##############################################################

# Files to only build when we're building
dgen = dgen
dmnumincd = dmnuminc.d
opsd = ops.d
dsp2defd = dsp2def.d
dcoder = dcoder
dgen_build = dgen-build

distdir_global = distdir
distdir_local = distdir-local

# Redirect distdir, while resetting the files above ^^
# to empty
$(distdir_global): $(distdir_local)

SAVE_DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)
DISTFILES = 
distdir-local:
	$(MAKE) $(AM_MAKEFLAGS) \
		top_distdir="$(top_distdir)" \
		distdir="$(distdir)" \
		am__remove_distdir=: \
		am__skip_length_check=: \
		dgen= dmnumincd= opsd= dsp2defd= dcoder= Makefile= \
		dgen_build= \
		distdir_local= DISTFILES="$(SAVE_DISTFILES)" \
		distdir
