DGEN_H_BUILT = \
	dsp2def.h.built \
	dmnuminc.h.built \
	dsp1f.h.built

DGEN_OPS_BUILT= \
	dgen_0.h.built \
	dvt_0.h.built \
	dnode_0.h.built \
	dpawn_0.h.built

PRODSRCS = \
	$(DGEN_H_BUILT) \
	$(DGEN_OPS_BUILT) \
	errors.d.built \
	d-mode-ops.el \
	dm-errs.h.built

MAINTAINERCLEANFILES = $(PRODSRCS)

EDITSRCS = dm-prop.c paths.h
DISTEDITSRCS = dcoder.c dmnuminc.d dsp2def.d ops.d
BUILT_SOURCES = $(EDITSRCS) $(DISTEDITSRCS)
DISTCLEANFILES = dm-config.h
INFILES = $(DISTEDITSRCS) $(EDITSRCS)
CLEANFILES = $(INFILES)
EXTRA_DIST = $(DISTEDITSRCS:=in) $(PRODSRCS) $(IDFILES)
SUFFIXES =

dist_noinst_DATA = dsp1f.d startup_dgen.d errors.d
nodist_noinst_DATA = $(MAINTAINERCLEANFILES)
dist_noinst_SCRIPTS = dgen-build
dgen-build: startup_dgen.d errors.d

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = \
	-DDM_DISABLE_THREADS=1 -DDM_HAVE_CONFIG_H -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	-DDM_NO_PROP=1 -DDISABLE_OPENSSL=1 \
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

$(DGEN_H_BUILT): $(dgen) $(dgen_build)
	f=`echo $@ | sed -e 's,.*/,,g' -e 's,\..*,.d,g'`; \
	"$(srcdir)/dgen-build" \
	  "$(top_builddir)/src/codegen/dgen" "$(srcdir)" \
	  "$$f" "$@"
	(f=`echo $@ | sed -e 's,.*/,,g' -e 's,\.built,,g'`; \
	cd "$(srcdir)/.."; \
	rm $$f; \
	$(LN_S) codegen/$$f.built $$f)

dsp2def.h.built: $(dsp2defd)
dmnuminc.h.built: $(dmnuminc)
dsp1f.h.built: dsp1f.d

$(DGEN_OPS_BUILT): $(opsd) $(dgen) $(dgen_build)
	dict=`echo $@ | sed -e 's,.*/,,g' -e 's,_.*,,g'`; \
	"$(srcdir)/dgen-build" \
	  "$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
	  "$@" "/$$dict"
	(f=`echo $@ | sed -e 's,.*/,,g' -e 's,\.built,,g'`; \
	cd "$(srcdir)/.." ; \
	rm $$f ; \
	$(LN_S) codegen/$$f.built $$f)

dcoder.h: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dcoder"

dm-errs.h.built: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/derr"

d-mode-ops.el: $(opsd) $(dgen) $(dgen_build)
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/demacs"

errors.d.built: $(dcoder)
	"$(top_builddir)/src/codegen/dcoder" \
		"$(srcdir)/errors.d.built"
	( \
	  cd "$(srcdir)/../../dcode" \
	  && rm -f errors.d \
	  && $(LN_S) ../src/codegen/errors.d.built errors.d \
	)

edit = -e 's,[@]dmsockdir[@],$(dmsockdir),g' \
       -e 's,[@]dmstartdir[@],.,g' \
       -e 's,[@]pkglibdir[@],$(pkglibdir),g' \
       -e 's,[@]dmconfdir[@],$(dmconfdir),g' \
       -e 's,[@]ROLLBITS[@],$(ROLLBITS),g' \
       -e 's,[@]BYTECORRECT[@],$(BYTECORRECT),g' \
       -e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g' \
       -e 's,[@]srcdir[@],$(srcdir),g'

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
