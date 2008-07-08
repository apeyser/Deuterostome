PRODSRCS = \
	$(srcdir)/dmnuminc.h.built \
	$(srcdir)/dsp1f.h.built \
	$(srcdir)/dsp2def.h.built \
	$(srcdir)/dgen_0.h.built \
	$(srcdir)/dnode_0.h.built \
	$(srcdir)/dvt_0.h.built \
	$(srcdir)/dpawn_0.h.built \
	$(srcdir)/errors.d \
	$(srcdir)/d-mode-ops.el \
	$(srcdir)/dm-errs.h.built

MAINTAINERCLEANFILES = $(PRODSRCS) \
	$(srcdir)/dcoder.h
dist_noinst_DATA = \
	$(srcdir)/dmnuminc.d \
	$(srcdir)/ops.d \
	$(srcdir)/dsp1f.d \
	$(srcdir)/dsp2def.d \
	$(srcdir)/startup_dgen.d \
	$(MAINTAINERCLEANFILES)

dist_noinst_SCRIPTS = $(srcdir)/dgen-build

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = \
	-DDM_DISABLE_THREADS=1 -DDM_HAVE_CONFIG_H -DDM_DISABLE_REGEX=1 \
	-DDM_DISABLE_XDISPLAY=1 -DDISABLE_NEXTEVENT=1 \
	-DDM_NO_PROP=1 \
	-I"$(top_srcdir)/src" -I"$(top_builddir)/src"
AM_LDFLAGS = -no-undefined
INCLUDE = @INCLTDL@
BUILT_SOURCES = paths.h dm-prop.c
CLEANFILES = $(BUILT_SOURCES)
DISTCLEANFILES = dm-config.h

noinst_LTLIBRARIES = libdmbootstrap.la
noinst_PROGRAMS = dgen dcoder

EXTRA_DIST = dmnuminc.d.in ops.d.in paths.h.in dsp2def.d.in

libdmbootstrap_la_SOURCES = dmnum.c dsp1.c dsp2.c dmnuminc.h dsp1f.h paths.h \
	dmglobals.c dm1.c dm2.c dm-conv.c dm-swapbytes.h \
	dm4.c dm5.c dm6.c dm7.c dm8.c dm-types.c \
	dm-signals.c dm-signals.h

dgen_SOURCES = dgen.c dgen_0.h dgen_1.h dm-nextevent.c dm-nextevent.h \
	dm3.c dm3.h dm-prop.c dm-prop.h paths.h
dm3.c: paths.h

dgen_CFLAGS = $(AM_CFLAGS) @LIBDM_DLL_IMPORT@
dgen_LDADD = libdmbootstrap.la

dcoder_SOURCES = dcoder.c dcoder.h dm-errs.h.built
dcoder_CPPFLAGS = -I"$(top_srcdir)/src" -I"$(top_builddir)/src" \
	-DDM_HAVE_CONFIG_H

$(srcdir)/dsp2def.h.built: dsp2def.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dsp2def.d" "$@" 
	(cd "$(srcdir)/.." ; \
	 rm dsp2def.h ; \
	 $(LN_S) codegen/dsp2def.h.built dsp2def.h)

$(srcdir)/dmnuminc.h.built: dmnuminc.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dmnuminc.d" "$@" 
	(cd "$(srcdir)/.." ; \
	 rm dmnuminc.h ; \
	 $(LN_S) codegen/dmnuminc.h.built dmnuminc.h)

$(srcdir)/dsp1f.h.built: dsp1f.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "dsp1f.d" "$@"
	(cd "$(srcdir)/.." ; \
	 rm dsp1f.h ; \
	 $(LN_S) codegen/dsp1f.h.built dsp1f.h)

edit = sed \
	-e 's,[@]dmsockdir[@],$(dmsockdir),g' \
	-e 's,[@]dmstartdir[@],$(srcdir),g' \
	-e 's,[@]pkglibdir[@],$(pkglibdir),g' \
	-e 's,[@]dmconfdir[@],$(dmconfdir),g' \
	-e 's,[@]ROLLBITS[@],$(ROLLBITS),g' \
	-e 's,[@]BYTECORRECT[@],$(BYTECORRECT),g' \
	-e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g' \
	-e 's,[@]srcdir[@],$(srcdir),g'

paths.h: paths.h.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/paths.h.in >$@.tmp
	mv $@.tmp $@

ops.d: ops.d.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/ops.d.in >$@.tmp
	mv $@.tmp $@

dcoder.c: dcoder.c.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dcoder.c.in >$@.tmp
	mv $@.tmp $@

dmnuminc.d: dmnuminc.d.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dmnuminc.d.in >$@.tmp
	mv $@.tmp $@

dsp2def.d: dsp2def.d.in Makefile
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dsp2def.d.in >$@.tmp
	mv $@.tmp $@

$(srcdir)/dgen_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dgen"
	(cd "$(srcdir)/.." ; \
	rm dgen_0.h ; \
	$(LN_S) codegen/dgen_0.h.built dgen_0.h)

$(srcdir)/dvt_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dvt"
	(cd "$(srcdir)/.." ; \
	rm dvt_0.h ; \
	$(LN_S) codegen/dvt_0.h.built dvt_0.h)

$(srcdir)/dnode_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dnode"
	(cd "$(srcdir)/.." ; \
	rm dnode_0.h ; \
	$(LN_S) codegen/dnode_0.h.built dnode_0.h)

$(srcdir)/dpawn_0.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dpawn"
	(cd "$(srcdir)/.." ; \
	rm dpawn_0.h ; \
	$(LN_S) codegen/dpawn_0.h.built dpawn_0.h)

$(srcdir)/dcoder.h: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/dcoder"

$(srcdir)/dm-errs.h.built: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/derr"

$(srcdir)/d-mode-ops.el: ops.d dgen dgen-build
	"$(srcdir)/dgen-build" \
		"$(top_builddir)/src/codegen/dgen" "$(srcdir)" "ops.d" \
		"$@" "/demacs"

$(srcdir)/errors.d: dcoder
	"$(top_builddir)/src/codegen/dcoder" "$(srcdir)/errors.d"
	(cd "$(srcdir)/../../dcode" ; \
	rm errors.d ; \
	$(LN_S) ../src/codegen/errors.d errors.d)

dm-prop.c: Makefile $(top_builddir)/src/codegen/dm-prop.c.in
	-rm -f $@ $@.tmp
	$(edit) "$(top_builddir)/src/codegen/dm-prop.c.in" > $@.tmp
	mv $@.tmp $@

distdir:
	@srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	topsrcdirstrip=`echo "$(top_srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	list='$(DISTFILES)'; \
	  dist_files=`for file in $$list; do echo $$file; done | \
	  sed -e "s|^$$srcdirstrip/||;t" \
	      -e "s|^$$topsrcdirstrip/|$(top_builddir)/|;t"`; \
	case $$dist_files in \
	  */*) $(MKDIR_P) `echo "$$dist_files" | \
			   sed '/\//!d;s|^|$(distdir)/|;s,/[^/]*$$,,' | \
			   sort -u` ;; \
	esac; \
	for file in $$dist_files; do \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  if test -d $$d/$$file; then \
	    dir=`echo "/$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      cp -pR $(srcdir)/$$file $(distdir)$$dir || exit 1; \
	    fi; \
	    cp -pR $$d/$$file $(distdir)$$dir || exit 1; \
	  else \
	    test -f $(distdir)/$$file \
	    || cp -p $$d/$$file $(distdir)/$$file \
	    || exit 1; \
	  fi; \
	done
