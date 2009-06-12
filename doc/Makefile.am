INFILES = config.sty
EXTRA_DIST = $(INFILES:=in) \
	2ND_PRNT.PS \
	chap1.tex chap2.tex chap3.tex appendix.tex \
	dvt.bib \
	redefs.sty pdf.sty options.sty newbook.sty \
	dvt-3.0.tex dvt-emacs.tex newbook.tex \
	contents1.doc contents2.doc Contents3.doc \
	dd1.doc dd2.doc dd3.doc \
	part1.doc part2.doc part3.doc part4.doc \
	title.doc
CLEANFILES = $(INFILES)
SUFFIXES =

doc_DATA = dvt-3.0.pdf dvt-emacs.pdf newbook.pdf

dvt-emacs.bbl: dvt-emacs.tex
dvt-emacs.tex: config.sty
	test -e $@ || cp $(srcdir)/$@ $@
	touch $@
dvt-emacs.pdf: dvt-emacs.bbl

dvt-3.0.bbl: dvt-3.0.tex
dvt-3.0.tex: config.sty 
	test -e $@ || cp $(srcdir)/$@ $@
	touch $@
dvt-3.0.pdf: dvt-3.0.bbl

newbook.pdf: newbook.ind newbook.bbl
newbook.bbl: newbook.tex
newbook.tex: chap1.tex chap2.tex chap3.tex appendix.tex config.sty newbook.sty
	test -e $@ || cp $(srcdir)/$@ $@
	touch $@

edit = sed -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
	$(SVNVERSION_EDIT) \
	-e 's|[@]amsmath[@]|\$(amsmath)|g'

include $(top_srcdir)/m4/cf_latex.make
include $(top_srcdir)/m4/cf_svnversion.make
include $(top_srcdir)/m4/cf_bundle_sub.make
include $(top_srcdir)/m4/cf_in.make

distclean-local: distclean-local-tex

.PHONY: distclean-local-tex
distclean-local-tex:
	if test "$(abs_builddir)" != "$(abs_srcdir)"; then \
	  rm -f "$(builddir)"/dvt-3.0.tex \
	        "$(builddir)"/newbook.tex \
	        "$(builddir)"/dvt-emacs.tex || :; \
	fi

