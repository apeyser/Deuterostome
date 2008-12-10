doc_DATA = dvt-3.0.pdf dvt-emacs.pdf newbook.pdf

dist_noinst_DATA = chap1.tex chap2.tex chap3.tex \
	dvt.bib \
	redefs.sty pdf.sty options.sty \
	dvt-3.0.tex dvt-emacs.tex newbook.tex

EXTRA_DIST = 2ND_PRNT.PS \
	contents1.doc contents2.doc Contents3.doc \
	dd1.doc dd2.doc dd3.doc \
	part1.doc part2.doc part3.doc part4.doc \
	title.doc

dvt-emacs.bbl: dvt-emacs.tex
dvt-emacs.tex: dvt-emacs.bbl config.sty
dvt-3.0.bbl: dvt-3.0.tex
dvt-3.0.tex: dvt-3.0.bbl config.sty
newbook.tex: chap1.tex chap2.tex chap3.tex config.sty
newbook.pdf: newbook.ind

include $(top_srcdir)/m4/cf_latex.make
