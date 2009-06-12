.tex.pdf:
	TEXINPUTS="$(srcdir):$(builddir):$$TEXINPUTS" \
		$(pdflatex) -interaction nonstopmode $<
	TEXINPUTS="$(srcdir):$(builddir):$$TEXINPUTS" \
		$(pdflatex) -interaction nonstopmode $<

.bib.bbl:
	TEXINPUTS="$(srcdir):$(builddir):$$TEXINPUTS" \
		$(pdflatex) -interaction nonstopmode `basename $< .bib`
	TEXINPUTS="$(srcdir):$(builddir):$$TEXINPUTS" \
		BIBINPUTS="$(srcdir):$(builddir):$$BIBINPUTS" \
		$(bibtex) `basename $@ .bbl`

.idx.ind:
	$(makeindex) $<

.tex.idx:
	TEXINPUTS="$(srcdir):$(builddir):$$TEXINPUTS" \
		$(pdflatex) -interaction nonstopmode $<
	TEXINPUTS="$(srcdir):$(builddir):$$TEXINPUTS" \
		$(pdflatex) -interaction nonstopmode $<

.PHONY: clean-local-latex

clean-local-latex:
	-rm *.aux *.bbl *.log *.toc *.out *.blg *.pdf *.ilg *.ind *.lot

clean-local: clean-local-latex
