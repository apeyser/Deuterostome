nodist_dmstart_SCRIPTS = runtool.sh
if ENABLE_BASH
nodist_dmstart_SCRIPTS += dnode.sh
endif
if ENABLE_PERL
nodist_dmstart_SCRIPTS += print.pl
endif

EXTRA_DIST = dnode.sh.in print.pl.in runtool.sh.in
CLEANFILES = $(nodist_dmstart_SCRIPTS)



clean-local:
	-rm -f $(dmstart_SCRIPTS)

XTERM_START = /^[[:space:]]*[@]ENABLE_XTERM_START[@][[:space:]]*$$/
XTERM_END = /^[[:space:]]*[@]ENABLE_XTERM_END[@][[:space:]]*$$/
if !ENABLE_TAIL
XTERM_DELETE = -e '$(XTERM_START),$(XTERM_END) d'
else ENABLE_TAIL
if !ENABLE_XTERM
XTERM_DELETE = -e '$(XTERM_START),$(XTERM_END) d'
else ENABLE_XTERM
XTERM_DELETE = -e '$(XTERM_START) d' -e '$(XTERM_END) d'
endif ENABLE_XTERM
endif ENABLE_TAIL

edit = sed \
	-e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g' \
	-e 's,[@]bindir[@],$(bindir),g' \
	-e 's,[@]guipdf[@],$(guipdf),g' \
	-e 's,[@]dmstartdir[@],$(dmstartdir),g' \
	-e 's,[@]ENABLE_BASH[@],$(ENABLE_BASH),g' \
	-e 's,[@]ENABLE_PERL[@],$(ENABLE_PERL),g' \
	-e 's,[@]ENABLE_XTERM[@],$(ENABLE_XTERM),g' \
	-e 's,[@]ENABLE_TAIL[@],$(ENABLE_TAIL),g' \
	-e 's,[@]ENABLE_ATPS[@],$(ENABLE_ATPS),g' \
	-e 's,[@]ENABLE_LP[@],$(ENABLE_LP),g' \
	-e 's,[@]ENABLE_PSTPDF[@],$(ENABLE_PSTPDF),g' \
	-e 's,[@]ENABLE_PDFTPS[@],$(ENABLE_PDFTPS),g' \
	-e 's,[@]ENABLE_DVIPDF[@],$(ENABLE_DVIPDF),g' \
	-e 's,[@]ENABLE_XDVI[@],$(ENABLE_XDVI),g' \
	-e 's,[@]ENABLE_GS[@],$(ENABLE_GS),g' \
	-e 's,[@]ENABLE_PDFLATEX[@],$(ENABLE_PDFLATEX),g' \
	-e 's,[@]ENABLE_LATEX[@],$(ENABLE_LATEX),g' \
	$(XTERM_DELETE)

if ENABLE_BASH
dnode.sh: Makefile $(srcdir)/dnode.sh.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dnode.sh.in >$@.tmp
	mv $@.tmp $@
endif ENABLE_BASH

if ENABLE_PERL
print.pl: Makefile $(srcdir)/print.pl.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/print.pl.in >$@.tmp
	mv $@.tmp $@
endif ENABLE_PERL

runtool.sh: Makefile $(srcdir)/runtool.sh.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/runtool.sh.in >$@.tmp
	mv $@.tmp $@

