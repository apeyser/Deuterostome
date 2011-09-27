# Copyright 2011 Alexander Peyser & Wolfgang Nonner
#
# This file is part of Deuterostome.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY# without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
INFILES = dnode.sh print.pl runtool.sh emacsclient-run.sh
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
SUFFIXES =

clean-local: ; -rm -f $(dmstart_SCRIPTS)

nodist_dmstart_SCRIPTS = runtool.sh
if ENABLE_BASH
nodist_dmstart_SCRIPTS += dnode.sh
endif
if ENABLE_PERL
nodist_dmstart_SCRIPTS += print.pl
endif
if ENABLE_EMACS
if ENABLE_EMACSCLIENT
nodist_dmstart_SCRIPTS += emacsclient-run.sh
endif ENABLE_EMACSCLIENT
endif ENABLE_EMACS

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

edit = -e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g' \
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
       -e 's,[@]ENABLE_EPSTPDF[@],$(ENABLE_EPSTPDF),g' \
       -e 's,[@]ENABLE_PDFTPS[@],$(ENABLE_PDFTPS),g' \
       -e 's,[@]ENABLE_DVIPDF[@],$(ENABLE_DVIPDF),g' \
       -e 's,[@]ENABLE_XDVI[@],$(ENABLE_XDVI),g' \
       -e 's,[@]ENABLE_GS[@],$(ENABLE_GS),g' \
       -e 's,[@]ENABLE_PDFLATEX[@],$(ENABLE_PDFLATEX),g' \
       -e 's,[@]ENABLE_LATEX[@],$(ENABLE_LATEX),g' \
       -e 's,[@]ENABLE_EMACSCLIENT[@],$(ENABLE_EMACSCLIENT),g' \
       $(XTERM_DELETE)

include $(top_srcdir)/m4/cf_in.make
