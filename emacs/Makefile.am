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
INFILES = d-comint-mode.el init.el d-mode.el
BUILT_SOURCES = dm-server.el $(INFILES)
EXTRA_DIST = $(INFILES:=in) $(IDFILES) dm-server-source.el
CLEANFILES = $(BUILT_SOURCES) dm-server-script
SUFFIXES =

dm-server.el: dm-server-source.el
	echo "(setq output-file-name \"$@\") \
	      (setq load-path (cons nil load-path)) \
	      (setq load-path (cons \"$(srcdir)\" load-path)) \
	      (setq gunzip \"$(gunzip)\") \
	      (load \"$<\" nil nil t)" > dm-server-script
	$(EMACS) -batch -q -l dm-server-script -f dm-server-build

nodist_pkglisp_LISP = $(INFILES) d-mode-ops.el dm-server.el

edit = -e 's,[@]bindir[@],$(bindir),g' \
       -e 's,[@]pkglispdir[@],$(pkglispdir),g' \
       -e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g' \
       -e 's,[@]dmstartdir[@],$(dmstartdir),g' \
       -e 's,[@]pkglispdir[@],$(pkglispdir),g' \
       -e 's,[@]dmconfdir[@],$(dmconfdir),g' \
       -e 's,[@]pkglibexecdir[@],$(pkglibexecdir),g' \
       -e 's,[@]NAMEBYTES[@],@NAMEBYTES@,g'

include $(top_srcdir)/m4/cf_in.make
