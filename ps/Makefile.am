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
INFILES = $(nodist_pkgpsdata_DATA)
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
SUFFIXES =

dist_pkgpsdata_DATA = cajal2.img form_1.ps form_2.ps graf_1.ps gray.ps \
	monday.ps struct.ps text.ps thursday_cajal2.ps

nodist_pkgpsdata_DATA = thursday.ps tuesday.ps

edit = -e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g'

include $(top_srcdir)/m4/cf_in.make

