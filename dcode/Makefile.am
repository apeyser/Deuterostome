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
dist_dmstart_DATA = color.d master.d sched.d plugin.d \
	startup_libs.d startup_dgen.d \
	linkedlist.d eps.d compose.d startup_common.d \
	theeye.d startup_dnode.d startup_dpawn.d \
	startup_dvt.d dvt_macros.d matrix.d new-plugin.d \
	processes.d

nodist_dmstart_DATA = startup_common_in.d errors.d

dist_dmconf_DATA = dvt.d dpawn.d dnode.d

INFILES = startup_common_in.d
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
SUFFIXES =

if ENABLE_XPS
XPS_EDIT = -e 's,[@]ENABLE_XPS[@],true,g'
else !ENABLE_XPS
XPS_EDIT = -e 's,[@]ENABLE_XPS[@],false,g'
endif !ENABLE_XPS

if ENABLE_PERL
PERL_EDIT = -e 's,[@]ENABLE_PERL[@],true,g'
else !ENABLE_PERL
PERL_EDIT = -e 's,[@]ENABLE_PERL[@],false,g'
endif !ENABLE_PERL

if ENABLE_REGEX
REGEX_EDIT = -e 's,[@]ENABLE_REGEX[@],true,g'
else !ENABLE_REGEX
REGEX_EDIT = -e 's,[@]ENABLE_REGEX[@],false,g'
endif !ENABLE_REGEX

if ENABLE_PLUGINS_SUPPORT
PLUGINS_SUPPORT_EDIT = -e 's,[@]ENABLE_PLUGINS_SUPPORT[@],true,g'
else !ENABLE_PLUGINS_SUPPORT
PLUGINS_SUPPORT_EDIT = -e 's,[@]ENABLE_PLUGINS_SUPPORT[@],false,g'
endif !ENABLE_PLUGINS_SUPPORT

if ENABLE_THREADS
THREADS_EDIT = -e 's,[@]ENABLE_THREADS[@],true,g' \
	       -e 's,[@]NUMTHREADS[@],$(ENABLE_THREADS),g'
else !ENABLE_THREADS
THREADS_EDIT = -e 's,[@]ENABLE_THREADS[@],false,g' \
	       -e 's,[@]NUMTHREADS[@],1,g'
endif !ENABLE_THREADS

if ENABLE_EMACS
if ENABLE_EMACSCLIENT
EMACSCLIENT_EDIT = -e 's,[@]ENABLE_EMACSCLIENT[@],true,g'
else !ENABLE_EMACSCLIENT
EMACSCLIENT_EDIT = -e 's,[@]ENABLE_EMACSCLIENT[@],false,g'
endif !ENABLE_EMACSCLIENT
else !ENABLE_EMACS
EMACSCLIENT_EDIT = -e 's,[@]ENABLE_EMACSCLIENT[@],false,g'
endif !ENABLE_EMACS

if ENABLE_ATLAS
ATLAS_EDIT = -e 's,[@]ENABLE_ATLAS[@],true,g'
else !ENABLE_ATLAS
ATLAS_EDIT = -e 's,[@]ENABLE_ATLAS[@],false,g'
endif !ENABLE_ATLAS

if ENABLE_RTHREADS
RTHREADS_EDIT = -e 's,[@]ENABLE_RTHREADS[@],true,g'
else !ENABLE_RTHREADS
RTHREADS_EDIT = -e 's,[@]ENABLE_RTHREADS[@],false,g'
endif !ENABLE_RTHREADS

PETSC_EDIT_ON = -e 's,[@]ENABLE_PETSC[@],true,g'
PETSC_EDIT_OFF = -e 's,[@]ENABLE_PETSC[@],false,g'

if !ENABLE_PLUGINS_SUPPORT
PETSC_EDIT = $(PETSC_EDIT_OFF)
else ENABLE_PLUGINS_SUPPORT
if !ENABLE_PLUGINS
PETSC_EDIT = $(PETSC_EDIT_OFF)
else ENABLE_PLUGINS
if !ENABLE_PETSC
PETSC_EDIT = $(PETSC_EDIT_OFF)
else ENABLE_PETSC
PETSC_EDIT = $(PETSC_EDIT_ON)
endif ENABLE_PETSC
endif ENABLE_PLUGINS
endif ENABLE_PLUGINS_SUPPORT

if ENABLE_SEM
SEM_EDIT = -e 's,[@]ENABLE_SEM[@],true,g'
else !ENABLE_SEM
SEM_EDIT = -e 's,[@]ENABLE_SEM[@],false,g'
endif !ENABLE_SEM

PROGS_EDIT = -e 's,[@]GS[@],$(ENABLE_GS),g' \
	     -e 's,[@]PDFLATEX[@],$(ENABLE_PDFLATEX),g' \
	     -e 's,[@]LATEX[@],$(ENABLE_LATEX),g' \
	     -e 's,[@]EPSTOPDF[@],$(ENABLE_EPSTPDF),g' \
	     -e 's,[@]PDFCROP[@],$(ENABLE_PDFCROP),g' \
	     -e 's,[@]PDFTOPS[@],$(ENABLE_PDFTOPS),g' \
	     -e 's,[@]SED[@],$(SED),g'

edit = -e 's,[@]pkglibdir[@],$(pkglibdir),g' \
       -e 's,[@]bindir[@],$(bindir),g' \
       -e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g' \
       -e 's,[@]BASH[@],$(ENABLE_BASH),g' \
       $(PLUGINS_DELETE) \
       $(PLUGINS_SUPPORT_EDIT) \
       $(THREADS_EDIT) \
       $(XPS_EDIT) \
       $(PERL_EDIT) \
       $(EMACSCLIENT_EDIT) \
       $(ATLAS_EDIT) \
       $(RTHREADS_EDIT) \
       $(REGEX_EDIT) \
       $(PETSC_EDIT) \
       $(SEM_EDIT) \
       $(PROGS_EDIT)

include $(top_srcdir)/m4/cf_in.make
