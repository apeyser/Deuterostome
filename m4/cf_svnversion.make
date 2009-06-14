# -*- mode: makefile; -*-

if MAINTAINER_MODE
SVNVERSION_DIR = $(top_srcdir)
else !MAINTAINER_MODE
SVNVERSION_DIR = $(top_builddir)
endif !MAINTAINER_MODE

$(SVNVERSION_DIR)/svnversion.stamp:
	cd $(SVNVERSION_DIR) && $(MAKE) $(AM_MAKEFLAGS) svnversion.stamp

SVNVERSION_STAMP = `cat $(SVNVERSION_DIR)/svnversion.stamp`
SVNVERSION_EDIT = -e "s,[@]SVNVERSION[@],$(SVNVERSION_STAMP),g"

SVNID_STAMP = `git log -n 1 --pretty='format:%cn %cd %H' $<`
SVNID_EDIT = -e "s,[@]SVNID[@],$(SVNID_STAMP),g"

SVNVERSION_TARGET = svnversion
