# -*- mode: makefile; -*-

if MAINTAINER_MODE
$(top_srcdir)/svnversion.stamp:
	cd $(top_srcdir) && $(MAKE) $(AM_MAKEFLAGS) svnversion.stamp
else !MAINTAINER_MODE
$(top_builddir)/svnversion.stamp:
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) svnversion.stamp
endif !MAINTAINER_MODE

SVNVERSION_EDIT = \
	-e "s,[@]SVNVERSION[@],`cat $(top_builddir)/svnversion.stamp`,g"

