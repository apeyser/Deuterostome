# -*- mode: makefile; -*-

SVNVERSION_TARGET = svnversion
.PHONY: $(SVNVERSION_TARGET)
svnversion.stamp $(SVNVERSION_TARGET): $(top_srcdir)/configure
	$(SHELL) ./config.status svnversion.stamp

SVNVERSION_EXTRA_DIST = svnversion.stamp

distclean-local: distclean-local-svnversion

.PHONY: distclean-local-svnversion
distclean-local-svnversion:
	if test `cd "$(top_srcdir)" && pwd -P` \
	   	!= `cd "$(top_builddir)" && pwd -P` ; then \
	  if cd "$(top_builddir)" ; then \
	    rm svnversion.stamp || : ; \
	  else false; \
	  fi ; \
	else : ; \
	fi

dist-hook: $(SVNVERSION_TARGET)
	cp -p $(builddir)/svnversion.stamp $(distdir)/svnversion.stamp

