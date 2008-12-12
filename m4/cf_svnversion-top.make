# -*- mode: makefile; -*-

.PHONY: svnversion

svnversion.stamp svnversion: $(top_srcdir)/configure
	$(SHELL) ./config.status --recheck \
	  && ./config.status

EXTRA_DIST += svnversion.stamp

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

dist-hook: svnversion
	cp -p $(builddir)/svnversion.stamp $(distdir)/svnversion.stamp

