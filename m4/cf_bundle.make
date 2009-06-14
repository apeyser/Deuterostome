# -*- mode: makefile; -*-

BUNDLE = $(GENTOO_BUNDLE) $(DEBIAN_BUNDLE)

include $(top_srcdir)/m4/cf_gentoo.make
include $(top_srcdir)/m4/cf_debian.make

.PHONY: bundle
bundle: $(SVNVERSION_TARGET) $(distdir).tar.bz2

$(distdir).tar.bz2: distdir
	cp -pRH $(BUNDLE) $(distdir)
	list='$(BUNDLE)'; for subdir in $$list ; do ( \
	  cd $(distdir)/$$list && if test -e .git ; then rm -rf .git ; fi; \
	); done
	tardir=$(distdir) && $(am__tar) | bzip2 -9 -c >$(distdir).tar.bz2
	$(am__remove_distdir)

BUNDLE_CLEANFILES = $(distdir).tar.bz2


