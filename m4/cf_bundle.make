# -*- mode: makefile; -*-

BUNDLE = $(GENTOO_BUNDLE) $(DEBIAN_BUNDLE)

include $(srcdir)/m4/cf_gentoo.make
include $(srcdir)/m4/cf_debian.make

.PHONY: bundle
bundle: $(SVNVERSION_TARGET) $(distdir).tar.bz2

$(distdir).tar.bz2: distdir distsvn
	cp -pRH $(BUNDLE) $(distdir)
	tardir=$(distdir) && $(am__tar) | bzip2 -9 -c >$(distdir).tar.bz2
	$(am__remove_distdir)

BUNDLE_CLEANFILES = $(distdir).tar.bz2

include $(top_srcdir)/m4/cf_bundle_sub.make

