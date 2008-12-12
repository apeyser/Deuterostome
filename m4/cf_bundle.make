# -*- mode: makefile; -*-

bundle =

bundle: distdir
	cp -aH $(bundle) $(distdir)
	tardir=$(distdir) && $(am__tar) | bzip2 -9 -c >$(distdir).tar.bz2
	$(am__remove_distdir)
