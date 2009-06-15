# -*- mode: makefile; -*-

gentoo/dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild:
	cd gentoo \
	&& git checkout master \
	&& cp `echo dev-lang/dm/dm-*.ebuild | sed -e 's/[ \t]+/\n/g' \
	        | sort | tail -n 1` dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild \
	&& git add dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild

.PHONY: update-version
update-version: gentoo/dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild

.PHONY: gentoo-setup
gentoo-setup: update-version
	find $(srcdir)/gentoo -name "*~" -print0 | xargs -r0 rm
	( \
		cd $(srcdir)/gentoo ; \
		for i in dev-lang/dm/*.ebuild; do \
			ebuild --force $$i digest || exit 1; \
		done \
	)


.PHONY: rsync
rsync: dist
	ssh -t gentoo sudo -v \
	  && rsync $(DIST_ARCHIVES) \
		--rsync-path='sudo rsync' \
		gentoo:/usr/local/portage/distfiles/
	source /etc/make.conf \
	  && if test -z "$$DISTDIR" ; then DISTDIR=/usr/portage/distfiles; fi \
	  && if test -e $$DISTDIR/$(DIST_ARCHIVES); then \
		sudo rm $$DISTDIR/$(DIST_ARCHIVES); \
	fi

.PHONY: gentoo-ci
gentoo-ci: gentoo-setup
	cd gentoo \
	&& git checkout master \
	&& git commit -a -m 'Update ebuilds' && git push

.PHONY: ebuild
ebuild: rsync gentoo-ci
	sudo layman -s gentoo-local

GENTOO_BUNDLE = gentoo
