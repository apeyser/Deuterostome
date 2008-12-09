DEBIAN_FILES = \
	debian/changelog debian/docs debian/menu.ex \
	debian/compat debian/emacsen-install.ex debian/postinst.ex \
	debian/control debian/emacsen-remove.ex debian/postrm.ex \
	debian/copyright debian/emacsen-startup.ex debian/preinst.ex \
	debian/cron.d.ex debian/init.d.ex debian/prerm.ex \
	debian/dirs debian/manpage.1.ex debian/README.Debian \
	debian/dm-default.ex debian/manpage.sgml.ex debian/rules \
	debian/dm.doc-base.EX debian/manpage.xml.ex debian/watch.ex

GENTOO_FILES = \
	gentoo/dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild \
	gentoo/dev-lang/dm/Manifest \
	gentoo/dev-lang/dm/files/50dm-gentoo.el \
	gentoo/dev-lang/dm/files/dnoded.conf \
	gentoo/dev-lang/dm/files/dnoded.rc6


EXTRA_DIST = bootstrap autom4te.cfg \
	gnulib-tool-import.sh link-warning.h \
	$(DEBIAN_FILES) $(GENTOO_FILES)

if ENABLE_EMACS
EMACS_DIR=emacs
endif ENABLE_EMACS

if ENABLE_BASH
if ENABLE_ENV
BASH_DIR=sh
endif ENABLE_ENV
endif ENABLE_BASH

if ENABLE_DOC
DOC_DIR=doc
endif ENABLE_DOC

SUBDIRS = libltdl lib src plugins $(BASH_DIR) ps \
	$(EMACS_DIR) dcode test \
	m4 $(DOC_DIR)
ACLOCAL_AMFLAGS = -I m4 -I m4/latex
DISTCHECK_CONFIGURE_FLAGS = --enable-plugins --enable-daemon
#--disable-process

@MAKE_SVNVERSION@

gentoo/dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild:
	svn mv gentoo/dev-lang/dm/dm-*.ebuild $@

.PHONY: update-version
update-version: gentoo/dev-lang/dm/dm-@PACKAGE_VERSION@.ebuild

.PHONY: gentoo-setup
gentoo-setup: rsync
	find $(srcdir)/gentoo -name "*~" -print0 | xargs -r0 rm
	( \
		cd $(srcdir)/gentoo ; \
		for i in dev-lang/dm/*.ebuild; do \
			ebuild --force $$i digest || exit 1; \
		done \
	)

.PHONY: rsync
rsync: dist
	scp $(DIST_ARCHIVES) root@gentoo:/usr/local/portage/distfiles/
	source /etc/make.conf \
	&& if test -z "$$DISTDIR" ; then DISTDIR=/usr/portage/distfiles; fi \
	&& if test -e $$DISTDIR/$(DIST_ARCHIVES); then \
		sudo rm $$DISTDIR/$(DIST_ARCHIVES); \
	fi

.PHONY: ebuild
ebuild: update-version gentoo-setup
	svn ci -m '' gentoo
	sudo layman -s gentoo-local

gencode-all: gencode all

.PHONY: gencode
gencode: 
	cd libltdl && $(MAKE) $(AM_MAKEFLAGS) all
	cd src && $(MAKE) $(AM_MAKEFLAGS) gencode
