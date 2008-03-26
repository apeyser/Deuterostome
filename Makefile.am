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
	gentoo/dev-lang/dm/dm-4.0.ebuild \
	gentoo/dev-lang/dm/Manifest \
	gentoo/dev-lang/dm/files/50dm-gentoo.el \
	gentoo/dev-lang/dm/files/digest-dm-4.0 \
	gentoo/dev-lang/dm/files/dnoded.conf \
	gentoo/dev-lang/dm/files/dnoded.rc6


EXTRA_DIST = bootstrap autom4te.cfg $(DEBIAN_FILES) $(GENTOO_FILES)

if ENABLE_EMACS
EMACS_DIR=emacs
endif ENABLE_EMACS

if ENABLE_BASH
if ENABLE_ENV
BASH_DIR=sh
endif ENABLE_ENV
endif ENABLE_BASH

SUBDIRS = libltdl src plugins $(BASH_DIR) ps \
	$(EMACS_DIR) dcode test \
	m4
ACLOCAL_AMFLAGS = -I m4
DISTCHECK_CONFIGURE_FLAGS = --enable-plugins --enable-daemon
#--disable-process

.PHONY: gentoo-setup
gentoo-setup:
	find $(srcdir)/gentoo -name "*~" -print0 | xargs -r0 rm
	( \
		cd $(srcdir)/gentoo ; \
		for i in dev-lang/dm/*.ebuild; do \
			ebuild --force $$i digest || exit 1; \
		done \
	)

DISTPKG = dm-4.0.tar.gz 
.PHONY: rsync
rsync: dist
	rsync $(DISTPKG) root@deli1:/usr/local/portage/distfiles/
	! test -e /usr/portage/distfiles/$(DISTPKG) \
		|| sudo rm /usr/portage/distfiles/$(DISTPKG)

.PHONY: ebuild
ebuild: rsync gentoo-setup
	svn ci -m ''

gencode-all: gencode all
.PHONY: gencode
gencode: 
	cd libltdl && $(MAKE) $(AM_MAKEFLAGS) all
	cd src && $(MAKE) $(AM_MAKEFLAGS) gencode

