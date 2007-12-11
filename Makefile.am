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
	gentoo/dev-lang/dm/files/digest-dm-4.0

EXTRA_DIST = bootstrap autom4te.cfg $(DEBIAN_FILES) $(GENTOO_FILES)

if ENABLE_EMACS
EMACS_DIR=emacs
endif ENABLE_EMACS

if ENABLE_PLUGINS
PLUGINS_DIR=plugins
endif ENABLE_PLUGINS

if ENABLE_BASH
if ENABLE_ENV
BASH_DIR=sh
endif ENABLE_ENV
endif ENABLE_BASH

SUBDIRS = libltdl src $(PLUGINS_DIR) $(BASH_DIR) ps $(EMACS_DIR) dcode test \
	m4
ACLOCAL_AMFLAGS = -I m4
DISTCHECK_CONFIGURE_FLAGS = 
#--disable-process


gencode-all: gencode all
gencode: 
	cd libltdl && $(MAKE) $(AM_MAKEFLAGS) all
	cd src && $(MAKE) $(AM_MAKEFLAGS) gencode

