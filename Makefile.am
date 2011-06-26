EXTRA_DIST = bootstrap autom4te.cfg \
	fink/dm.el fink/dm.info \
	testboxes/dict.box testboxes/list.box testboxes/longarray.box

CLEANFILES = $(BUNDLE_CLEANFILES)

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

SUBDIRS = libltdl lib src dcode plugins $(BASH_DIR) ps \
	$(EMACS_DIR) test \
	m4 $(DOC_DIR)
ACLOCAL_AMFLAGS = -I m4 -I m4/latex
DISTCHECK_CONFIGURE_FLAGS = --enable-plugins --enable-daemon
#--disable-process

gencode-all: gencode all

.PHONY: gencode
gencode: 
	cd libltdl && $(MAKE) $(AM_MAKEFLAGS) all
	cd lib && $(MAKE) $(AM_MAKEFLAGS) all
	cd src && $(MAKE) $(AM_MAKEFLAGS) gencode

libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

include $(srcdir)/m4/cf_bundle.make
