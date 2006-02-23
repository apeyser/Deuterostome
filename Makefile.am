EXTRA_DIST = bootstrap

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

SUBDIRS = libltdl src $(PLUGINS_DIR) $(BASH_DIR) ps $(EMACS_DIR) dcode
ACLOCAL_AMFLAGS = -I m4
DISTCHECK_CONFIGURE_FLAGS = --disable-process

@SET_MAKE@
gencode-all: gencode all
gencode: 
	cd src && $(MAKE) $(AM_MAKEFLAGS) gencode

