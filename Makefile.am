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

if ENABLE_UTILS
UTILS_DIR=utils
endif ENABLE_UTILS

SUBDIRS = libltdl src $(PLUGINS_DIR) $(BASH_DIR) ps $(EMACS_DIR) dcode \
	$(UTILS_DIR)
ACLOCAL_AMFLAGS = -I m4

@SET_MAKE@
gencode-all: gencode all
gencode: 
	cd src && $(MAKE) $(AM_MAKEFLAGS) gencode

