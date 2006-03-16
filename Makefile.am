EXTRA_DIST = bootstrap autom4te.cfg

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

