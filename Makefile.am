EXTRA_DIST = bootstrap

if EMACS_ENABLED
EMACS_DIR=emacs
endif EMACS_ENABLED

if ENABLE_PLUGINS
PLUGINS_DIR=plugins
endif ENABLE_PLUGINS

if BASH_ENABLED
if ENV_ENABLED
BASH_DIR=sh
endif ENV_ENABLED
endif BASH_ENABLED

SUBDIRS = libltdl src $(PLUGINS_DIR) $(BASH_DIR) ps $(EMACS_DIR) dcode
ACLOCAL_AMFLAGS = -I m4

@SET_MAKE@
codegen-all: codegen all
codegen: 
	cd src/codegen && $(MAKE) $(AM_MAKEFLAGS) all