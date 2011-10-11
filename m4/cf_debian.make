# -*- mode: makefile; -*-

PDEBUILD_NAME="build"

PDEBUILD_VARS = \
	: $${TMP=/tmp}; \
	PDEBBUILD_DESTTOP="$$TMP/dm-$(PDEBUILD_NAME)"; \
	PDEBBUILD_DEST="$$PDEBBUILD_DESTTOP/dm-$(PACKAGE_VERSION)"

PDEBUILD_SETUP = \
	$(PDEBUILD_VARS); \
	echo "Initializing in $$PDEBBUILD_DEST"; \
	rm -rf "$$PDEBUILD_DESTTOP"; \
	$(MKDIR_P) "$$PDEBBUILD_DESTTOP" \
	&& cp -prf "$(srcdir)" "$$PDEBBUILD_DEST" \
	&& cd "$$PDEBBUILD_DEST"

PDEBUILD_SED = \
	$(PDEBUILD_VARS); \
	echo "Updating files in $$PDEBBUILD_DEST"; \
	cd "$$PDEBBUILD_DEST" \
	&& $(SED) -e 's/[@]VERSION[@]/@VERSION@/g'\
	          -e "s/[@]DATE[@]/`date -R`/g" \
	          debian/changelog.in >debian/changelog \
	&& $(SED) -e "s/[@]ATLAS_VERSION[@]/$$ATLAS_VERSION/g" \
		  -e "s/[@]EMACS_VERSION[@]/$$EMACS_VERSION/g" \
		  -e "s/[@]LIBATLAS_HEADERS[@]/$$LIBATLAS_HEADERS/g" \
	          debian/control.in >debian/control 

PDEBUILD_ACTION = \
	$(PDEBUILD_VARS); \
	echo "Building in $$PDEBBUILD_DEST"; \
	cd "$$PDEBBUILD_DEST" \
	&& pdebuild --buildresult "$(abs_builddir)/.."

PDEBUILD_CLEANUP = \
	$(PDEBUILD_VARS); \
	echo "Cleanup up $$PDEBBUILD_DEST"; \
	rm -rf "$$PDEBBUILD_DEST"

.PHONY: debian-setup
debian-setup:
	@$(PDEBUILD_SETUP)

.PHONY: debian-sed
debian-sed:
	@: $${ATLAS_VERSION=''}; \
	: $${EMACS_VERSION=23}; \
	: $${LIBATLAS_HEADERS=libatlas-dev}; \
	$(PDEBUILD_SED)

.PHONY: debian-action
debian-action:
	@$(PDEBUILD_ACTION)

.PHONY: debian-cleanup
debian-cleanup:
	@$(PDEBUILD_CLEANUP)

.PHONY: debian-init
debian-init: debian-setup debian-sed

.PHONY: debian-fin
debian-fin: debian-action debian-cleanup

.PHONY: debian-int
debian-int: debian-init debian-fin

.PHONY: debian
debian:
	$(MAKE) $(AM_MAKEFLAGS) PDEBUILD_NAME="$$$$" debian-int

.PHONY: ubuntu-sed
ubuntu-sed:
	@: $${ATLAS_VERSION='(>= 3.6.0-22ubuntu2)'}; \
	: $${EMACS_VERSION=23}; \
	: $${LIBATLAS_HEADERS=libatlas-headers}; \
	$(PDEBUILD_SED)

.PHONY: ubuntu-init
ubuntu-init: debian-setup ubuntu-sed

.PHONY: ubuntu-int
ubuntu-int: ubuntu-init debian-fin

.PHONY: ubuntu
ubuntu:
	$(MAKE) $(AM_MAKEFLAGS) PDEBUILD_NAME="$$$$" ubuntu-int

DEBIAN_BUNDLE = debian
