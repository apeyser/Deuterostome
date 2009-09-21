# -*- mode: makefile; -*-

PDEBUILD=': $${TMP=/tmp} \
	&& DESTTOP="$$TMP/dm-$$$$" \
	&& DEST="$$DESTTOP/dm-$(PACKAGE_VERSION)" \
	&& echo "Building in $$DEST" \
	&& $(MKDIR_P) "$$DESTTOP" \
	&& cp -prf "$(srcdir)" "$$DEST" \
	&& cd "$$DEST" \
	&& $(SED) -e \'s/[@]VERSION[@]/@VERSION@/g\' \
	          -e "s/[@]DATE[@]/`date -R`/g" \
	          debian/changelog.in >debian/changelog \
	&& $(SED) -e \'s/[@]ATLAS_VERSION[@]/$$ATLAS_VERSION/g\' \
	          debian/control.in >debian/control \
	&& pdebuild --buildresult "$(abs_builddir)/.." \
	&& cd "$(abs_builddir)" \
	&& rm -rf "$$DEST"'

.PHONY: debian
debian: 
	ATLAS_VERSION=''; \
	$(PDEBUILD)


.PHONY: ubuntu
ubuntu:
	ATLAS_VERSION='(>= 3.6.0-22ubuntu2)'; \
	$(PDEBUILD)

DEBIAN_BUNDLE = debian
