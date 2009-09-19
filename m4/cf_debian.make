# -*- mode: makefile; -*-

.PHONY: debian
debian: 
	@: $${TMP=/tmp} \
	&& DESTTOP="$$TMP/dm-$$$$" \
	&& DEST="$$DESTTOP/dm-$(PACKAGE_VERSION)" \
	&& echo "Building in $$DEST" \
	&& $(MKDIR_P) "$$DESTTOP" \
	&& cp -prf "$(srcdir)" "$$DEST" \
	&& cd "$$DEST" \
	&& $(SED) -e 's/[@]VERSION[@]/@VERSION@/g' \
	          -e "s/[@]DATE[@]/`date -R`/g" \
	          debian/changelog.in >debian/changelog \
	&& pdebuild --buildresult "$(abs_builddir)/.." \
	&& cd "$(abs_builddir)" \
	&& rm -rf "$$DEST"

DEBIAN_BUNDLE = debian
