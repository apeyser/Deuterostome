# -*- mode: makefile; -*-

if MAINTAINER_MODE
SVNVERSION_DIR = $(top_srcdir)
else !MAINTAINER_MODE
SVNVERSION_DIR = $(top_builddir)
endif !MAINTAINER_MODE

$(SVNVERSION_DIR)/svnversion.stamp:
	cd $(SVNVERSION_DIR) && $(MAKE) $(AM_MAKEFLAGS) svnversion.stamp

SVNVERSION_STAMP = `cat $(SVNVERSION_DIR)/svnversion.stamp`
SVNVERSION_EDIT = -e "s,[@]SVNVERSION[@],$(SVNVERSION_STAMP),g"

SVNID_STAMP = `cat $(srcdir)/$@in.idstamp || echo unknown`
SVNID_EDIT = -e "s,[@]SVNID[@],$(SVNID_STAMP),g"

IDFILES_RULE = \
	@echo "Building idstamp: $@"; \
	f=`echo $@ | sed 's,.idstamp,,g'` \
	&& if git log -n 1 --pretty='format:%cn %cd %H' \
	      `readlink $$f || echo $$f` \
		>$@.tmp 2>/dev/null ; then \
	  if test "`cat $@`" = "`cat $@.tmp`"; then \
	    rm $@.tmp || exit 1; \
	  else \
	    mv $@.tmp $@ || exit 1; \
	    touch $$f || exit 1; \
	  fi; \
	else rm $@.tmp; \
	fi;

IDFILES = $(INFILES:=in.idstamp)
all-local: $(idfiles)

if MAINTAINER_MODE
$(IDFILES): ; $(IDFILES_RULE)
idfiles: $(IDFILES)
idfiles = idfiles
endif MAINTAINER_MODE

SVNVERSION_TARGET = svnversion
