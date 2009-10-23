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

SVNID_STAMP = `cat $(srcdir)/$@in.idstamp 2>/dev/null || echo unknown`
SVNID_EDIT = -e "s,[@]SVNID[@],$(SVNID_STAMP),g"

IDFILES_RULE_BASE = \
	echo "Building idstamp: $$id"; \
	f=`echo $$id | sed 's,.idstamp,,g'` \
	&& t=`echo $$$$.$$id` \
	&& if git log -n 1 --pretty='format:%cn %cd %H' \
	      `readlink $$f || echo $$f` \
		>$$t 2>/dev/null ; then \
	  if test "`cat $$id 2>/dev/null`" = "`cat $$t 2>/dev/null`"; \
	  then \
	    rm $$t || exit 1; \
	  else \
	    mv $$t $$id || exit 1; \
	    touch $$f || exit 1; \
	  fi; \
	else rm $$t; \
	fi


IDFILES_RULE = @id=$@; $(IDFILES_RULE_BASE)

IDFILES_REFRESH = \
	@list="$(IDFILES)"; for id in $$list; do $(IDFILES_RULE_BASE); done

IDFILES = $(INFILES:=in.idstamp)
all-local: $(idfiles)

.PHONY: idfiles

if MAINTAINER_MODE
$(IDFILES): ; $(IDFILES_RULE)
idfiles: ; $(IDFILES_REFRESH)
idfiles = idfiles
endif MAINTAINER_MODE

SVNVERSION_TARGET = svnversion
