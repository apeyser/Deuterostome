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

IDFILES_RULE = 	\
	list='$(INFILES:=in)'; (for f in $$list ; do \
	  if test -e $$f.idstamp ; then \
	    if git log -n 1 --pretty='format:%cn %cd %H' \
	      `readlink $$f || echo $$f` >$$f.idstamp.tmp 2>/dev/null ; then\
	      if test "`cat $$f.idstamp`" = "`cat $$f.idstamp.tmp`"; then \
		rm $$f.idstamp.tmp || exit 1; \
	      else \
		mv $$f.idstamp.tmp $$f.idstamp || exit 1; \
		touch $$f || exit 1; \
	      fi; \
	    else rm $$f.idstamp.tmp; \
	    fi; \
	  fi; \
	done)

IDFILES = $(INFILES:=in.idstamp)

.PHONY: idfiles
idfiles: ; $(IDFILES_RULE)

if MAINTAINER_MODE
IDFILES_TARGET = idfiles
else !MAINTAINER_MODE
IDFILES_TARGET = 
endif !MAINTAINER_MODE

SVNVERSION_TARGET = svnversion
