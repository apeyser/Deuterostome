# -*- mode: makefile; -*-

include $(top_srcdir)/m4/cf_svnversion.make

Makefile = Makefile
SVNVERSION_STAMP_DEP = @SVNVERSION_STAMP@

SUFFIXES += .cin .hin .elin .din .psin .shin .plin .styin

PACKAGE_EDIT = -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g'

INFILES_RULE = \
	! test -e $@.tmp || rm -f $@.tmp \
	&& sed $(edit) \
	       $(SVNVERSION_EDIT) \
	       $(SVNID_EDIT) \
	       $(PACKAGE_EDIT) \
	       $< >$@.tmp \
	&& mv $@.tmp $@

.cin.c:     ; $(INFILES_RULE)
.hin.h:     ; $(INFILES_RULE)
.din.d:     ; $(INFILES_RULE)
.psin.ps:   ; $(INFILES_RULE)
.shin.sh:   ; $(INFILES_RULE)
.plin.pl:   ; $(INFILES_RULE)
.elin.el:   ; $(INFILES_RULE)
.styin.sty: ; $(INFILES_RULE)

$(INFILES): $(IDFILES) $(Makefile) $(SVNVERSION_STAMP_DEP)
