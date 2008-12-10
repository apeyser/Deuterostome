dist_pkglisp_LISP = dm-server.el
NODISTFILES = d-comint-mode.el init.el d-mode.el
nodist_pkglisp_LISP = $(NODISTFILES) d-mode-ops.el
EXTRA_DIST = d-comint-mode.el.in init.el.in d-mode.el.in
CLEANFILES = $(NODISTFILES)

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@pkglispdir\@,$(pkglispdir),g' \
	-e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
	-e 's,[@]NAMEBYTES[@],@NAMEBYTES@,g' \
	-e 's,[@]SVNVERSION[@],@SVNVERSION@,g'

d-mode.el: Makefile $(srcdir)/d-mode.el.in @SVNVERSION_STAMP@
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

d-comint-mode.el: Makefile $(srcdir)/d-comint-mode.el.in @SVNVERSION_STAMP@
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

init.el: Makefile $(srcdir)/init.el.in @SVNVERSION_STAMP@
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

include $(top_srcdir)/m4/cf_svnversion.make

