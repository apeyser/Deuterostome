dist_pkglisp_LISP = dm-server.el
NODISTFILES = d-comint-mode.el init.el 
nodist_pkglisp_LISP = $(NODISTFILES) d-mode-ops.el d-mode.el
EXTRA_DIST = d-comint-mode.el.in init.el.in d-mode.el.in
CLEANFILES = $(NODISTFILES)

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@pkglispdir\@,$(pkglispdir),g'

d-comint-mode.el: Makefile $(srcdir)/d-comint-mode.el.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

init.el: Makefile $(srcdir)/init.el.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@


