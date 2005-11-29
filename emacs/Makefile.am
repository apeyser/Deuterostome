dist_pkglisp_LISP = dm-server.el
nodist_pkglisp_LISP = d-comint-mode.el d-mode.el init.el
EXTRA_DIST = d-comint-mode.el.in init.el.in
CLEANFILES = $(nodist_pkglisp_LISP)

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


