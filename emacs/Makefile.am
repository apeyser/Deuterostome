dist_pkglisp_LISP = dm-server.el
nodist_pkglisp_LISP = d-comint-mode.el d-mode.el init.el
EXTRA_DIST = d-comint-mode.el.in init.el.in d-mode.el.in
CLEANFILES = $(nodist_pkglisp_LISP)

REGEX_START = /^[[:space:]]*[@]ENABLE_REGEX_START[@][[:space:]]*$$/
REGEX_END   = /^[[:space:]]*[@]ENABLE_REGEX_END[@][[:space:]]*$$/

if ENABLE_REGEX
REGEX_EDIT = -e '$(REGEX_START) d' \
			 -e '$(REGEX_END) d' \
			 -e 's,[@]ENABLE_REGEX[@],$(ENABLE_REGEX),g'
else !ENABLE_REGEX
REGEX_EDIT = -e '$(REGEX_START),$(REGEX_END) d'
endif !ENABLE_REGEX

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@pkglispdir\@,$(pkglispdir),g' \
	-e 's,@NAMEBYTES\@,@NAMEBYTES@,g' \
	$(REGEX_EDIT)

d-mode.el: Makefile $(srcdir)/d-mode.el.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

d-comint-mode.el: Makefile $(srcdir)/d-comint-mode.el.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@

init.el: Makefile $(srcdir)/init.el.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/$@.in >$@.tmp
	mv $@.tmp $@


