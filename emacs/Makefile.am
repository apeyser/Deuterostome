INFILES = d-comint-mode.el init.el d-mode.el
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
SUFFIXES =

dist_pkglisp_LISP = dm-server.el
nodist_pkglisp_LISP = $(INFILES) d-mode-ops.el

edit = -e 's,@bindir\@,$(bindir),g' \
       -e 's,@pkglispdir\@,$(pkglispdir),g' \
       -e 's,[@]NAMEBYTES[@],@NAMEBYTES@,g'

include $(top_srcdir)/m4/cf_in.make
