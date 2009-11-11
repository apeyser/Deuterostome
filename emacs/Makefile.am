INFILES = d-comint-mode.el init.el d-mode.el
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
SUFFIXES =

dist_pkglisp_LISP = dm-server-22.el dm-server-23.el
nodist_pkglisp_LISP = $(INFILES) d-mode-ops.el

edit = -e 's,@bindir\@,$(bindir),g' \
       -e 's,@pkglispdir\@,$(pkglispdir),g' \
       -e 's,[@]NAMEBYTES[@],@NAMEBYTES@,g'

include $(top_srcdir)/m4/cf_in.make
