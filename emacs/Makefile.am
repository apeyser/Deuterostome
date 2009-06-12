INFILES = d-comint-mode.el init.el d-mode.el
EXTRA_DIST = $(INFILES:=in)
CLEANFILES = $(INFILES)
SUFFIXES =

dist_pkglisp_LISP = dm-server.el
nodist_pkglisp_LISP = $(INFILES) d-mode-ops.el

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@pkglispdir\@,$(pkglispdir),g' \
	-e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
	-e 's,[@]NAMEBYTES[@],@NAMEBYTES@,g' \
	$(SVNVERSION_EDIT)

include $(top_srcdir)/m4/cf_svnversion.make
include $(top_srcdir)/m4/cf_bundle_sub.make
include $(top_srcdir)/m4/cf_in.make
