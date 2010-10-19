INFILES = d-comint-mode.el init.el d-mode.el
BUILT_SOURCES = dm-server.el $(INFILES)
EXTRA_DIST = $(INFILES:=in) $(IDFILES) dm-server-source.el
CLEANFILES = $(BUILT_SOURCES) dm-server-script
SUFFIXES =

dm-server.el: dm-server-source.el
	echo "(setq output-file-name \"$@\") \
	      (setq load-path (cons nil load-path)) \
	      (setq load-path (cons \"$(srcdir)\" load-path)) \
	      (setq gunzip \"$(gunzip)\") \
	      (load \"$<\" nil nil t)" > dm-server-script
	$(EMACS) -batch -q -l dm-server-script -f dm-server-build

nodist_pkglisp_LISP = $(INFILES) d-mode-ops.el dm-server.el

edit = -e 's,[@]bindir[@],$(bindir),g' \
       -e 's,[@]pkglispdir[@],$(pkglispdir),g' \
       -e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g' \
       -e 's,[@]dmstartdir[@],$(dmstartdir),g' \
       -e 's,[@]pkglispdir[@],$(pkglispdir),g' \
       -e 's,[@]dmconfdir[@],$(dmconfdir),g' \
       -e 's,[@]pkglibexecdir[@],$(pkglibexecdir),g' \
       -e 's,[@]NAMEBYTES[@],@NAMEBYTES@,g'

include $(top_srcdir)/m4/cf_in.make
