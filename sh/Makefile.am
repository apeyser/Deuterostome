nodist_dmstart_SCRIPTS = dnode.sh print_.sh print.sh
EXTRA_DIST = dnode.sh.in print_.sh.in print.sh.in
CLEANFILES = $(nodist_dmstart_SCRIPTS)

clean-local:
	-rm -f $(dmstart_SCRIPTS)

edit = sed \
	-e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g' \
	-e 's,[@]bindir[@],$(bindir),g' \
	-e 's,[@]guipdf[@],$(guipdf),g' \
	-e 's,[@]dmstartdir[@],$(dmstartdir),g'

dnode.sh: Makefile $(srcdir)/dnode.sh.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dnode.sh.in >$@.tmp
	mv $@.tmp $@

print_.sh: Makefile $(srcdir)/print_.sh.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/print_.sh.in >$@.tmp
	mv $@.tmp $@

print.sh: Makefile $(srcdir)/print.sh.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/print.sh.in >$@.tmp
	mv $@.tmp $@
