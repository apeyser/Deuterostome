dist_pkgpsdata_DATA = cajal2.img form_1.ps form_2.ps graf_1.ps gray.ps \
	monday.ps struct.ps text.ps thursday_cajal2.ps

nodist_pkgpsdata_DATA = thursday.ps tuesday.ps
EXTRA_DIST = thursday.ps.in tuesday.ps.in
CLEANFILES = $(nodist_pkgpsdata_DATA)

edit = sed -e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g'

thursday.ps: Makefile $(srcdir)/thursday.ps.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/thursday.ps.in >$@.tmp
	mv $@.tmp $@

tuesday.ps: Makefile $(srcdir)/tuesday.ps.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/tuesday.ps.in >$@.tmp
	mv $@.tmp $@
include $(top_srcdir)/m4/cf_bundle_sub.make

