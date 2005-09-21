dist_dmstart_DATA = color.d dvt_macros.d master.d sched.d startup_dnode.d \
	startup_dvt.d
nodist_dmstart_DATA =  theeye.d
EXTRA_DIST = theeye.d.in
CLEANFILES = $(nodist_dmstart_DATA)

edit = sed -e 's,[@]pkglibdir[@],$(pkglibdir),g'

theeye.d: Makefile $(srcdir)/theeye.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/theeye.d.in >$@.tmp
	mv $@.tmp $@
