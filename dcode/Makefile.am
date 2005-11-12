dist_dmstart_DATA = color.d dvt_macros.d master.d sched.d startup_dnode.d \
	startup_dvt.d
nodist_dmstart_DATA =  theeye.d
EXTRA_DIST = theeye.d.in
CLEANFILES = $(nodist_dmstart_DATA)

PLUGIN_START = /^[[:space:]]*[@]ENABLE_PLUGINS_START[@][[:space:]]*$$/
PLUGIN_END = /^[[:space:]]*[@]ENABLE_PLUGINS_END[@][[:space:]]*$$/
if ENABLE_PLUGINS
PLUGIN_DELETE = -e '$(PLUGIN_START) d' -e '$(PLUGIN_END) d'
else !ENABLE_PLUGINS
PLUGIN_DELETE = -e '$(PLUGIN_START),$(PLUGIN_END) d'
endif !ENABLE_PLUGINS

THREADS_START = /^[[:space:]]*[@]ENABLE_THREADS_START[@][[:space:]]*$$/
THREADS_END   = /^[[:space:]]*[@]ENABLE_THREADS_END[@][[:space:]]*$$/

if ENABLE_THREADS
THREADS_EDIT = -e '$(THREADS_START) d' \
			   -e '$(THREADS_END) d' \
			   -e 's,[@]ENABLE_THREADS[@],$(ENABLE_THREADS),g'
else !ENABLE_THREADS
THREADS_EDIT = -e '$(THREADS_START),$(THREADS_END) d'
endif !ENABLE_THREADS

edit = sed  -e 's,[@]pkglibdir[@],$(pkglibdir),g' \
			-e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
			$(PLUGIN_DELETE) \
			$(THREADS_EDIT)

theeye.d: $(srcdir)/Makefile $(srcdir)/theeye.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/theeye.d.in >$@.tmp
	mv $@.tmp $@

startup_dnode.d: $(srcdir)/Makefile $(srcdir)/startup_dnode.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_dnode.d.in >$@.tmp
	mv $@.tmp $@
