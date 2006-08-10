dist_dmstart_DATA = color.d master.d sched.d plugin.d
nodist_dmstart_DATA =  theeye.d startup_dnode.d \
	startup_dvt.d startup_dgen.d dvt_macros.d 
dist_dmconf_DATA = dvt.d
nodist_dmconf_DATA = dnode.d

EXTRA_DIST = theeye.d.in dnode.d.in dvt_macros.d.in
CLEANFILES = $(nodist_dmstart_DATA) $(nodist_dmconf_DATA)

XPS_START = /^[[:space:]]*[@]ENABLE_XPS_START[@][[:space:]]*$$/
XPS_ELSE = /^[[:space:]]*[@]ENABLE_XPS_ELSE[@][[:space:]]*$$/
XPS_END = /^[[:space:]]*[@]ENABLE_XPS_END[@][[:space:]]*$$/
if ENABLE_XPS
XPS_DELETE = -e '$(XPS_START) d' -e '$(XPS_ELSE),$(XPS_END) d'
else !ENABLE_XPS
XPS_DELETE = -e '$(XPS_START),$(XPS_ELSE) d' -e '$(XPS_END) d'
endif !ENABLE_XPS

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
			$(THREADS_EDIT) \
			$(XPS_DELETE)

theeye.d: $(srcdir)/Makefile $(srcdir)/theeye.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/theeye.d.in >$@.tmp
	mv $@.tmp $@

dnode.d: $(srcdir)/Makefile $(srcdir)/dnode.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dnode.d.in >$@.tmp
	mv $@.tmp $@

dvt_macros.d: $(srcdir)/Makefile $(srcdir)/dvt_macros.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/dvt_macros.d.in >$@.tmp
	mv $@.tmp $@
