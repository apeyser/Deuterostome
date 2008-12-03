dist_dmstart_DATA = color.d master.d sched.d plugin.d errors.d
nodist_dmstart_DATA =  theeye.d startup_dnode.d startup_dpawn.d \
	startup_dvt.d startup_dgen.d dvt_macros.d matrix.d new-plugin.d \
	startup_common.d startup_libs.d
dist_dmconf_DATA = dvt.d dpawn.d
nodist_dmconf_DATA = dnode.d

EXTRA_DIST = theeye.d.in dnode.d.in dvt_macros.d.in \
	startup_dvt.d.in startup_dnode.d.in startup_dpawn.d.in \
	matrix.d.in startup_dgen.d.in new-plugin.d.in \
	dpawn.d startup_common.d.in startup_libs.d.in

CLEANFILES = $(nodist_dmstart_DATA) $(nodist_dmconf_DATA)

XPS_START = /^[[:space:]]*[@]ENABLE_XPS_START[@][[:space:]]*$$/
XPS_ELSE = /^[[:space:]]*[@]ENABLE_XPS_ELSE[@][[:space:]]*$$/
XPS_END = /^[[:space:]]*[@]ENABLE_XPS_END[@][[:space:]]*$$/
if ENABLE_XPS
XPS_DELETE = -e '$(XPS_START) d' -e '$(XPS_ELSE),$(XPS_END) d'
else !ENABLE_XPS
XPS_DELETE = -e '$(XPS_START),$(XPS_ELSE) d' -e '$(XPS_END) d'
endif !ENABLE_XPS

PERL_START = /^[[:space:]]*[@]ENABLE_PERL_START[@][[:space:]]*$$/
PERL_END = /^[[:space:]]*[@]ENABLE_PERL_END[@][[:space:]]*$$/
if ENABLE_PERL
PERL_DELETE = -e '$(PERL_START) d' -e '$(PERL_END) d'
else !ENABLE_PERL
PERL_DELETE = -e '$(PERL_START),$(PERL_END) d'
endif !ENABLE_PERL

REGEX_START = /^[[:space:]]*[@]ENABLE_REGEX_START[@][[:space:]]*$$/
REGEX_END = /^[[:space:]]*[@]ENABLE_REGEX_END[@][[:space:]]*$$/
if ENABLE_REGEX
REGEX_DELETE = -e '$(REGEX_START) d' -e '$(REGEX_END) d'
else !ENABLE_REGEX
REGEX_DELETE = -e '$(REGEX_START),$(REGEX_END) d'
endif !ENABLE_REGEX

PLUGINS_START = /^[[:space:]]*[@]ENABLE_PLUGINS_START[@][[:space:]]*$$/
PLUGINS_END = /^[[:space:]]*[@]ENABLE_PLUGINS_END[@][[:space:]]*$$/
if ENABLE_PLUGINS
PLUGINS_DELETE = -e '$(PLUGINS_START) d' -e '$(PLUGINS_END) d'
else !ENABLE_PLUGINS
PLUGINS_DELETE = -e '$(PLUGINS_START),$(PLUGINS_END) d'
endif !ENABLE_PLUGINS

PLUGINS_SUPPORT_START = /^[[:space:]]*[@]ENABLE_PLUGINS_SUPPORT_START[@][[:space:]]*$$/
PLUGINS_SUPPORT_END = /^[[:space:]]*[@]ENABLE_PLUGINS_SUPPORT_END[@][[:space:]]*$$/
if ENABLE_PLUGINS_SUPPORT
PLUGINS_SUPPORT_DELETE = -e '$(PLUGINS_SUPPORT_START) d' -e '$(PLUGINS_SUPPORT_END) d'
else !ENABLE_PLUGINS_SUPPORT
PLUGINS_SUPPORT_DELETE = -e '$(PLUGINS_SUPPORT_START),$(PLUGINS_SUPPORT_END) d'
endif !ENABLE_PLUGINS_SUPPORT


THREADS_START = /^[[:space:]]*[@]ENABLE_THREADS_START[@][[:space:]]*$$/
THREADS_END   = /^[[:space:]]*[@]ENABLE_THREADS_END[@][[:space:]]*$$/

if ENABLE_THREADS
THREADS_EDIT = -e '$(THREADS_START) d' \
			   -e '$(THREADS_END) d' \
			   -e 's,[@]ENABLE_THREADS[@],$(ENABLE_THREADS),g'
else !ENABLE_THREADS
THREADS_EDIT = -e '$(THREADS_START),$(THREADS_END) d'
endif !ENABLE_THREADS

EMACSCLIENT_START = /^[[:space:]]*[@]ENABLE_EMACSCLIENT_START[@][[:space:]]*$$/
EMACSCLIENT_END = /^[[:space:]]*[@]ENABLE_EMACSCLIENT_END[@][[:space:]]*$$/

if ENABLE_EMACS
if ENABLE_EMACSCLIENT
EMACSCLIENT_EDIT =  -e '$(EMACSCLIENT_START) d' -e '$(EMACSCLIENT_END) d'
else !ENABLE_EMACSCLIENT
EMACSCLIENT_EDIT =  -e '$(EMACSCLIENT_START),$(EMACSCLIENT_END) d'
endif !ENABLE_EMACSCLIENT
else !ENABLE_EMACS
EMACSCLIENT_EDIT =  -e '$(EMACSCLIENT_START),$(EMACSCLIENT_END) d'
endif !ENABLE_EMACS

ATLAS_START = /^[[:space:]]*[@]ENABLE_ATLAS_START[@][[:space:]]*$$/
ATLAS_END   = /^[[:space:]]*[@]ENABLE_ATLAS_END[@][[:space:]]*$$/

if ENABLE_ATLAS
ATLAS_EDIT = -e '$(ATLAS_START) d' -e '$(ATLAS_END) d'
else !ENABLE_ATLAS
ATLAS_EDIT = -e '$(ATLAS_START),$(ATLAS_END) d'
endif !ENABLE_ATLAS

RTHREADS_START = /^[[:space:]]*[@]ENABLE_RTHREADS_START[@][[:space:]]*$$/
RTHREADS_END   = /^[[:space:]]*[@]ENABLE_RTHREADS_END[@][[:space:]]*$$/

if ENABLE_RTHREADS
RTHREADS_EDIT = -e '$(RTHREADS_START) d' -e '$(RTHREADS_END) d'
else !ENABLE_RTHREADS
RTHREADS_EDIT = -e '$(RTHREADS_START),$(RTHREADS_END) d'
endif !ENABLE_RTHREADS

PETSC_START = /^[[:space:]]*[@]ENABLE_PETSC_START[@][[:space:]]*$$/
PETSC_END   = /^[[:space:]]*[@]ENABLE_PETSC_END[@][[:space:]]*$$/

PETSC_EDIT_ON = -e '$(PETSC_START) d' -e '$(PETSC_END) d'
PETSC_EDIT_OFF = -e '$(PETSC_START),$(PETSC_END) d'
if !ENABLE_PLUGINS_SUPPORT
PETSC_EDIT = $(PETSC_EDIT_OFF)
else ENABLE_PLUGINS_SUPPORT
if !ENABLE_PLUGINS
PETSC_EDIT = $(PETSC_EDIT_OFF)
else ENABLE_PLUGINS
if !ENABLE_PETSC
PETSC_EDIT = $(PETSC_EDIT_OFF)
else ENABLE_PETSC
PETSC_EDIT = $(PETSC_EDIT_ON)
endif ENABLE_PETSC
endif ENABLE_PLUGINS
endif ENABLE_PLUGINS_SUPPORT

SVNVERSION = $(shell )

SVNVER_EDIT = -e "s/\\\$$SVNVERSION\\(:[^$$]*\\)\\?\\\$$/\\\$$SVNVERSION: `svnversion || echo exported`\\\$$/"

edit = sed  -e 's,[@]pkglibdir[@],$(pkglibdir),g' \
			-e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
			-e 's,[@]NAMEBYTES[@],$(NAMEBYTES),g' \
			$(PLUGINS_DELETE) \
			$(PLUGINS_SUPPORT_DELETE) \
			$(THREADS_EDIT) \
			$(XPS_DELETE) \
			$(PERL_DELETE) \
			$(EMACSCLIENT_EDIT) \
			$(ATLAS_EDIT) \
			$(RTHREADS_EDIT) \
			$(REGEX_DELETE) \
			$(PETSC_EDIT) \
			$(SVNVER_EDIT)

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

startup_dvt.d: $(srcdir)/Makefile $(srcdir)/startup_dvt.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_dvt.d.in >$@.tmp
	mv $@.tmp $@

startup_dnode.d: $(srcdir)/Makefile $(srcdir)/startup_dnode.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_dnode.d.in >$@.tmp
	mv $@.tmp $@

startup_dpawn.d: $(srcdir)/Makefile $(srcdir)/startup_dpawn.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_dpawn.d.in >$@.tmp
	mv $@.tmp $@

startup_dgen.d: $(srcdir)/Makefile $(srcdir)/startup_dgen.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_dgen.d.in >$@.tmp
	mv $@.tmp $@

startup_common.d: $(srcdir)/Makefile $(srcdir)/startup_common.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_common.d.in >$@.tmp
	mv $@.tmp $@

startup_libs.d: $(srcdir)/Makefile $(srcdir)/startup_libs.d.in 
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/startup_libs.d.in >$@.tmp
	mv $@.tmp $@

matrix.d: $(srcdir)/Makefile $(srcdir)/matrix.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/matrix.d.in >$@.tmp
	mv $@.tmp $@

new-plugin.d: $(srcdir)/Makefile $(srcdir)/new-plugin.d.in
	rm -f $@ $@.tmp
	$(edit) $(srcdir)/new-plugin.d.in >$@.tmp
	mv $@.tmp $@
