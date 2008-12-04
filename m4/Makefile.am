if ENABLE_PLUGINS_SUPPORT
PLUGINDISTR = plugin-config.m4
else !ENABLE_PLUGINS_SUPPORT
PLUGINNODISTR = plugin-config.m4
endif !ENABLE_PLUGINS_SUPPORT

dist_m4data_DATA = $(PLUGINDISTR)
dist_noinst_DATA = acx_pthread.m4 ax_cflags_strict_prototypes.m4 \
	ax_prefix_config_h.m4 configure.m4 lf_x11.m4 \
	cf_atlas.m4 \
	lib_socket_nsl.m4 $(PLUGINNODISTR) \
	basic-defs.m4 \
	cf_petsc.m4 \
	cf_mpi.m4 \
	ax_set_version_info.m4 \
	petsc.make \
	svnversion.m4

EXTRA_DIST = gnulib-cache.m4 dirfd.m4 d-type.m4 extensions.m4 fnmatch.m4 glob.m4 gnulib-common.m4 gnulib-comp.m4 gnulib-tool.m4 libtool.m4 onceonly.m4

@MAKE_SVNVERSION_M4@
