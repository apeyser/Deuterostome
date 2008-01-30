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
	basic-defs.m4
