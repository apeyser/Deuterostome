if ENABLE_PLUGINS
PLUGINDISTR = plugin-config.m4
else !ENABLE_PLUGINS
PLUGINNODISTR = plugin-config.m4
endif !ENABLE_PLUGINS

m4data_DATA = $(PLUGINDISTR)
noinst_DATA = acx_pthread.m4 ax_cflags_strict_prototypes.m4 \
	ax_prefix_config_h.m4 configure.m4 lf_x11.m4 \
	lib_socket_nsl.m4 libtool.m4 $(PLUGINNODISTR)
