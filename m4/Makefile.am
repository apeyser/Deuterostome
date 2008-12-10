if ENABLE_PLUGINS_SUPPORT
PLUGINDISTR = plugin-config.m4
else !ENABLE_PLUGINS_SUPPORT
PLUGINNODISTR = plugin-config.m4
endif !ENABLE_PLUGINS_SUPPORT

dist_m4data_DATA = $(PLUGINDISTR)
dist_noinst_DATA = \
	acx_pthread.m4 ax_cflags_strict_prototypes.m4 \
	ax_prefix_config_h.m4 configure.m4 lf_x11.m4 \
	cf_atlas.m4 \
	lib_socket_nsl.m4 $(PLUGINNODISTR) \
	basic-defs.m4 \
	cf_petsc.m4 \
	cf_mpi.m4 \
	ax_set_version_info.m4 \
	petsc.make \
	cf_latex.m4 cf_latex.make \
	cf_svnversion.m4 cf_svnversion.make cf_svnversion-top.make

EXTRA_DIST = \
	gnulib-cache.m4 dirfd.m4 d-type.m4 extensions.m4 \
	fnmatch.m4 glob.m4 gnulib-common.m4 gnulib-comp.m4 \
	gnulib-tool.m4 libtool.m4 onceonly.m4 \
	latex/ac_latex_class_article.m4 latex/ac_latex_class_book.m4 \
	latex/ac_latex_classes.m4 latex/ac_latex_class.m4 \
	latex/ac_latex_class_report.m4 latex/ac_latex_dvips_o_stdout.m4 \
	latex/ac_latex_dvips_t_a4_landscape.m4 latex/ac_latex_dvips_t_a4.m4 \
	latex/ac_latex_dvips_t_letter_landscape.m4 \
	latex/ac_latex_dvips_t_letter.m4 latex/ac_latex_dvips_t.m4 \
	latex/ac_latex_package_amsmath.m4 latex/ac_latex_package_fontenc.m4 \
	latex/ac_latex_package_input.m4 latex/ac_latex_package.m4 \
	latex/ac_latex_package_opt.m4 latex/ac_latex_packages.m4 \
	latex/_ac_latex_test.m4 latex/ac_prog_bibtex.m4 \
	latex/ac_prog_dvibook.m4 latex/ac_prog_dvipdf.m4 \
	latex/ac_prog_dvips.m4 latex/ac_prog_fig2dev.m4 \
	latex/ac_prog_gnuplot.m4 latex/ac_prog_gunzip.m4 \
	latex/ac_prog_gzip.m4 latex/ac_prog_jpeg2ps.m4 \
	latex/ac_prog_jpegtopnm.m4 latex/ac_prog_kpsewhich.m4 \
	latex/ac_prog_latex2html.m4 latex/ac_prog_latex2man.m4 \
	latex/ac_prog_latex2png.m4 latex/ac_prog_latex2rtf.m4 \
	latex/ac_prog_latex.m4 latex/ac_prog_makeindex.m4 \
	latex/ac_prog_mf.m4 latex/ac_prog_mktexpk.m4 \
	latex/ac_prog_pdflatex.m4 latex/ac_prog_pngtopnm.m4 \
	latex/ac_prog_pnmtops.m4 latex/ac_prog_ps2pdf.m4 \
	latex/ac_prog_psbook.m4 latex/ac_prog_psnup.m4 \
	latex/ac_prog_psresize.m4 latex/ac_prog_psselect.m4 \
	latex/ac_prog_texhash.m4 latex/ac_texmf_path.m4 latex/latex.m4 \
	latex/LICENSE latex/README
