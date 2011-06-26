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
	cf_gentoo.make cf_debian.make cf_in.make \
	cf_X11.m4 cf_sem.m4

GNULIB = 00gnulib.m4 fseeko.m4 memrchr.m4 \
	ftello.m4 mkdir.m4 \
	alloca.m4 futimens.m4 mkdtemp.m4 \
	argz.m4 getcwd.m4 mmap-anon.m4 \
	getdtablesize.m4 mode_t.m4 \
	getlogin_r.m4 multiarch.m4 \
	gettime.m4 onceonly.m4 \
	gettimeofday.m4 open.m4 \
	glibc21.m4 openat.m4 \
	glob.m4 plugin-config.m4 \
	gnulib-cache.m4 realloc.m4 \
	gnulib-common.m4 rmdir.m4 \
	gnulib-comp.m4 save-cwd.m4 \
	gnulib-tool.m4 stat-time.m4 \
	chdir-long.m4 include_next.m4 stat.m4 \
	chown.m4 lchown.m4 stdbool.m4 \
	clock_time.m4 stddef_h.m4 \
	cloexec.m4 lib-ld.m4 stdint.m4 \
	close.m4 lib-link.m4 stdio_h.m4 \
	codeset.m4 lib-prefix.m4 stdlib_h.m4 \
	configmake.m4 lib_socket_nsl.m4 strdup.m4 \
	libtool.m4 strerror.m4 \
	d-type.m4 localcharset.m4 strerror_r.m4 \
	dirent_h.m4 locale-fr.m4 string_h.m4 \
	dirfd.m4 locale-ja.m4 sys_stat_h.m4 \
	dirname.m4 locale-zh.m4 sys_time_h.m4 \
	double-slash-root.m4 lock.m4 tempname.m4 \
	dup2.m4 longlong.m4 threadlib.m4 \
	errno_h.m4 lseek.m4 time_h.m4 \
	error.m4 lstat.m4 timespec.m4 \
	extensions.m4 ltdl.m4 unistd-safer.m4 \
	fchdir.m4 ltoptions.m4 unistd_h.m4 \
	fclose.m4 ltsugar.m4 unlink.m4 \
	fcntl-o.m4 ltversion.m4 utimbuf.m4 \
	fcntl.m4 lt~obsolete.m4 utimens.m4 \
	fcntl_h.m4 malloc.m4 utimensat.m4 \
	fdopendir.m4 mbrtowc.m4 utimes.m4 \
	fflush.m4 mbsinit.m4 warn-on-use.m4 \
	filenamecat.m4 mbsrtowcs.m4 wchar_h.m4 \
	fnmatch.m4 mbstate_t.m4 wchar_t.m4 \
	fpurge.m4 memchr.m4 wctype_h.m4 \
	freading.m4 mempcpy.m4 wint_t.m4

EXTRA_DIST = $(GNULIB) \
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
