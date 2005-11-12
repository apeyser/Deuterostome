AC_INIT([D-Machine], [3.2], [Alexander Peyser <a.peyser@umiami.edu>], [dm])
AC_CONFIG_MACRO_DIR(m4)
AC_CANONICAL_SYSTEM
AM_CONFIG_HEADER([config.h])
AM_INIT_AUTOMAKE
AM_PATH_LISPDIR
AM_MAINTAINER_MODE

CF_AC_ENABLE([plugins], [compile or install plugins], [yes])
CF_IF_ENABLED([plugins], [
  CF_AM_ENABLE([process], [compile or install process plugin], [yes])
])

CF_AC_ARG_VAR([SYSDICTSIZE], [system dictionary size], [500])
CF_AC_ARG_VAR([ROLLBITS], [unroll math loops to 2^rollbits], [3])

CF_AM_ENABLE([setuid], [compile or install setuid wrapper], [yes])
CF_AC_ENABLE([threads], 
             [compile and install threads, value is default number], 
             [1])
CF_IF_ENABLED([threads], [
  CF_AC_ARG_VAR([THREADNUM], [max number of threads], [8])
  CF_AC_ARG_VAR([THREADMUL], [min unrolls per thread], [4])
])
CF_ACX_PTHREAD([threads])

CF_AC_ARG_DIR([dmbindir], [location for dm binary], [${bindir}/${PACKAGE}])
CF_AC_ARG_DIR([dmstartdir], [location for dm scripts], [${datadir}/${PACKAGE}])
CF_AC_ARG_DIR([pkgpsdatadir], [location for ps files], 
  [${datadir}/${PACKAGE}/ps])
CF_AC_ARG_DIR([pkglispdir], [location for lisp files],
  [${lispdir}/${PACKAGE}])

CF_ON_TARGET([*-*-darwin*], [
  AC_CHECK_PROG([guipdf], [launch], [launch -i com.apple.Preview])
])
AC_CHECK_PROG([guipdf], [gnome-open], [gnome-open])
AC_CHECK_PROG([guipdf], [kpdf], [kpdf])
AC_CHECK_PROG([guipdf], [xpdf], [xpdf])
AC_CHECK_PROG([guipdf], [gv], [gv])
AC_CHECK_PROG([guipdf], [gs], [gs])

CF_AM_PROG([ENABLE_BASH], [bash])
CF_AM_PROG([ENABLE_ENV], [env], [/usr/bin])

CF_AC_ARG_VAR_QUOTE([guipdf],
 [name of program to display pdf's, eg gnome-open or launch -i com.apple.Preview],
 [xpdf])

CF_EMACS_ENABLED

AC_PROG_CC
AC_PROG_CC_STDC
AC_C_CONST
AC_C_RESTRICT
AC_CHECK_HEADERS([unistd.h])
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LN_S

CF_SET_TOP_DIR
AC_LIBTOOL_WIN32_DLL
AC_LIBTOOL_DLOPEN
AC_LIBLTDL_CONVENIENCE
AC_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEP)

AC_CHECK_LIBM
CF_AC_PATH_XTRA
CF_PREPEND_VARS([LIBS], [XLIB LIBM])
AC_C_BIGENDIAN
CF_AC_CHECK_SIZEOF([time_t], [4])
CF_AC_CHECK_SIZEOF([void*], [4])

# check pid_t for plugins/process.c
CF_IF_ENABLED([process], [CF_AC_CHECK_SIZEOF([pid_t], [4])])
CF_IF_ENABLED([process], [CF_AC_CHECK_SIZEOF([int], [4])])
CF_IF_ENABLED([process], [CF_AC_CHECK_HEADER([sys/wait.h])])
CF_IF_ENABLED([setuid],  [CF_AC_CHECK_HEADER([sys/wait.h])])

CF_AC_CHECK_HEADERS([endian.h machine/endian.h], [ENDIAN_HDR], 
  [Header for endianness], [NO_ENDIAN_HDR])
CF_AC_CHECK_HEADER([math.h])
CF_AC_CHECK_HEADER([stdlib.h])
CF_AC_CHECK_HEADER([stdio.h])
CF_AC_CHECK_HEADER([sys/socket.h])
CF_AC_CHECK_HEADER([sys/select.h])
CF_AC_CHECK_HEADER([netinet/in.h])
CF_AC_CHECK_HEADER([errno.h])
CF_AC_CHECK_HEADER([netdb.h])
CF_AC_CHECK_HEADER([fcntl.h])
CF_AC_CHECK_HEADER([unistd.h])
CF_AC_CHECK_HEADER([string.h])
CF_AC_CHECK_HEADER([dirent.h])
CF_AC_CHECK_HEADER([time.h])
CF_AC_CHECK_HEADER([sys/stat.h])
CF_AC_CHECK_HEADER([fnmatch.h])
CF_AC_CHECK_HEADER([sys/types.h])
CF_AC_CHECK_HEADER([signal.h])
AC_CHECK_HEADER([X11/Xlib.h])
AC_CHECK_HEADER([X11/Xutil.h])
CF_AC_CHECK_HEADER([strings.h])
CF_AC_CHECK_HEADER([inttypes.h])
CF_IF_ENABLED([plugins], [CF_AC_CHECK_HEADER([stdarg.h])])
AC_CHECK_HEADER([error.h], [
  AC_DEFINE([HAVE_ERROR_H], [1], [Define to 1 if you have <error.h>])
])

CF_IF_UNDEF([CFLAGS], [
  CF_GCC_COMPILER_OPTION([CFLAGS], [-g])
  CF_GCC_COMPILER_OPTION([CFLAGS], [-O2])
])
CF_GCC_COMPILER_OPTION([CFLAGS], [-fno-strict-aliasing])

CF_GCC_COMPILER_OPTION([GCC_OPT], [-pipe])
#CF_GCC_COMPILER_OPTION([GCC_OPT], [-fno-math-errno])
#CF_GCC_COMPILER_OPTION([GCC_OPT], [-fno-trapping-math])
#CF_GCC_COMPILER_OPTION([GCC_OPT], [-fno-signaling-nans])
CF_GCC_COMPILER_OPTION([GCC_OPT], [-fno-unsigned-char])
CF_GCC_COMPILER_OPTION([GCC_OPT], [-ffast-math])
#CF_GCC_COMPILER_OPTION([GCC_OPT], [-fno-finite-math-only])
#CF_GCC_COMPILER_OPTION([GCC_OPT], [-fcx-limited-range])
CF_GCC_COMPILER_OPTION([GCC_OPT], [-Wno-pointer-sign])
AC_SUBST([GCC_OPT])

AC_SUBST([INCLTDL])
AC_SUBST([LIBLTDL])
AC_CONFIG_SUBDIRS([libltdl])

CF_WIN_DLL_IMPORT(DM)
CF_WIN_DLL_IMPORT(PLUGIN)

CF_SUBST_DEFINE([NAMEBYTES], 
				[18L], 
				[Number of bytes in a d name, without terminating '\0'],
				[18])

AC_CONFIG_FILES([Makefile 
		 src/Makefile 
		 src/codegen/Makefile src/codegen/startup_dgen.d src/codegen/dmnuminc.d
		 plugins/Makefile 
		 ps/Makefile 
		 sh/Makefile
		 emacs/Makefile emacs/d-mode.el
		 dcode/Makefile dcode/startup_dvt.d
])

AC_OUTPUT
