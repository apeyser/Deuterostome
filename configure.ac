AC_INIT([D-Machine], [3.2], [Alexander Peyser <a.peyser@umiami.edu>], [dm])
AC_CONFIG_MACRO_DIR(m4)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([config.h])
AX_PREFIX_CONFIG_H([src/dm-config.h])
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
CF_AC_ARG_DIR([dmconfdir], [locations for per-host configuration],
  [${sysconfdir}/${PACKAGE}])
CF_AC_ARG_DIR([pkgpsdatadir], [location for ps files], 
  [${datadir}/${PACKAGE}/ps])
CF_AC_ARG_DIR([pkglispdir], [location for lisp files],
  [${lispdir}/${PACKAGE}])
CF_AC_ARG_DIR([tmp], [location of temp dir], [/tmp])

CF_AC_ENABLE([unix-sockets], [compile and install unix domain sockets], [yes])
CF_IF_ENABLED([unix-sockets], [
  CF_AC_ARG_DIR([dmsockdir], 
    [location for dm unix sockets], 
    [${tmp}/${PACKAGE}/sockets])
])

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
CF_AC_CHECK_HEADER([sys/un.h])
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
CF_AC_CHECK_HEADER([time.h])
CF_AC_CHECK_HEADER([sys/time.h])
AC_CHECK_HEADER([X11/Xlib.h])
AC_CHECK_HEADER([X11/Xutil.h])
CF_AC_CHECK_HEADER([strings.h])
CF_AC_CHECK_HEADER([inttypes.h])
CF_IF_ENABLED([plugins], [CF_AC_CHECK_HEADER([stdarg.h])])
AC_CHECK_HEADER([error.h], [
  AC_DEFINE([HAVE_ERROR_H], [1], [Define to 1 if you have <error.h>])
])

AX_CFLAGS_STRICT_PROTOTYPES()
CF_IF_UNDEF([CFLAGS], [
  CF_GCC_COMPILER_OPTION([-g])
  CF_GCC_COMPILER_OPTION([-O2])
])
CF_GCC_COMPILER_OPTION([-ftree-vectorize])
CF_GCC_COMPILER_OPTION([-fno-strict-aliasing])

CF_GCC_COMPILER_OPTION([-pipe], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fno-unsigned-char], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-ffast-math], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fno-finite-math-only], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fno-unsafe-math-optimizations], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-Wno-pointer-sign], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fprefetch-loop-arrays], [GCC_CFLAGS])

AC_SUBST([INCLTDL])
AC_SUBST([LIBLTDL])
AC_CONFIG_SUBDIRS([libltdl])

CF_WIN_DLL_IMPORT(DM)
CF_WIN_DLL_IMPORT(PLUGIN)

CF_SUBST_DEFINE([NAMEBYTES], 
				[18L], 
				[Number of bytes in a d name, without terminating '\0'],
				[18])

LIB_SOCKET_NSL

AC_CONFIG_LINKS([
		src/dsp1f.h:src/codegen/dsp1f.h.built
		src/dmnuminc.h:src/codegen/dmnuminc.h.built
		src/codegen/dmglobals.c:src/dmglobals.c
		src/codegen/dsp1.c:src/dsp1.c
		src/codegen/dsp2.c:src/dsp2.c
		src/codegen/dm1.c:src/dm1.c
		src/codegen/dm2.c:src/dm2.c
		src/codegen/dm3.c:src/dm3.c
		src/codegen/dm4.c:src/dm4.c
		src/codegen/dm5.c:src/dm5.c
		src/codegen/dm6.c:src/dm6.c
		src/codegen/dm7.c:src/dm7.c
		src/codegen/dm8.c:src/dm8.c
		src/codegen/dmnum.c:src/dmnum.c
		src/codegen/paths.h.in:src/paths.h.in
])

AC_CONFIG_FILES([
		 Makefile 
		 src/Makefile 
		 src/codegen/Makefile src/codegen/startup_dgen.d src/codegen/dmnuminc.d
		 plugins/Makefile 
		 ps/Makefile 
		 sh/Makefile
		 emacs/Makefile emacs/d-mode.el
		 dcode/Makefile dcode/startup_dvt.d dcode/startup_dnode.d
])

AC_OUTPUT
