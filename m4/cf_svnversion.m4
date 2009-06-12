dnl -----------------------------------------------------------------
dnl cf_svnversion: 
dnl Macros & stuff for embedding svnversion information into
dnl   files.
dnl In top level Makefile.am, 
dnl   include $(srcdir)/m4/cf_svnversion-top.make
dnl In Makefiles where SVNVERSION is to be embedded,
dnl   include $(top_srcdir)/m4/cf_svnversion.make
dnl In configure.ac, call CF_SVNVERSION
dnl
dnl A file svnversion.stamp is created in the top srcdir
dnl  if in maintainer mode, and svnversion comes back with 
dnl  something useful. That file will be in the distribution
dnl  tar.gz (by Makefile rules above).
dnl Build directory will have an svnversion.stamp that overrides
dnl  the previous, if an svnversion change occurs in the distribution.
dnl ------------------------------------------------------------------
dnl
dnl CF_SVNVERSION_CHECK([var])
dnl
AC_DEFUN([CF_SVNVERSION_CHECK], [dnl
  test -n "$cf_svnversion$1" \
    && test "$cf_svnversion$1" != "unknown" \
    && test "$cf_svnversion$1" != "exported" \
    && SVNVERSION="$cf_svnversion$1" dnl
])dnl
dnl
dnl CF_SVNVERSION_READ([var], [dir], [optional command])
dnl
AC_DEFUN([CF_SVNVERSION_READ], [dnl
  ifelse([$#], [2], [dnl
    test -e "$2svnversion.stamp" \
      && cf_svnversion$1=`cat "$2svnversion.stamp" 2>/dev/null`], dnl
    [cf_svnversion$1=`$3 2>/dev/null`])dnl
])dnl
dnl
dnl CF_SVNVERSION_WRITE([var], [dir])
dnl
AC_DEFUN([CF_SVNVERSION_WRITE], [dnl
  test "$SVNVERSION" != "$cf_svnversion$1" \
    && echo "$SVNVERSION" >"$2svnversion.stamp" dnl
])dnl
dnl
dnl
dnl CF_SVNVERSION
dnl
dnl during configuration, runs svnversion and
dnl  if the version has changed, stores it in svnversion.stamp
dnl  and $srcdir/svnversion.stamp if maintainter mode is on.
dnl Also, sets up dependencies and inital setup in makefiles,
dnl  @SVNVERSION_MAKE@ for Makefiles that insert svnverions.
dnl SVNVERSION then gets AC_SUBST'D, and SVNVERSION_STAMP is SUBST'D
dnl  for dependencies in Makefiles.
dnl
AC_DEFUN([CF_SVNVERSION], [dnl
  AC_MSG_CHECKING([adding SVNVERSION to config.status])
dnl
  AC_CONFIG_COMMANDS([svnversion.stamp], [
    SVNVERSION="unknown"
dnl
    CF_SVNVERSION_READ([_src], [$srcdir/])
    CF_SVNVERSION_READ([_build], [])
    CF_SVNVERSION_READ([], [], [svnversion])
dnl
    CF_SVNVERSION_CHECK([_src])
    CF_SVNVERSION_CHECK([_build])
    CF_SVNVERSION_CHECK([])
dnl
    if test $USE_MAINTAINER_MODE = yes; then
       CF_SVNVERSION_WRITE([_src], [$srcdir/])
    fi
    CF_SVNVERSION_WRITE([_build])
    AC_MSG_NOTICE([src stamp = $cf_svnversion_src])
    AC_MSG_NOTICE([build stamp = $cf_svnversion_build])
    AC_MSG_NOTICE([svnversion = $cf_svnversion])
    AC_MSG_NOTICE([SVNVERSION = $SVNVERSION])
], [
    USE_MAINTAINER_MODE=$USE_MAINTAINER_MODE
])
dnl
    SVNVERSION_STAMP="\$(top_srcdir)/svnversion.stamp \$(top_builddir)/svnversion.stamp"
    AC_SUBST([SVNVERSION_STAMP])
dnl
  AC_MSG_RESULT([instantiated]) dnl
])dnl
dnl
