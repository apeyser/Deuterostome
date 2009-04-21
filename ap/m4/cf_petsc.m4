AC_DEFUN([CF_PETSC_MAKE], [`dnl 
    make -s -f ${srcdir}/m4/petsc.make dnl
    PETSC_ARCH=${PETSC_ARCH} dnl
    PETSC_DIR=${PETSC_DIR} dnl
    $1`dnl
])dnl
dnl
AC_DEFUN([CF_PETSC_EXTRACT], [dnl
  $1=CF_PETSC_MAKE([$1])
  AC_SUBST([$1]) dnl
])dnl
dnl
AC_DEFUN([CF_PETSC], [dnl
  AC_REQUIRE([CF_MPI])
  AC_PREREQ(2.50)
  cf_petsc_ok=no
dnl
  AC_ARG_WITH([petsc], dnl
    [AC_HELP_STRING([--with-petsc=<dir>], [use PETSC libraries in <dir>])])
  AC_MSG_CHECKING([for petsc configuration])
  case "${with_petsc-no}" in
    yes | "") AC_MSG_ERROR([petsc dir not defined]);;
    no) cf_petsc_ok=disable ;;
    *) PETSC_DIR="$with_petsc" ;;
  esac
dnl
  if test "$cf_petsc_ok" == disable ; then
    AC_MSG_RESULT([no, petsc will not be configured])
  else
    AC_ARG_WITH([petsc-arch],
      [AC_HELP_STRING([--with-petsc-arch=<arch>], [use PETSC <arch>])])
    case "${with_petsc_arch-no}" in
      yes | "") AC_MSG_ERROR([petsc arch not defined]);;
      no) AC_MSG_ERROR([--with-petsc-arch must be called for petsc]);;
      *) PETSC_ARCH="$with_petsc_arch";;
    esac
dnl
    CF_PETSC_EXTRACT([PETSC_LIB])
    CF_PETSC_EXTRACT([PETSC_CCPPFLAGS])
    PETSC_CCPPFLAGS=`echo "$PETSC_CCPPFLAGS" | sed s/\"/\'\"\'/g`
dnl
    AC_MSG_RESULT([yes, petsc will be configured with arch ${PETSC_ARCH} ]dnl
[from ${PETSC_DIR}])
dnl
    cf_petsc_save_LIBS="$LIBS"
    cf_petsc_save_CPPFLAGS="$CPPFLAGS"
dnl
    LIBS="$LIBS $PETSC_LIB"
    CPPFLAGS="$CPPFLAGS $PETSC_CCPPFLAGS"
    AC_MSG_CHECKING([for VecCreate in $PETSC_LIB])
    AC_TRY_LINK_FUNC([VecCreate], [cf_petsc_ok=yes], [cf_petsc_ok=no])
    if test "$cf_petsc_ok" = no ; then
      AC_MSG_ERROR([unable to link VecCreate])
    else
      AC_MSG_RESULT([yes])
    fi
    LIBS="$cf_petsc_save_LIBS"
    CPPFLAGS="$cf_petsc_save_CPPFLAGS"
dnl
dnl check for headers
    LIBS="$LIBS $PETSC_LIB"
    CPPFLAGS="$CPPFLAGS $PETSC_CCPPFLAGS"
    AC_CHECK_HEADERS([petsc.h], 
      [
        AC_MSG_CHECKING([for $ac_header]) 
	AC_MSG_RESULT([yes])
      ], [
        AC_MSG_CHECKING([for $ac_header])
        AC_MSG_ERROR([Not found])
    ])
    LIBS="$cf_petsc_save_LIBS"
    CPPFLAGS="$cf_petsc_save_CPPFLAGS"

    AC_DEFINE([HAVE_PETSC], 1, [Define if you have PETSC library.])
    AC_DEFINE([ENABLE_PETSC], 1, [Define if enabling PETSC])
    CF_SUBST_DEFINE_UNQUOTED([PETSC_DIR], dnl
      [$PETSC_DIR], dnl
      [Define to PETSC installation directory])
    CF_SUBST_DEFINE_UNQUOTED([PETSC_ARCH], dnl
      [$PETSC_ARCH], dnl
      [Define to PETSC arch])
  fi
dnl
  CF_AM_CONDITIONAL([PETSC], [test "$cf_petsc_ok" = yes])
  if test "$cf_petsc_ok" = yes ; then
    cf_enable_petsc=yes
  else
    cf_enable_petsc=no
  fi
  CF_AC_SUBST([enable_petsc], [$cf_enable_petsc])dnl
])dnl
dnl
AC_DEFUN([CF_CHECK_PETSC_SIZEOF], [dnl
  cf_petsc_save_CPPFLAGS="$CPPFLAGS"
  cf_petsc_save_LIBS="$LIBS"
  CPPFLAGS="$CPPFLAGS $PETSC_CCPPFLAGS"
  LIBS="$LIBS $PETSC_LIB"
  CF_AC_CHECK_SIZEOF([$1], [$2], [#include <petsc.h>])
  LIBS="$cf_petsc_save_LIBS"
  CPPFLAGS="$cf_petsc_save_CPPFLAGS" dnl
])dnl
