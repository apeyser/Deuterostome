AC_DEFUN([CF_ATLAS], [
  AC_PREREQ(2.50)
  cf_atlas_ok=no

  AC_ARG_WITH([atlas],
	  [AC_HELP_STRING([--with-atlas=<lib>], [use ATLAS libraries <lib>])])
  case "$with_atlas" in
	  yes | "") ;;
	  no) cf_atlas_ok=disable ;;
	  -* | */* | *.a | *.so | *.so.* | *.o) ATLAS_LIBS="$with_atlas" ;;
	  *) ATLAS_LIBS="-l$with_atlas" ;;
  esac
  AC_ARG_WITH([atlas-flags],
    [AC_HELP_STRING([--with-atlas-flags=<flags>], [use <flags> for atlas])])
  case "$with_atlas_flags" in
    yes) ;;
    "" | no) ATLAS_FLAGS="" ;;
    *) ATLAS_FLAGS="$with_atlas_flags" ;;
  esac

  cf_atlas_save_LIBS="$LIBS"
  cf_atlas_save_CPPFLAGS="$CPPFLAGS"

  # First, check ATLAS_LIBS environment variable
  if test $cf_atlas_ok = no; then
    if test "x$ATLAS_LIBS" != x; then
	    LIBS="$ATLAS_LIBS $LIBS"
      CPPFLAGS="$CPPFLAGS $ATLAS_FLAGS"
	    AC_MSG_CHECKING([for cblas_sgemm in $ATLAS_LIBS])
	    AC_TRY_LINK_FUNC([cblas_sgemm], [cf_atlas_ok=yes], [ATLAS_LIBS=""])
	    AC_MSG_RESULT([$cf_atlas_ok])
      if test $cf_atlas_ok = yes ; then
        AC_MSG_CHECKING([for clapack_dgesv in $ATLAS_LIBS])
        AC_TRY_LINK_FUNC([clapack_dgesv], [cf_atlas_ok=yes], 
                         [ATLAS_LIBS="" ; cf_atlas_ok=no])
        AC_MSG_RESULT([$cf_atlas_ok])
      fi
	    LIBS="$cf_atlas_save_LIBS"
      CPPFLAGS="$cf_atlas_save_CPPFLAGS"
    fi
  fi

  # BLAS in ATLAS library? (http://math-atlas.sourceforge.net/)
  if test $cf_atlas_ok = no; then
     CPPFLAGS="$CPPFLAGS $ATLAS_FLAGS"
	   AC_SEARCH_LIBS([cblas_sgemm], [ptcblas cblas],
                    [if test "$ac_res" = "none required" ; then 
                       ATLAS_LIBS="-latlas"
                     else
                       ATLAS_LIBS="$ac_res -latlas"
                     fi
                     cf_atlas_ok=yes],
                    [], [-latlas])
     AC_MSG_CHECKING([for cblass_sgemm in ptcblas and cblas])
     if test $cf_atlas_ok = no ; then
        AC_MSG_ERROR([No atlas cblas found])
     else
        AC_MSG_RESULT([yes, in $ac_res])
        AC_SEARCH_LIBS([clapack_dgesv], [lapack],
                       [AC_MSG_CHECKING([for clapack_dgesv in lapack])
                        if test "$ac_res" != "none required" ; then
                         ATLAS_LIBS="-llapack $ATLAS_LIBS"
                        fi
                        AC_MSG_RESULT([yes])
                       ],
                       [AC_MSG_CHECKING([for clapack_dgesv in lapack])
                        cf_atlas_ok=no
                        AC_MSG_ERROR([None found])
                       ],
                       [$ATLAS_LIBS])
     fi
     CPPFLAGS="$cf_atlas_save_CPPFLAGS"
  fi

  LIBS="$cf_atlas_save_LIBS"
  if test x"$cf_atlas_ok" = xyes; then
    LIBS="$ATLAS_LIBS $LIBS"
    CPPFLAGS="$CPPFLAGS $ATLAS_FLAGS"
    AC_CHECK_HEADERS([cblas.h clapack.h], 
                     [AC_MSG_CHECKING([for $ac_header])
                      AC_MSG_RESULT([yes])],
                     [AC_MSG_CHECKING([for $ac_header])
                      AC_MSG_ERROR([Not found])])
     LIBS="$cf_atlas_save_LIBS"
     CPPFLAGS="$cf_atlas_save_CPPFLAGS"
     AC_DEFINE([HAVE_ATLAS], 1, [Define if you have ATLAS library.])
  fi

  AM_CONDITIONAL([ENABLE_ATLAS], [test x"$cf_atlas_ok" = xyes])
  AC_SUBST([ATLAS_LIBS])
  AC_SUBST([ATLAS_FLAGS])
])
