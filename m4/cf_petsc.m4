AC_DEFUN([CF_PETSC], [
  AC_REQUIRE([CF_MPI])
  AC_PREREQ(2.50)
  cf_petsc_ok=no

  AC_ARG_WITH([petsc],
    [AC_HELP_STRING([--with-petsc=<lib>], [use PETSC libraries <lib>])])
  case "$with_petsc" in
    yes | "") ;;
    no) cf_petsc_ok=disable ;;
    -* | */* | *.a | *.so | *.so.* | *.o) PETSC_LIBS="$with_petsc" ;;
    *) PETSC_LIBS="-l$with_petsc" ;;
  esac

  AC_ARG_WITH([petsc-flags],
    [AC_HELP_STRING([--with-petsc-flags=<flags>], [use <flags> for petsc])])
  case "$with_petsc_flags" in
    yes) ;;
    "" | no) PETSC_FLAGS="" ;;
    *) PETSC_FLAGS="$with_petsc_flags" ;;
  esac

  cf_petsc_save_LIBS="$LIBS"
  cf_petsc_save_CPPFLAGS="$CPPFLAGS"

  # First, check PETSC_LIBS environment variable
  if test "$cf_petsc_ok" = no; then
    if test "x$PETSC_LIBS" != x; then
      LIBS="$PETSC_LIBS $LIBS"
      CPPFLAGS="$CPPFLAGS $PETSC_FLAGS"
      AC_MSG_CHECKING([for VecCreate in $PETSC_LIBS])
	AC_TRY_LINK_FUNC([VecCreate], [cf_petsc_ok=yes], [PETSC_LIBS=""])
	AC_MSG_RESULT([$cf_petsc_ok])
      LIBS="$cf_petsc_save_LIBS"
      CPPFLAGS="$cf_petsc_save_CPPFLAGS"
    fi
  fi

  # petsc in PETSC library?
  if test "$cf_petsc_ok" = no; then
     PETSC_LIBS="-lpetsc"
     CPPFLAGS="$CPPFLAGS $PETSC_FLAGS"
     AC_SEARCH_LIBS([VecCreate], [petsc],
       [AC_MSG_CHECKING([for VecCreate in $ac_res])
        if test -n "$ac_lib" ; then 
           PETSC_LIBS="$ac_res $PETSC_LIBS"
        fi
        AC_MSG_RESULT([yes])
           cf_petsc_ok=yes
           break
        ], [
 	  AC_MSG_CHECKING([for VecCreate in $ac_res])
          AC_MSG_RESULT([no])
        ], 
        [$PETSC_LIBS])
     AC_MSG_CHECKING([for VecCreate in petsc])
     if test $cf_petsc_ok = no ; then
        AC_MSG_ERROR([No petsc found])
     else
        AC_MSG_RESULT([yes, in $ac_res])
     fi
     CPPFLAGS="$cf_petsc_save_CPPFLAGS"
  fi

  LIBS="$cf_petsc_save_LIBS"
  if test "$cf_petsc_ok" = yes; then
    LIBS="$PETSC_LIBS $LIBS"
    CPPFLAGS="$CPPFLAGS $PETSC_FLAGS"
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
  fi

  AM_CONDITIONAL([ENABLE_PETSC], [test "$cf_petsc_ok" = yes])
  AC_SUBST([PETSC_LIBS])
  AC_SUBST([PETSC_FLAGS])
])
