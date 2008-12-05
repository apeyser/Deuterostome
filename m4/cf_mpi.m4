AC_DEFUN([CF_MPI], [dnl
  AC_PREREQ(2.50)
  cf_mpi_ok=no
dnl
  AC_ARG_WITH([mpi],
    [AC_HELP_STRING([--with-mpi=<lib>], [use MPI libraries <lib>])])
  case "$with_mpi" in
    yes | "") ;;
    no) cf_mpi_ok=disable ;;
    -* | */* | *.a | *.so | *.so.* | *.o) MPI_LIBS="$with_mpi" ;;
    *) MPI_LIBS="-l$with_mpi" ;;
  esac
dnl
  AC_ARG_WITH([mpi-flags],
    [AC_HELP_STRING([--with-mpi-flags=<flags>], [use <flags> for mpi])])
  case "$with_mpi_flags" in
    yes) ;;
    "" | no) MPI_FLAGS="" ;;
    *) MPI_FLAGS="$with_mpi_flags" ;;
  esac
dnl
  cf_mpi_save_LIBS="$LIBS"
  cf_mpi_save_CPPFLAGS="$CPPFLAGS"
dnl
  # First, check MPI_LIBS environment variable
  if test "$cf_mpi_ok" = no; then
    if test "x$MPI_LIBS" != x; then
      LIBS="$MPI_LIBS $LIBS"
      CPPFLAGS="$CPPFLAGS $MPI_FLAGS"
      AC_MSG_CHECKING([for MPI_Init_thread in $MPI_LIBS])
	AC_TRY_LINK_FUNC([MPI_Init_thread], [cf_mpi_ok=yes], [MPI_LIBS=""])
	AC_MSG_RESULT([$cf_mpi_ok])
      LIBS="$cf_mpi_save_LIBS"
      CPPFLAGS="$cf_mpi_save_CPPFLAGS"
    fi
  fi
dnl
  # mpi in MPI library?
  if test "$cf_mpi_ok" = no; then
     MPI_LIBS=""
     CPPFLAGS="$CPPFLAGS $MPI_FLAGS"
     AC_SEARCH_LIBS([MPI_Init_thread], [mpich],
       [AC_MSG_CHECKING([for MPI_Init_thread in $ac_res])
        if test -n "$ac_lib" ; then 
           MPI_LIBS="$ac_res $MPI_LIBS"
        fi
        AC_MSG_RESULT([yes])
           cf_mpi_ok=yes
           break
        ], [
 	  AC_MSG_CHECKING([for MPI_Init_thread in $ac_res])
          AC_MSG_RESULT([no])
        ], 
        [$MPI_LIBS])
     AC_MSG_CHECKING([for MPI_Init_thread in mpich])
     if test $cf_mpi_ok = no ; then
        AC_MSG_RESULT([No mpich MPI_Init_thread found])
     else
        AC_MSG_RESULT([yes, in $ac_res])
     fi
     CPPFLAGS="$cf_mpi_save_CPPFLAGS"
  fi
dnl
  LIBS="$cf_mpi_save_LIBS"
  if test "$cf_mpi_ok" = yes; then
    LIBS="$MPI_LIBS $LIBS"
    CPPFLAGS="$CPPFLAGS $MPI_FLAGS"
    AC_CHECK_HEADERS([mpi.h], 
      [
        AC_MSG_CHECKING([for $ac_header]) 
	AC_MSG_RESULT([yes])
      ], [
        AC_MSG_CHECKING([for $ac_header])
        AC_MSG_ERROR([Not found])
     ])
     LIBS="$cf_mpi_save_LIBS"
     CPPFLAGS="$cf_mpi_save_CPPFLAGS"
     AC_DEFINE([HAVE_MPI], 1, [Define if you have MPI library.])
  fi
dnl
  AM_CONDITIONAL([ENABLE_MPI], [test "$cf_mpi_ok" = yes])
  AC_SUBST([MPI_LIBS])
  AC_SUBST([MPI_FLAGS])
  if test "$cf_mpi_ok" = yes ; then
    ifelse($1,,:,$1)
  else
    ifelse($2,,:,$2)
  fi dnl
])dnl
