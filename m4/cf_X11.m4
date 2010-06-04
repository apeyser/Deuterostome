AC_DEFUN([CF_X11_ENABLED], [dnl
  AC_REQUIRE([CF_AC_PATH_XTRA])dnl
  if test x"$no_x" == xyes; then
    ifelse([$2], , [:], [$2])
  else
    ifelse([$1], , [:], [$1])
  fi dnl
])dnl
dnl
AC_DEFUN([CF_X11_ENABLE], [dnl
  CF_X11_ENABLED([dnl
    AM_CONDITIONAL([ENABLE_X11], [:])dnl
  ], [dnl
    AM_CONDITIONAL([ENABLE_X11], [false])dnl
  ])dnl
])dnl
dnl

    