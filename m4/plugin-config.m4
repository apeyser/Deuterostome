
AC_DEFUN([CF_PKG_CHECK_MODULES_], [dnl
  ifelse([$3], , , [dnl
    changequote(<<, >>)dnl
    define(<<CF_PKG_CHECK_MODULES_VAR>>, 
           [$1_]translit(<<$3>>, [a-z], [A-Z])) dnl
    changequote([, ])
    AC_MSG_CHECKING([for pkg-config value of $3 in $2])
    if test "${CF_PKG_CHECK_MODULES_VAR+set}" = set ; then
      AC_MSG_WARN([$3 set from command line])
      pkg_failed=no
    elif CF_PKG_CHECK_MODULES_VAR[=`$PKG_CONFIG --variable="$3" "$2"`] ; then
    :
    else
      AC_MSG_ERROR([Unable to get variable $3])
    fi
    AC_MSG_RESULT([found, is ${]CF_PKG_CHECK_MODULES_VAR[}])
    AC_SUBST(CF_PKG_CHECK_MODULES_VAR)
    CF_PKG_CHECK_MODULES_([$1], [$2], m4_shift(m4_shift(m4_shift($@))))
  ])
])

AC_DEFUN([CF_PKG_CHECK_MODULES], [dnl
  changequote(<<, >>)dnl
  define(<<CF_PKG_CHECK_MODULES_NAME>>, translit(<<$1>>, [a-z-], [A-Z_])) dnl
  changequote([, ])
  PKG_CHECK_MODULES(CF_PKG_CHECK_MODULES_NAME, [$1])
  if test $pkg_failed = untried ; then
    AC_MSG_ERROR([Failed to find pkg-config])
  fi
  CF_PKG_CHECK_MODULES_(CF_PKG_CHECK_MODULES_NAME, $@)
])

AC_DEFUN([CF_MAKEFILE_PLUGIN], [dnl
  AC_SUBST_FILE([MAKEFILE_PLUGIN])
  CF_PKG_CHECK_MODULES([dm], [plugindata_dir])
  MAKEFILE_PLUGIN="${DM_PLUGINDATA_DIR}/Makefile.plugin"
])
