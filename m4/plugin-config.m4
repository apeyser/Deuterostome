AC_DEFUN([CF_PKG_CHECK_MODULES_], [dnl
  ifelse([$3], , , [dnl
    changequote(<<, >>)dnl
    define(<<CF_PKG_CHECK_MODULES_VAR>>, 
           [$1_]translit(<<$3>>, [a-z], [A-Z])) dnl
    changequote([, ])

    AC_MSG_CHECKING([for pkg-config value of variable $3 in $2])
    if test "${CF_PKG_CHECK_MODULES_VAR+set}" = set ; then
      AC_MSG_WARN(CF_PKG_CHECK_MODULES_VAR [set from command line])
      pkg_failed=no
    else
		if test "$3" = "prefix" && \
		   CF_PKG_CHECK_MODULES_VAR[=`$PKG_CONFIG \
               --variable="$3" "$2"`] ; then
		   :
        elif test "$3" != "prefix" && \
    	   CF_PKG_CHECK_MODULES_VAR[=`$PKG_CONFIG \
		     --define-variable=prefix=\\${$1_PREFIX} \
			 --variable="$3" "$2"`] ; then
    	   :
		else
		   AC_MSG_ERROR([Unable to get variable $3])
		fi
    fi
    CF_PKG_CHECK_MODULES_VAR[_BASE=${]CF_PACKAGE_MODULES_VAR[}]
    AC_MSG_RESULT([found, is \`${]CF_PKG_CHECK_MODULES_VAR[}'])
    AC_SUBST(CF_PKG_CHECK_MODULES_VAR)
    CF_PKG_CHECK_MODULES_([$1], [$2], m4_shift(m4_shift(m4_shift($@))))dnl
  ])dnl
])dnl
dnl
AC_DEFUN([CF_PKG_CHECK_FLAGS_], [dnl
  ifelse([$3], , , [dnl
    changequote(<<, >>)dnl
    define(<<CF_PKG_CHECK_MODULES_VAR>>, 
           [$1_]translit(<<$3>>, [a-z], [A-Z])) dnl
    changequote([, ])
    AC_MSG_CHECKING([for pkg-config value of flag $3 in $2])
    if test "${CF_PKG_CHECK_MODULES_VAR+set}" = set ; then
      AC_MSG_WARN(CF_PKG_CHECK_MODULES_VAR [set from command line])
      pkg_failed=no
    elif CF_PKG_CHECK_MODULES_VAR[=`$PKG_CONFIG \
			     --define-variable=prefix=\\${$1_PREFIX} \
				 --$3 "$2"`] ; then
    :
    else
      AC_MSG_ERROR([Unable to get variable $3])
    fi
    CF_PKG_CHECK_MODULES_VAR[_BASE=${]CF_PACKAGE_MODULES_VAR[}]
    AC_MSG_RESULT([found, is \`${]CF_PKG_CHECK_MODULES_VAR[}'])
    AC_SUBST(CF_PKG_CHECK_MODULES_VAR)
    CF_PKG_CHECK_MODULES_([$1], [$2], m4_shift(m4_shift(m4_shift($@))))dnl
  ])dnl
])dnl
dnl
AC_DEFUN([CF_PKG_CHECK_MODULES], [dnl
  changequote(<<, >>)dnl
  define(<<CF_PKG_CHECK_MODULES_NAME>>, translit(<<$1>>, [a-z-], [A-Z_])) dnl
  changequote([, ])
  PKG_CHECK_MODULES(CF_PKG_CHECK_MODULES_NAME, [$1])
  if test $pkg_failed = untried ; then
    AC_MSG_ERROR([Failed to find pkg-config])
  fi
  CF_PKG_CHECK_MODULES_(CF_PKG_CHECK_MODULES_NAME, $@)dnl
])dnl
dnl
AC_DEFUN([CF_PKG_CHECK_FLAGS], [dnl
  changequote(<<, >>)dnl
  define(<<CF_PKG_CHECK_MODULES_NAME>>, translit(<<$1>>, [a-z-], [A-Z_])) dnl
  changequote([, ])
  PKG_CHECK_MODULES(CF_PKG_CHECK_MODULES_NAME, [$1])
  if test $pkg_failed = untried ; then
    AC_MSG_ERROR([Failed to find pkg-config])
  fi
  CF_PKG_CHECK_FLAGS_(CF_PKG_CHECK_MODULES_NAME, $@)dnl
])dnl
dnl