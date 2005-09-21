AC_DEFUN([CF_AC_CHECK_HEADER], [AC_CHECK_HEADER([$1], [], [dnl
  AC_MSG_ERROR([Header $1 not found])])dnl
])

AC_DEFUN([CF_AC_CHECK_HEADERS], [dnl
  for cf_ac_hdr in $1 ; do
      AC_CHECK_HEADER($cf_ac_hdr, [cf_ac_hdr_fnd=yes ; break], [])
  done
  if test "x$cf_ac_hdr_fnd" == "x" ; then
	  if ifelse([$4], , [:], [false]); then
		  AC_MSG_ERROR([No header in $1 found])
    else
      AC_MSG_WARN([No header in $1 found])
	  	AC_DEFINE([$4], [1], ["$3 not found"])
    fi
  else
    AC_DEFINE_UNQUOTED([$2], [<$cf_ac_hdr>], [$3])dnl
  fi
])

AC_DEFUN([CF_AC_CHECK_SIZEOF], [dnl
  AC_CHECK_SIZEOF([$1], [0])dnl
  changequote(<<, >>)dnl
  define(<<AC_CV_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
  changequote([, ])dnl
  if test "$AC_CV_NAME" == "0" ; then
    AC_MSG_WARN([sizeof($1) is unknown, confirm that it is $2 on target])
  elif test "$AC_CV_NAME" != "$2" ; then
    AC_MSG_ERROR([sizeof($1) = $AC_CV_NAME, must be $2])
  fi
  undefine([AC_CV_NAME])dnl
])

AC_DEFUN([CF_AC_ARG_ENABLE], [dnl
  AC_ARG_ENABLE([$1], [  --enable-$1=[$2]     Sets $2 (default $4)], 
	[dnl
		case "$enableval" in
		  no|yes) $3="$4";;
			*) $3="$enableval";;
		esac dnl
		], [$3="$4"])
  AC_SUBST([$3])dnl
  AC_DEFINE_UNQUOTED([$3], [$$3], [$2])dnl
])

dnl
dnl CF_DEF_TARGET([target-pattern], [var-to-define])
dnl wildcard matches target-pattern to target, and if matches, var is defined
dnl
AC_DEFUN([CF_DEF_TARGET], [dnl
  AC_MSG_CHECKING([if target is $1])
  case "$target" in 
    $1) 
        AC_DEFINE([$2], [yes], [Target definition])
        AC_SUBST([$2], [yes])
	AC_MSG_RESULT([yes, defining $2]);;
    *)
        AC_MSG_RESULT([no, not defining $2]);;
  esac dnl
])

AC_DEFUN([CF_ON_TARGET], [dnl
  case "$target" in
    $1) $2;;
    *)  $3;;
  esac dnl
])

AC_DEFUN([CF_GCC_COMPILER_OPTION], [dnl
  AC_REQUIRE([AC_PROG_CC])dnl
  AC_REQUIRE([AC_PROG_LIBTOOL])dnl
  if test "x$GCC" == "xyes" ; then
    AC_REQUIRE([LT_AC_PROG_SED])dnl
    AC_MSG_CHECKING([for compiler options $2])
    AC_LANG_PUSH(C)
    CF_GCO_S=
    lt_simple_compile_test_code="int some_variable = 0;\n"
    printf "$lt_simple_compile_test_code" > conftest.$ac_ext
    lt_compiler_flag="$2"
    lt_compile=`echo "$ac_compile" | $SED \
      -e 's:.*FLAGS}? :&$lt_compiler_flag :; t' \
      -e 's: [[^ ]]*conftest\.: $lt_compiler_flag&:; t' \
      -e 's:$: $lt_compiler_flag:'`
    (eval echo "\"\$as_me:__oline__: $lt_compile\"" >&AS_MESSAGE_LOG_FD)
    (eval "$lt_compile" 2>conftest.err)
    ac_status=$?
    cat conftest.err >&AS_MESSAGE_LOG_FD
    echo "$as_me:__oline__: \$? = $ac_status" >&AS_MESSAGE_LOG_FD
    if (exit $ac_status) && test -s "$ac_outfile"; then
      # The compiler can only warn and ignore the option if not recognized
      # So say no if there are warnings other than the usual output.
      $echo "X$_lt_compiler_boilerplate" | $Xsed >conftest.exp
      $SED '/^$/d' conftest.err >conftest.er2
      if test ! -s conftest.err || diff conftest.exp conftest.er2 >/dev/null; then
        CF_GCO_S=yes
      fi
    fi
    $rm conftest*
 
    if test x"$CF_GCO_S" = xyes ; then
        AC_MSG_RESULT([Adding $2 to $1])
        $1="$$1 $2"
    else
        AC_MSG_RESULT([Failed, not adding $2 to $1])
    fi
    AC_LANG_POP(C)
  fi dnl
])

AC_DEFUN([CF_AC_ARG_WITH], [dnl
  AC_ARG_WITH([$1],
              [  --with-$1=[$4]    $2 ($3)],
              [dnl
							  case "$withval" in
								  no|yes) $1='$3';;
									*) eval $1="'$withval'";;
							  esac dnl
							],
              [$1='$3'])
  AC_SUBST([$1])dnl
])

AC_DEFUN([CF_AC_ARG_WITH_IF], [dnl
  AC_ARG_WITH([$1],
              [  --with-$1=[$4]    $2 ($3)],
              [dnl
							  case "$withval" in
								  no|yes) $1='$3';;
									*) eval $1="'$withval'";;
							  esac dnl
							],
              [if test X"${$1}" = X ; then $1='$3' ; fi])
  AC_SUBST([$1])dnl
])

AC_DEFUN([CF_AC_ARG_WITH_DIR], [dnl
  CF_AC_ARG_WITH([$1], [$2], [$3], [dir])dnl
])

AC_DEFUN([CF_WIN_DLL_IMPORT], [dnl
  AC_REQUIRE([AC_CANONICAL_SYSTEM])dnl
  case "$host" in
    *-*-cygwin*|*-*-mingw*)
      if test X"$enable_shared" = Xyes ; then
            AC_TRY_LINK_FUNC([lib$1_is_dll],
	                     [LIB$1_DLL_IMPORT=-DLIB$1_DLL_IMPORT])
      fi
      ;;
  esac
  AC_SUBST(LIB$1_DLL_IMPORT)dnl
])

AC_DEFUN([CF_SET_TOP_DIR], [dnl
  for top_builddir in . .. ../.. $ac_auxdir $ac_auxdir/.. ; do
    test -f $top_builddir/configure && break
  done
  echo $ac_auxdir dnl
])

AC_DEFUN([CF_PREPEND_VARS], [dnl
  for cf_prepend_var in $2 ; do
    eval $1="\"\$$cf_prepend_var \$$1\""
  done
  AC_SUBST($1)dnl
])

AC_DEFUN([CF_SAVE_VAR], [dnl
  cf_save_var_$1="$$1" dnl
])

AC_DEFUN([CF_UNSAVE_VAR], [dnl
  $1="$cf_save_var_$1" dnl
])

AC_DEFUN([CF_VAR_COLLAPSED], [dnl
  CF_SAVE_VAR([prefix])
  CF_SAVE_VAR([exec_prefix])
  if test x"$prefix" = xNONE ; then
    prefix="$ac_default_prefix"
  fi
  if test x"$exec_prefix" = xNONE ; then
    exec_prefix="$prefix"
  fi
dnl
  AC_MSG_CHECKING([value of \${$2}])
  eval eval eval eval eval $1_$2="$$2"
  AC_SUBST([$1_$2])dnl
  AC_MSG_RESULT([setting \${$1_$2} to ${$1_$2}])
dnl
  CF_UNSAVE_VAR([exec_prefix])
  CF_UNSAVE_VAR([prefix])dnl
])

AC_DEFUN([CF_AM_ENABLE], [dnl
	if test x"$enable_$1" = x ; then enable_$1='$3'; fi
  AC_ARG_ENABLE([$1], [  --enable-$1     $2 ($3)])
	AM_CONDITIONAL([$1], [test x"$enable_$1" = x"yes"])dnl
])

AC_DEFUN([CF_AM_PROG], [dnl
  AC_CHECK_PROG([$1], [$2], [$as_dir/$ac_word$ac_exec_ext], [], [$3])
  AM_CONDITIONAL([$1], [test "${$1+set}" = set]) dnl
])

AC_DEFUN([CF_EMACS_ENABLED], [dnl
  AC_REQUIRE([AM_PATH_LISPDIR])
	AC_MSG_CHECKING([if emacs is enabled (\$EMACS != no)])
	if test x"$EMACS" = xno ; then 
    AM_CONDITIONAL([EMACS_ENABLED], [false])
		AC_MSG_RESULT([emacs NOT enabled])
  else
	  AM_CONDITIONAL([EMACS_ENABLED], [:])
		AC_MSG_RESULT([emacs enabled])
  fi dnl
])

AC_DEFUN([CF_SET_EXPR], [dnl
	AC_SUBST([$1])
  $1=`expr $2` dnl
])

AC_DEFUN([CF_SUBST_DEFINE], [dnl
  AC_DEFINE([$1], [$2], [$3])
	AC_SUBST([$1])
	$1='ifelse([$4], ,[$2],[$4])'dnl
])

AC_DEFUN([CF_SUBST_DEFINE_UNQUOTED], [dnl
  AC_DEFINE_UNQUOTED([$1], [$2], [$3])
	AC_SUBST([$1])
	$1="ifelse([$4], ,[$2],[$4])" dnl
])

AC_DEFUN([CF_AC_PATH_XTRA], [dnl
  AC_REQUIRE([AC_PATH_XTRA])
	if test x"$no_x" == xyes ; then
	  AM_CONDITIONAL([X_DISPLAY_MISSING], [:])
		X_LDFLAGS=''
  else
    AM_CONDITIONAL([X_DISPLAY_MISSING], [false])
		X_LDFLAGS='$(X_LIBS) $(X_PRE_LIBS) -lX11 $(X_EXTRA_LIBS)'
  fi
  AC_SUBST(X_DISPLAY_MISSING) 
	AC_SUBST(X_LDFLAGS) dnl
])
    