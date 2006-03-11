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

AC_DEFUN([CF_IF_UNDEF], [dnl
  AC_MSG_CHECKING([if $1 is set])
  if test "${$1-set}" == set ; then
    AC_MSG_RESULT([no])
    $2
  else
    AC_MSG_RESULT([yes])
  fi
])

AC_DEFUN([CF_GCC_COMPILER_OPTION], [dnl
  AC_REQUIRE([AC_PROG_CC])dnl
  AC_REQUIRE([AC_PROG_LIBTOOL])dnl
  CF_GCC_COMPILER_OPTION_INT([$1],ifelse([$2],[],[CFLAGS],[$2]))
])

AC_DEFUN([CF_GCC_COMPILER_OPTION_INT], [
  AC_SUBST([$2])
  if test "x$GCC" == "xyes" ; then
    AC_REQUIRE([LT_AC_PROG_SED])dnl
    AC_MSG_CHECKING([for compiler options $1])
    AC_LANG_PUSH(C)
    CF_GCO_S=
    lt_simple_compile_test_code="int some_variable = 0;\n"
    printf "$lt_simple_compile_test_code" > conftest.$ac_ext
    lt_compiler_flag="$1"
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
        AC_MSG_RESULT([Adding])
        $2="$$2 $1"
    else
        AC_MSG_RESULT([Failed, not adding])
    fi
    AC_LANG_POP(C)
  fi dnl
])

AC_DEFUN([CF_AC_ARG_VAR], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_ARG>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])
  cf_ac_cv_arg_val="CF_AC_CV_ARG"
  AC_MSG_CHECKING([if ${cf_ac_cv_arg_val} is set])
  AC_ARG_VAR(CF_AC_CV_ARG, [$2, default=$3])
  if test "${CF_AC_CV_ARG-set}" == set ; then $1="$3"; fi
  AC_SUBST($1)
  AC_DEFINE_UNQUOTED(CF_AC_CV_ARG, [${$1}], [$2])
  AC_MSG_RESULT([setting to ${$1}])dnl
])

AC_DEFUN([CF_AC_ARG_VAR_SUBST], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_ARG>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])
  cf_ac_cv_arg_val="CF_AC_CV_ARG"
  AC_MSG_CHECKING([if ${cf_ac_cv_arg_val} is set])
  AC_ARG_VAR(CF_AC_CV_ARG, [$2, default=$3])
  if test "${CF_AC_CV_ARG-set}" == set ; then $1="$3"; fi
  CF_AC_SUBST([$1])
  AC_MSG_RESULT([setting to ${$1}])dnl
])

AC_DEFUN([CF_AC_ARG_VAR_SUBST_EVAL], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_ARG>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])
  cf_ac_cv_arg_val="CF_AC_CV_ARG"
  AC_MSG_CHECKING([if ${cf_ac_cv_arg_val} is set])
  AC_ARG_VAR(CF_AC_CV_ARG, [$2, default=$3])
  if test "${CF_AC_CV_ARG-set}" == set ; then $1="$3"; fi
  CF_AC_SUBST_EVAL([$1])
  AC_MSG_RESULT([setting to ${$1}])dnl
])

AC_DEFUN([CF_AC_ARG_VAR_QUOTE], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_ARG>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])
  cf_ac_cv_arg_val="CF_AC_CV_ARG"
  AC_MSG_CHECKING([if ${cf_ac_cv_arg_val} is set])
  AC_ARG_VAR(CF_AC_CV_ARG, [$2, default=$3])
  if test "${CF_AC_CV_ARG+set}" == set ; then 
     $1="${CF_AC_CV_ARG}"
  else
     CF_AC_CV_ARG="$3"
  fi
  if test "${$1-set}" == set ; then $1="${CF_AC_CV_ARG}" ; fi
  AC_SUBST($1)
  AC_DEFINE_UNQUOTED(CF_AC_CV_ARG, ["${$1}"], [$2])dnl
  AC_MSG_RESULT([setting to ${$1}])dnl
])

AC_DEFUN([CF_AC_ARG_DIR], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_DIR>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])dnl
  cf_ac_cv_dir_val="CF_AC_CV_DIR"
  AC_MSG_CHECKING([if ${cf_ac_cv_dir_val} is set])
  AC_ARG_VAR(CF_AC_CV_DIR, [$2, default=$3])
  if test "${CF_AC_CV_DIR-set}" == set ; then 
    $1="$3"
  else
    $1="${CF_AC_CV_DIR}"
  fi
  AC_SUBST([$1])
  AC_MSG_RESULT([setting to ${$1}])dnl
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

AC_DEFUN([CF_AM_CONDITIONAL], [dnl
  AM_CONDITIONAL([ENABLE_$1], [$2]) dnl
])

AC_DEFUN([CF_AM_ENABLE_DO], [dnl
  if test "${enable_$4-set}" == set ; then enable_$4='$3'; fi
  if test x"${enable_$4}" == x"yes" ; then enable_$4='$3'; fi
  AC_ARG_ENABLE([$1], [AC_HELP_STRING([--enable-$1], [$2 ($3)])])
  CF_AM_CONDITIONAL($5, [test "${enable_$4-no}" != "no"])
  if test "${enable_$4-no}" == "no" ; then 
    AC_MSG_RESULT([no, not enabled])
  else
    AC_MSG_RESULT([yes, enabled (${enable_$4})])
  fi dnl
])

AC_DEFUN([CF_AM_ENABLE], [dnl
  AC_MSG_CHECKING([if $1 is enabled])
  changequote(<<, >>)dnl
  define(<<CF_AM_CV_ENABLE>>, 
    patsubst(translit($1, [a-z], [A-Z]), <<->>, <<_>>))dnl
  define(<<CF_AM_CVS_ENABLE>>, patsubst($1, <<->>, <<_>>))dnl
  changequote([, ])dnl
  CF_AM_ENABLE_DO([$1], [$2], [$3], CF_AM_CVS_ENABLE, CF_AM_CV_ENABLE, )dnl
])

AC_DEFUN([CF_AC_DEFINE_IF_ENABLED_DEFINE], [dnl
  case "${enable_$2-no}" in
    yes) cf_ac_define_if_enabled_define=1 ;;
	*) cf_ac_define_if_enabled_define="${enable_$2-no}" ;;
  esac
  dnl
  if test "${cf_ac_define_if_enabled_define}" != "no" ; then
    AC_DEFINE_UNQUOTED([ENABLE_$1], [${cf_ac_define_if_enabled_define}], [$3])
  fi
  dnl
])

AC_DEFUN([CF_AC_DEFINE_IF_ENABLED_SUBST], [dnl
  AC_SUBST([ENABLE_$1])
  ENABLE_$1="${enable_$2-no}"
])

AC_DEFUN([CF_AC_DEFINE_IF_ENABLED], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_DEFINE_IF_ENABLED_CV>>, 
    patsubst(translit($1, [a-z], [A-Z]), <<->>, <<_>>))dnl
  define(<<CF_AC_DEFINE_IF_ENABLED_CVS>>, patsubst($1, <<->>, <<_>>))dnl
  changequote([, ])dnl
  CF_IF_ENABLED([$1], [
	CF_AC_DEFINE_IF_ENABLED_DEFINE(CF_AC_DEFINE_IF_ENABLED_CV, 
      CF_AC_DEFINE_IF_ENABLED_CVS, [$2])
  ])
  CF_AC_DEFINE_IF_ENABLED_SUBST(CF_AC_DEFINE_IF_ENABLED_CV, 
    CF_AC_DEFINE_IF_ENABLED_CVS)
])

AC_DEFUN([CF_AC_ENABLE], [dnl
  CF_AM_ENABLE([$1], [$2], [$3])
  CF_AC_DEFINE_IF_ENABLED([$1], [$2])
])

AC_DEFUN([CF_IF_ENABLED_DO], [
  ifelse([$2],[],,[dnl
    if test x"${enable_$1-no}" != x"no" ; then 
       $2 
    fi
  ]) dnl
  ifelse([$3],[],,[dnl 
    if test x"${enable_$1-no}" == x"no" ; then 
      $3 
    fi
  ])dnl
])

AC_DEFUN([CF_IF_ENABLED], [dnl
  changequote(<<, >>)dnl
  define(<<CF_IF_ENABLED_CVS>>, patsubst($1, <<->>, <<_>>))dnl
  changequote([, ])dnl
  CF_IF_ENABLED_DO(CF_IF_ENABLED_CVS, [$2], [$3])
])

AC_DEFUN([CF_AM_PROG], [dnl
  AC_CHECK_PROG([$1], [$2], [$as_dir/$ac_word$ac_exec_ext], [], [$3])
  AM_CONDITIONAL([$1], [test "${$1+set}" = set]) dnl
])

AC_DEFUN([CF_EMACS_ENABLED], [dnl
  AC_REQUIRE([AM_PATH_LISPDIR])
  AC_MSG_CHECKING([if emacs is enabled (\$EMACS != no)])
  if test x"$EMACS" = xno ; then 
  AM_CONDITIONAL([ENABLE_EMACS], [false])
    AC_MSG_RESULT([emacs NOT enabled])
  else
	AM_CONDITIONAL([ENABLE_EMACS], [:])
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
    
AC_DEFUN([CF_ACX_PTHREAD], [dnl
  NO_PTHREAD_CFLAGS="${CFLAGS}"
  AC_SUBST([NO_PTHREAD_CFLAGS])
  NO_PTHREAD_CC="${CC}"
  AC_SUBST([NO_PTHREAD_CC])
  NO_PTHREAD_LIBS="${LIBS}"
  AC_SUBST([NO_PTHREAD_LIBS])
  CF_IF_ENABLED([$1], [
    ACX_PTHREAD([
	  AC_MSG_CHECKING([flags for pthreads])
      CFLAGS="${CFLAGS} ${PTHREAD_CFLAGS}"
	  CC="${PTHREAD_CC}"
      LIBS="${PTHREAD_LIBS} ${LIBS}"
      AC_MSG_RESULT([found])
      AC_DEFINE([HAVE_PTHREAD], [1], 
        [Define if you have POSIX threads libraries and header files.])
    ], [  
	  AC_MSG_CHECKING([flags for pthreads])
      AC_MSG_ERROR([No threads found, disable threads])])
  ], [
   AC_MSG_CHECKING([flags for pthreads])
   AC_MSG_RESULT([$1 disabled])
  ])
])

AC_DEFUN([CF_AC_SUBST], [dnl
  AC_SUBST([$1])
  ifelse([$2], , , [$1="$2"])
])

AC_DEFUN([CF_AC_SUBST_EVAL], [dnl
  AC_SUBST([$1])
  ifelse([$2], , [eval $1="${$1}"], [eval $1="$2"])
])

AC_DEFUN([CF_M4_PLUGIN_], [dnl
  AC_CONFIG_COMMANDS([$2/$3], [dnl
    if m4 -P "-I${srcdir}/$2" \
      -DPLUGIN_NAME="$1" \
      -DPLUGIN_HEADER=$5 \
      -DPLUGIN_LOCAL=$6 \
      "$7/plugin.m4" >"$2/$3.tmp" \
      && mv "$2/$3.tmp" "$2/$3"  ; then
      :
    else
      rm "$2/$3.tmp" 
      AC_MSG_ERROR([Unable to build "$2/$3"])
    fi
  ])dnl
])

AC_DEFUN([CF_M4_PLUGIN], [dnl
  CF_M4_PLUGIN_([$1], [plugins], [$1.c], dnl
                [$1.plugin], [no], [yes], [${srcdir}/plugins])
  CF_M4_PLUGIN_([$1], [plugins], [$1.h], dnl
                [$1.plugin], [yes], [yes], [${srcdir}/plugins])
])
