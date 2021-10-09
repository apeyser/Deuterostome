AC_DEFUN([CF_AC_CHECK_HEADER], [AC_CHECK_HEADER([$1], [], [dnl
  AC_MSG_ERROR([Header $1 not found])])dnl
]) dnl
dnl
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
  fi dnl
])dnl
dnl
AC_DEFUN([CF_AC_CHECK_SIZEOF], [dnl
  AC_CHECK_SIZEOF([$1], [0], [$3])dnl
  changequote(<<, >>)dnl
  define(<<AC_CV_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
  changequote([, ])dnl
  if test "$AC_CV_NAME" == "0" ; then
    AC_MSG_WARN([sizeof($1) is unknown, confirm that it is $2 on target])
  elif test "$AC_CV_NAME" != "$2" ; then
    AC_MSG_ERROR([sizeof($1) = $AC_CV_NAME, must be $2])
  fi
  undefine([AC_CV_NAME])dnl
])dnl
dnl
dnl
dnl CF_AC_COMP_SIZEOF([syma], [symb], [headersa], [headersb])
dnl
AC_DEFUN([CF_AC_COMP_SIZEOF], [dnl
  AC_CHECK_SIZEOF([$1], [0], [$3])dnl
  AC_CHECK_SIZEOF([$2], [0], [$4])dnl
  changequote(<<, >>)dnl
  define(<<AC_CV_NAMEA>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
  changequote([, ])dnl
  changequote(<<, >>)dnl
  define(<<AC_CV_NAMEB>>, translit(ac_cv_sizeof_$2, [ *], [_p]))dnl
  changequote([, ])dnl
dnl
  if test "$AC_CV_NAMEA" = "0" ; then
    AC_MSG_WARN([sizeof($1) is unknown, ]dnl
[confirm that it equals sizeof($2) == $AC_CV_NAMEB on target])
  elif test "$AC_CV_NAMEB" = "0" ; then
    AC_MSG_WARN([sizeof($2) is unknown, ]dnl
[confirm that it equals sizeof($1) == $AC_CV_NAMEA on target])
  elif test "$AC_CV_NAMEA" != "$AC_CV_NAMEB" ; then
    AC_MSG_ERROR([sizeof($1) = $AC_CV_NAMEA, sizeof($2) = $AC_CV_NAMEB, ]dnl
[not matching"])
  fi dnl
dnl
  undefine([AC_CV_NAMEA])dnl
  undefine([AC_CV_NAMEB])dnl
])dnl
dnl
dnl
dnl CF_DEF_TARGET([target-pattern], [var-to-define])
dnl wildcard matches target-pattern to target, and if matches, var is defined
dnl
AC_DEFUN([CF_DEF_TARGET], [dnl
  AC_MSG_CHECKING([if target is $1])
  case "$target" in 
    $1) 
        AC_DEFINE([$2], [yes], [Target definition])
        AC_SUBST([$2], [yes])dnl
	AC_MSG_RESULT([yes, defining $2]);;
    *)
        AC_MSG_RESULT([no, not defining $2]);;
  esac dnl
])dnl
dnl
AC_DEFUN([CF_ON_TARGET], [dnl
  case "$target" in
    $1) $2;;
    *)  $3;;
  esac dnl
])dnl
dnl
AC_DEFUN([CF_CLEAR_DEF], [dnl
  if test "${$1-set}" == set ; then 
     $1=""
     cf_cleared_$1=:
  else
     cf_cleared_$1=false 
  fi dnl
])dnl
dnl
AC_DEFUN([CF_IF_UNDEF], [dnl
  AC_MSG_CHECKING([if $1 is set])
  if test "${$1-set}" == set \
     || test "${$1}" == "" \
     && test "$ac_cv_env_$1_set" == "" \
     || test "${cf_cleared_$1-set}" == ":" ; then
    AC_MSG_RESULT([no])
    $2
  else
    AC_MSG_RESULT([yes, `${$1}'])
  fi dnl
])dnl
dnl
AC_DEFUN([CF_LIBTOOL], [dnl
  AC_REQUIRE([CF_LIBTOOL_INT])dnl
  AC_REQUIRE([CF_LTDL_INT])dnl
])dnl
dnl
AC_DEFUN([CF_LIBTOOL_INT], [dnl
  LT_INIT
  AC_SUBST(LIBTOOL_DEPS)
  AC_LIBTOOL_WIN32_DLL
  AC_LIBTOOL_DLOPEN
  AC_LIBLTDL_CONVENIENCE
])dnl
dnl
AC_DEFUN([CF_LTDL_INT], [dnl
  LT_CONFIG_LTDL_DIR([libltdl])dnl
  LTDL_INIT dnl
  AC_SUBST([INCLTDL])dnl
  AC_SUBST([LIBLTDL])dnl
])dnl
dnl
AC_DEFUN([CF_GCC_COMPILER_OPTION], [dnl
  AC_REQUIRE([AC_PROG_CC])dnl
  CF_LIBTOOL dnl
  CF_GCC_COMPILER_OPTION_INT([$1], [GCC], [C], ifelse([$2],[],[GCC_CFLAGS],[$2]))dnl
])dnl
dnl
AC_DEFUN([CF_GXX_COMPILER_OPTION], [dnl
  LT_LANG([C++]) dnl
  AC_REQUIRE([AC_PROG_CXX]) dnl
  dnl AC_REQUIRE([AC_PROG_CXX])dnl
  CF_LIBTOOL dnl
  CF_GCC_COMPILER_OPTION_INT([$1], [GXX], [C++], ifelse([$2],[],[GCC_CXXFLAGS],[$2]))dnl
])dnl
dnl
AC_DEFUN([CF_GCC_COMPILER_OPTION_INT], [dnl
  AC_SUBST([$4])dnl
  if test "x$$2" == "xyes" ; then
    AC_MSG_CHECKING([for $2 compiler options $1])
    AC_LANG_PUSH($3)
    CF_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -Werror $1"
    AC_COMPILE_IFELSE([dnl
      AC_LANG_SOURCE([[int some_variable = 0;]])
    ], [dnl
      AC_MSG_RESULT([Adding])
      $4="$$4 $1"
    ], [
      AC_MSG_RESULT([Failed, not adding])
    ])
    AC_LANG_POP($3)
    CPPFLAGS="$CF_CPPFLAGS"
  else
    AC_MSG_NOTICE([Skipping check for $2 compiler options $1])
  fi dnl
])dnl
dnl
dnl $1=var, $2=code
AC_DEFUN([CF_DEFINE_IF_CODE_GCC], [dnl
  CF_DEFINE_IF_CODE_G([$1], [$2], [C], [GCC])dnl
])dnl
dnl
dnl $1=var, $2=code
AC_DEFUN([CF_DEFINE_IF_CODE_GXX], [dnl
  CF_DEFINE_IF_CODE_G([$1], [$2], [C++], [GXX])dnl
])dnl
dnl internal ^^
AC_DEFUN([CF_DEFINE_IF_CODE_G], [dnl
  if test "x$$4" == "xyes"; then
    AC_MSG_CHECKING([for value of HAVE_$1 for $4])
    AC_LANG_PUSH($3)
    CF_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -Werror"
    AC_COMPILE_IFELSE([dnl
      AC_LANG_SOURCE([[$2]])
    ], [dnl
      AC_DEFINE([HAVE_$1], [1], [Define if you have "$1"])
      AC_MSG_RESULT([HAVE_$1 defined])
    ],[dnl
      AC_MSG_RESULT([HAVE_$1 not defined])
    ])
    CPPFLAGS="$CF_CPPFLAGS"
    AC_LANG_POP($3)
  else
    AC_MSG_NOTICE([Skipping check for value of HAVE_$1 for $4])
  fi dnl
])dnl
dnl
AC_DEFUN([CF_AC_ARG_VAR], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_ARG>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])
  cf_ac_cv_arg_val="CF_AC_CV_ARG"
  AC_MSG_CHECKING([if ${cf_ac_cv_arg_val} is set])
  AC_ARG_VAR(CF_AC_CV_ARG, [$2, default=$3])
  if test "${CF_AC_CV_ARG-set}" == set ; then $1="$3"; fi
  AC_SUBST($1)dnl
  AC_DEFINE_UNQUOTED(CF_AC_CV_ARG, [${$1}], [$2])
  AC_MSG_RESULT([setting to ${$1}])dnl
])dnl
dnl
AC_DEFUN([CF_AC_ARG_VAR_SUBST], [dnl
  changequote(<<, >>)dnl
  define(<<CF_AC_CV_ARG>>, translit($1, [a-z], [A-Z]))dnl
  changequote([, ])
  cf_ac_cv_arg_val="CF_AC_CV_ARG"
  AC_MSG_CHECKING([if ${cf_ac_cv_arg_val} is set])
  AC_ARG_VAR(CF_AC_CV_ARG, [$2, default=$3])
  if test "${CF_AC_CV_ARG-set}" == set ; then $1="$3"; fi
  CF_AC_SUBST([$1])dnl
  AC_MSG_RESULT([setting to ${$1}])dnl
])dnl
dnl
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
])dnl
dnl
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
  AC_SUBST($1)dnl
  AC_DEFINE_UNQUOTED(CF_AC_CV_ARG, ["${$1}"], [$2])dnl
  AC_MSG_RESULT([setting to ${$1}])dnl
])dnl
dnl
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
  AC_SUBST([$1])dnl
  AC_MSG_RESULT([setting to ${$1}])dnl
])dnl
dnl
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
])dnl
dnl
AC_DEFUN([CF_SET_TOP_DIR], [dnl
  for top_builddir in . .. ../.. $ac_auxdir $ac_auxdir/.. ; do
    test -f $top_builddir/configure && break
  done dnl
])dnl
dnl
AC_DEFUN([CF_PREPEND_VARS], [dnl
  for cf_prepend_var in $2 ; do
    eval $1="\"\$$cf_prepend_var \$$1\""
  done
  AC_SUBST($1)dnl
])dnl
dnl
AC_DEFUN([CF_SAVE_VAR], [dnl
  cf_save_var_$1="$$1" dnl
])dnl
dnl
AC_DEFUN([CF_UNSAVE_VAR], [dnl
  $1="$cf_save_var_$1" dnl
])dnl
dnl
AC_DEFUN([CF_AM_CONDITIONAL], [dnl
  AM_CONDITIONAL([ENABLE_$1], [$2]) dnl
])dnl
dnl
AC_DEFUN([CF_AM_ENABLE_DO], [dnl
  if test "${enable_$4-set}" == set ; then enable_$4='$3'; fi
  if test "${enable_$4}" == "yes" ; then enable_$4='$6'; fi
  AC_ARG_ENABLE([$1], [AC_HELP_STRING([--enable-$1], [$2 ($3)])])
  CF_AM_CONDITIONAL($5, [test "${enable_$4-no}" != "no"])
  if test "${enable_$4-no}" == "no" ; then 
    AC_MSG_RESULT([no, not enabled])
  else
    AC_MSG_RESULT([yes, enabled (${enable_$4})])
  fi dnl
])dnl
dnl
dnl $1 = feauture
dnl $2 = comment
dnl $3 = default
dnl $4 = value if value passed is yes (defaults to $3)
AC_DEFUN([CF_AM_ENABLE], [dnl
  AC_MSG_CHECKING([if $1 is enabled])
  changequote(<<, >>)dnl
  define(<<CF_AM_CV_ENABLE>>, 
    patsubst(translit($1, [a-z], [A-Z]), <<->>, <<_>>))dnl
  define(<<CF_AM_CVS_ENABLE>>, patsubst($1, <<->>, <<_>>))dnl
  changequote([, ])dnl
  CF_AM_ENABLE_DO([$1], [$2], [$3], CF_AM_CVS_ENABLE, CF_AM_CV_ENABLE, 
    ifelse([$4], , [[$3]], [[$4]]))dnl
])dnl
dnl
dnl $1 = headers
dnl $2 = prefix
dnl $3 = numeric symbols
dnl $4... = pre-defines
dnl $5 = undefined values
AC_DEFUN([CF_AC_CHECK_SYM], [dnl
  AC_MSG_CHECKING([if macro $3 is defined in headers $1])
  changequote(<<, >>)dnl
  define(<<CF_AC_CHECK_SYM_DEF>>, <<$2>>_<<$3>>)dnl
  changequote([, ])dnl
  incls="$4
"
  for incl in [$1] ; do
    incls="${incls}
#include <${incl}>
"
  done
  AC_TRY_LINK([${incls}],
              [double test = (double) $3;],
	      [cf_ac_check_sym_succ=:],
	      [cf_ac_check_sym_succ=false])
  if $cf_ac_check_sym_succ ; then
    AC_DEFINE(CF_AC_CHECK_SYM_DEF, [$3], [Symbol for glob])
    AC_MSG_RESULT([defined, defining ]CF_AC_CHECK_SYM_DEF[ as $3]);
  else
    AC_DEFINE(CF_AC_CHECK_SYM_DEF, 
              ifelse([$5], , [0], [$5]), 
	      [Symbol for glob])
    AC_MSG_RESULT([not defined, defining ]CF_AC_CHECK_SYM_DEF[ as ]dnl
ifelse([$5], , [0], [$5]))
  fi dnl
])dnl
dnl
AC_DEFUN([CF_AC_DEFINE_IF_ENABLED_DEFINE], [dnl
  case "${enable_$2-no}" in
    yes) cf_ac_define_if_enabled_define=1 ;;
	*) cf_ac_define_if_enabled_define="${enable_$2-no}" ;;
  esac
dnl
  if test "${cf_ac_define_if_enabled_define}" != "no" ; then
    AC_DEFINE_UNQUOTED([ENABLE_$1], [${cf_ac_define_if_enabled_define}], [$3])
  fi dnl
])dnl
dnl
AC_DEFUN([CF_AC_DEFINE_IF_ENABLED_SUBST], [dnl
  AC_SUBST([ENABLE_$1])
  ENABLE_$1="${enable_$2-no}" dnl
])dnl
dnl
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
    CF_AC_DEFINE_IF_ENABLED_CVS)dnl
])
dnl
dnl $1 = feauture
dnl $2 = comment
dnl $3 = default
dnl $4 = value if value passed is yes (defaults to $3)
AC_DEFUN([CF_AC_ENABLE], [dnl
  CF_AM_ENABLE([$1], [$2], [$3], [$4])
  CF_AC_DEFINE_IF_ENABLED([$1], [$2])dnl
])dnl
dnl
AC_DEFUN([CF_IF_ENABLED_DO], [dnl
  ifelse([$2],[],,[dnl
    if test "${enable_$1-no}" != "no" ; then
       $2 
    fi
  ]) dnl
  ifelse([$3],[],,[dnl 
    if test "${enable_$1-no}" = "no" ; then
      $3 
    fi
  ])dnl
])dnl
dnl
dnl $1 = feature
dnl $2 = if enabled != no (and not empty)
dnl $3 = if enabled == no (or is empty)
AC_DEFUN([CF_IF_ENABLED], [dnl
  changequote(<<, >>)dnl
  define(<<CF_IF_ENABLED_CVS>>, patsubst($1, <<->>, <<_>>))dnl
  changequote([, ])dnl
  CF_IF_ENABLED_DO(CF_IF_ENABLED_CVS, [$2], [$3])
])dnl
dnl
AC_DEFUN([CF_AM_PROG], [dnl
  AC_CHECK_PROG([ENABLE_$1], [$2], [$as_dir/$ac_word$ac_exec_ext], [$4], [$3])
  if test -n "$ENABLE_$1" ; then
    AC_DEFINE_UNQUOTED([ENABLE_$1], ["${ENABLE_$1}"], [Path to $1])
  fi
  AM_CONDITIONAL([ENABLE_$1], [test "${ENABLE_$1+set}" = set])dnl
])dnl
dnl
dnl $1 = do if emacs enabled
dnl $2 = do if emacs not enabled (optional)
AC_DEFUN([CF_IF_EMACS_ENABLED], [dnl
  if test x"$EMACS" = xno ; then
    ifelse([$2],[],:,[$2])
  else
    $1
  fi dnl
])dnl
dnl
dnl $1 = do if emacs enabled (optional)
dnl $2 = do if emacs disabled (optional)
AC_DEFUN([CF_EMACS_ENABLED], [dnl
  AC_REQUIRE([AM_PATH_LISPDIR])
  AC_MSG_CHECKING([if emacs is enabled (\$EMACS != no)])
  CF_IF_EMACS_ENABLED([dnl
    CF_AM_CONDITIONAL([EMACS], [:])
    AC_MSG_RESULT([emacs enabled])
    ifelse($1,[],:,$1)dnl
  ], [dnl
    CF_AM_CONDITIONAL([EMACS], [false])
    AC_MSG_RESULT([emacs NOT enabled])
    ifelse($2,[],:,$2)dnl
  ])dnl
])dnl
dnl
dnl $1 = elisp file name (without el extension)
AC_DEFUN([CF_EMACS_SOURCE], [dnl
  AC_MSG_CHECKING([for emacs source file $1])
  CF_IF_EMACS_ENABLED([dnl
    cat >emacs-test <<EOF
(setq emacs-test-fin "el")
(defun emacs-test-dir (dir)
  (setq path (concat 
	      (file-name-as-directory 
	       (expand-file-name 
		(if dir dir default-directory)))
	      "$1."
	      emacs-test-fin))
  (when (file-readable-p path)
    (throw 'emacs-test nil)))

(defun emacs-test-do ()
  (catch 'emacs-test
    (mapc 'emacs-test-dir load-path)
    (setq emacs-test-fin "el.gz")
    (mapc 'emacs-test-dir load-path)
    (error "Source file not found: %s (load-path: %s)" 
	   "$1.el"
	   load-path))
  (princ emacs-test-fin))

(defun emacs-test ()
  (condition-case err (emacs-test-do)
      (error
       (message "%s" (error-message-string err))
       (kill-emacs 1))))

EOF
    if $EMACS -batch -q -l emacs-test -f emacs-test \
        >emacs-test-result 2>/dev/null; then
      fin=`cat emacs-test-result`
      rm emacs-test emacs-test-result
      AC_MSG_RESULT([found])
      if test "$fin" == "el.gz"; then
        AC_CHECK_PROG([gunzip], [gunzip], [gunzip])
	if test -z "$gunzip"; then
	  AC_MSG_ERROR([gunzip not found -- needed for $1])
	fi
      fi
    else
      rm emacs-test emacs-test-result
      AC_MSG_ERROR([not found])
    fi dnl
  ], [dnl
    AC_MSG_ERROR([emacs not enabled])dnl
  ])dnl
])dnl
dnl
AC_DEFUN([CF_SET_EXPR], [dnl
  AC_SUBST([$1])
  $1=`expr $2` dnl
])dnl
dnl
AC_DEFUN([CF_SUBST_DEFINE], [dnl
  AC_DEFINE([$1], [$2], [$3])
  AC_SUBST([$1])
  $1='ifelse([$4], ,[$2],[$4])'dnl
])dnl
dnl
AC_DEFUN([CF_SUBST_DEFINE_UNQUOTED], [dnl
  AC_DEFINE_UNQUOTED([$1], [$2], [$3])
  AC_SUBST([$1])
  $1="ifelse([$4], ,[$2],[$4])" dnl
])dnl
dnl
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
  AC_SUBST(X_LDFLAGS)dnl
])dnl
dnl
AC_DEFUN([CF_ACX_PTHREAD], [dnl
  case x"$cf_acx_pthread_check" in
    x)
      NO_PTHREAD_CFLAGS="${CFLAGS}"
      AC_SUBST([NO_PTHREAD_CFLAGS])dnl
      NO_PTHREAD_CC="${CC}"
      AC_SUBST([NO_PTHREAD_CC])dnl
      NO_PTHREAD_LIBS="${LIBS}"
      AC_SUBST([NO_PTHREAD_LIBS])dnl
      AC_MSG_CHECKING([flags for pthreads])
      ACX_PTHREAD([
        AC_MSG_CHECKING([flags for pthreads])
      	CFLAGS="${CFLAGS} ${PTHREAD_CFLAGS}"
      	CC="${PTHREAD_CC}"
      	LIBS="${PTHREAD_LIBS} ${LIBS}"
      	AC_MSG_RESULT([found])
      	AC_DEFINE([HAVE_PTHREAD], [1],
          [Define if you have POSIX threads libraries and header files.])
        cf_acx_pthread_check="yes"
      ], [
      	AC_MSG_CHECKING([flags for pthreads])
      	AC_MSG_WARN([No threads found])
      	cf_acx_pthread_check="no"
      ])
      ;;
dnl      
    xyes)
      AC_MSG_CHECKING([flags for pthreads])
      AC_MSG_RESULT([found(cached)])
      ;;
dnl
    xno)
      AC_MSG_CHECKING([flags for pthreads])
      AC_MSG_WARN([No threads found(cached)])
      ;;
dnl
    *)
      AC_MSG_CHECKING([flags for pthreads])
      AC_MSG_ERROR([Internal error in caching ($cf_acx_pthread_check)])
      ;;
  esac dnl
])dnl
dnl
AC_DEFUN([CF_ACX_PTHREAD_REQUIRE], [dnl
  CF_ACX_PTHREAD()
  AC_MSG_CHECKING([Checking if required pthreads available])
  if test "$cf_acx_pthread_check" = "yes" ; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_ERROR([$1])
  fi dnl
])dnl
dnl
AC_DEFUN([CF_AC_SUBST], [dnl
  AC_SUBST([$1])dnl
  ifelse([$2], , , [$1="$2"])
])dnl
dnl
AC_DEFUN([CF_AC_SUBST_EVAL], [dnl
  AC_SUBST([$1])dnl
  ifelse([$2], , [eval $1="${$1}"], [eval $1="$2"])dnl
])dnl
dnl
dnl 
dnl CF_BASIC_DEFS
dnl
dnl Sets up dependencies for src/basic-defs.h, calculates NAMEBYTES
dnl  and stores that in src/basic-defs.m4.
dnl AC_SUBST's NAMEBYTES
dnl
AC_DEFUN([CF_BASIC_DEFS], [dnl
  AC_CHECK_SIZEOF([void*])
  AC_DEFINE_UNQUOTED([DM_SIZEOF_VOIDP_], 
                     [$ac_cv_sizeof_voidp], 
                     [sizeof(void*)])
  AC_SUBST([CONFIG_STATUS_DEPENDENCIES], ['$(top_srcdir)/src/basic-defs.h'])dnl
  AC_MSG_CHECKING([for NAMEBYTES value])
  m4_include([m4/basic-defs.m4])
  if test $USE_MAINTAINER_MODE != yes; then
    AC_MSG_RESULT([NAMEBYTES = $NAMEBYTES])
  else
    AC_LANG_PUSH([C])
    AC_COMPUTE_INT([NAMEBYTES_NEW], [NAMEBYTES], [[
      #define DM_BASIC_DEFS_NO_INCLUDES 1
      #include "src/basic-defs.h"
    ]], [
      AC_MSG_ERROR([Unable to compute NAMEBYTES])
    ])
    AC_LANG_POP()
dnl
    if test $NAMEBYTES == $NAMEBYTES_NEW ; then
      AC_MSG_RESULT([unchanged, NAMEBYTES = $NAMEBYTES])
    else
      AC_MSG_RESULT([changed, NAMEBYTES = $NAMEBYTES, NAMEBYTES_NEW = $NAMEBYTES_NEW])
      [NAMEBYTES=$NAMEBYTES_NEW]
      AC_MSG_CHECKING([Updating "$srcdir"/m4/basic-defs.m4])
      if echo "[[NAMEBYTES=$NAMEBYTES_NEW]]" > "$srcdir"/m4/basic-defs.m4
      then
        AC_MSG_RESULT([successful])
      else
        AC_MSG_ERROR([failed])
      fi
    fi
  fi
  AC_SUBST([NAMEBYTES])dnl
])dnl
dnl
dnl 
dnl CF_INSERT_([> or >>], [file], [text], [quote])
dnl
dnl cat text + newline into file -- either using > or >>
dnl
AC_DEFUN([CF_INSERT_], [[cat $1"$2" <<$4_CF_INSERT_EOF_
$3
_CF_INSERT_EOF_]])dnl
dnl
dnl
dnl CF_INSERT([file], [text])
dnl
dnl overwrites file with 'text' + newline
dnl
AC_DEFUN([CF_INSERT], [CF_INSERT_([>], [$1], [$2])])dnl
dnl
dnl
dnl CF_APPEND([file], [text])
dnl
dnl appends 'text' + newline to file
dnl
AC_DEFUN([CF_APPEND], [CF_INSERT_([>>], [$1], [$2])])dnl
dnl
dnl CF_INSERT_LITERAL([file], [text])
dnl
dnl overwrites file with 'text' + newline
dnl The 'literal' series inserts without expansion
dnl
AC_DEFUN([CF_INSERT_LITERAL], [CF_INSERT_([>], [$1], [$2], [\])])dnl
dnl
dnl
dnl CF_APPEND_LITERAL([file], [text])
dnl
dnl appends 'text' + newline to file
dnl The 'literal' series inserts without expansion
dnl
AC_DEFUN([CF_APPEND_LITERAL], [CF_INSERT_([>>], [$1], [$2], [\])])dnl
dnl
AC_DEFUN([CF_C_INLINE], [dnl
  AC_C_INLINE
  if test $ac_cv_c_inline = "no" ; then
    AC_DEFINE([inline], [static])
  else
    AC_DEFINE([HAS_INLINE], [1], [Define to 1 if compiler has inline])
  fi dnl
])dnl
dnl
AC_DEFUN([CF_AC_CHECK_HEADER_WITH], [dnl
  cf_ac_includes_default="${ac_includes_default}"
  for i in $2 ; do
      ac_includes_default="
#include <$i>
"
  done
  AC_CHECK_HEADER([$1], [$3], [$4])
  ac_includes_default="${ac_includes_default}" dnl
])dnl
dnl
AC_DEFUN([CF_PROG_LEX], [dnl
  AC_MSG_CHECKING([for lex or flex])
  AC_REQUIRE([AC_PROG_LEX])
  if test "$LEX" == ":"; then
    AC_MSG_ERROR([No lex or flex found])
  fi
  AC_MSG_RESULT([yes, found $LEX])dnl
])dnl
dnl


    
