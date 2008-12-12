dnl ------------------------------------------------------------------
dnl cf_latex: macros && stuff for building pdflatex documentation
dnl include $(top_srcdir)/m4/cf_latex.makefile in Makefile.am's to get
dnl   basic rules
dnl
dnl Call CF_PROG_{PDFLATEX,BIBTEX,MAKEINDEX} to get the proper programs,
dnl   CF_LATEX_BIBSTYLE to guarantee bibstyle availability,
dnl   CF_LATEX_CLASS_{ARTICLE,BOOK} to guarantee class availability,
dnl   CF_LATEX_PACKAGE_AMSMATH to guarantee amsmath availability
dnl   CF_LATEX_PACKAGES to guarantee package availability
dnl   CF_LATEX_PACKAGE_OPT to guarantee package with option availability
dnl
dnl They all depend on m4/latex/... packages.
dnl ------------------------------------------------------------------
dnl
dnl CF_LATEX_PUSH_TEXINPUTS([extra directories])
dnl
AC_DEFUN([CF_LATEX_PUSH_TEXINPUTS], [dnl
  CF_LATEX_PACKAGES_TEXINPUTS="$TEXINPUTS"
  ifelse($1, , , [TEXINPUTS="$1:$TEXINPUTS"; export TEXINPUTS])dnl
])dnl
dnl
dnl CF_LATEX_POP_TEXINPUTS
AC_DEFUN([CF_LATEX_POP_TEXINPUTS], [dnl
  TEXINPUTS="$CF_LATEX_PACKAGES_TEXINPUTS"; export TEXINPUTS dnl
])dnl
dnl
dnl CF_LATEX_PACKAGES([packages,...], [class], [extra directories -- opt])
dnl
AC_DEFUN([CF_LATEX_PACKAGES], [dnl
  CF_LATEX_PUSH_TEXINPUTS([$3])
  m4_foreach([CF_LATEX_PACKAGES_PACKAGE], [$1], [dnl
    AC_LATEX_PACKAGE(CF_LATEX_PACKAGES_PACKAGE, [$2], [CF_LATEX_PACKAGES_TMP])
    if test "$CF_LATEX_PACKAGES_TMP" = "no" ; then
      AC_MSG_CHECKING([for packages $1 in class $2 with tex directories \`$TEXINPUTS'])
      AC_MSG_ERROR(CF_LATEX_PACKAGES_PACKAGE[ not found])
    fi
  ])dnl
  AC_MSG_CHECKING([for packages $1 in class $2 with tex directories \`$TEXINPUTS'])
  AC_MSG_RESULT([found])
  CF_LATEX_POP_TEXINPUTS dnl
])dnl
dnl
dnl CF_LATEX_PACKAGE_OPT([package], [class], [opts], [extra directories -- opt])
dnl
AC_DEFUN([CF_LATEX_PACKAGE_OPT], [dnl
  CF_LATEX_PUSH_TEXINPUTS([$4])
  AC_LATEX_PACKAGE_OPT([$1], [$2], [CF_LATEX_PACKAGE_OPT_TMP], [$3])
  AC_MSG_CHECKING([for package $1 in class $2 with option $3 and tex directories `$TEXINPUTS'])
  if test "$CF_LATEX_PACKAGE_OPT_TMP" = "no" ; then
    AC_MSG_ERROR([not found])
  fi
  AC_MSG_RESULT([found])
  CF_LATEX_POP_TEXINPUTS dnl
])dnl
dnl
dnl
dnl ---- Prog redirects ------
dnl
AC_DEFUN([CF_PROG_PDFLATEX], [AC_PROG_PDFLATEX])dnl
dnl
AC_DEFUN([CF_PROG_BIBTEX], [AC_PROG_BIBTEX])dnl
dnl
AC_DEFUN([CF_PROG_MAKEINDEX], [AC_PROG_MAKEINDEX])dnl
dnl
AC_DEFUN([CF_LATEX_CLASS_ARTICLE], [AC_LATEX_CLASS_ARTICLE])dnl
dnl
AC_DEFUN([CF_LATEX_CLASS_BOOK], [AC_LATEX_CLASS_BOOK])dnl
dnl
dnl CF_LATEX_PACKAGE_AMSMATH
dnl Sets up @amsmath@ AC_SUBST:
dnl  either \usepackage{amsmath} or \usepackage{amsmath,amsfonts}
dnl
AC_DEFUN([CF_LATEX_PACKAGE_AMSMATH], [dnl
  AC_MSG_CHECKING([for package amsmath])
  AC_LATEX_PACKAGE_AMSMATH
  AC_MSG_RESULT([found amsmath: $amsmath])dnl
])dnl
dnl
dnl CF_LATEX_BIBSTYLE([bibstyle], [class opt], [bibpackage opt])
dnl
AC_DEFUN([CF_LATEX_BIBSTYLE], [dnl
  CF_LATEX_BIBSTYLE_([$1],dnl
ifelse([$#], [1],[book],[$2]),dnl
ifelse([$#],[1], [natbib],[$#],[2],[natbib],[$3]))dnl
])dnl
dnl
dnl CF_LATEX_BIBSTYLE_([bibstyle], [class], [bibpackage])
dnl
AC_DEFUN([CF_LATEX_BIBSTYLE_], [dnl
  CF_LATEX_BIBSTYLE__([$1], [$2], [$3],dnl
[cf_cv_latex_bibstyle_]translit($1_$2_$3,[-],[_]))dnl
])dnl
dnl
dnl CF_LATEX_BIBSTYLE__([bibstyle], [class], [bibpackage], [cv_var])
dnl
AC_DEFUN([CF_LATEX_BIBSTYLE__], [dnl
  AC_CACHE_CHECK([for bibstyle $1 in class $2 and package $3],[$4],dnl
[_AC_LATEX_TEST([
\documentclass{$2}
\usepackage{$3}
\bibliographystyle{$1}
\begin{document}
\end{document}
],[$4])])
dnl
  if test "$$4" = "no" ; then
    AC_MSG_ERROR([bibstyle $1 not found])
  fi dnl
])dnl
dnl
