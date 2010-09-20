AC_DEFUN([CF_SEM], [dnl
  CF_AC_CHECK_HEADER([semaphore.h])
  AC_MSG_CHECKING([for sem_open in libraries])
  AC_SEARCH_LIBS([sem_open], [rt pthread], [dnl
    AC_MSG_RESULT([yes, $ac_cv_search_sem_open])
  ], [dnl
    AC_MSG_ERROR([No sem_open found])
  ])dnl
])dnl
