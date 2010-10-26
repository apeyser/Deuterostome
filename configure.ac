AC_INIT([D-Machine], [4.6.5], [Alexander Peyser <peyser.alex@gmail.com>], [dm])
AC_CONFIG_MACRO_DIR([m4 libltdl/m4])
AC_CANONICAL_SYSTEM
AC_CONFIG_AUX_DIR([libltdl/config])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([config.h])
AX_PREFIX_CONFIG_H([src/dm-config.h])
AX_PREFIX_CONFIG_H([src/codegen/dm-config.h])
AM_PATH_LISPDIR
AM_MAINTAINER_MODE

AX_SET_VERSION_INFO

CF_CLEAR_DEF([CFLAGS])

CF_AC_ENABLE([xps], [compile or install only new xps], [yes])
CF_AC_ENABLE([regex], [include regex], [yes])

CF_AC_ARG_VAR([SYS_DICT_SIZE], [system dictionary size], [500])
CF_AC_ARG_VAR([MAX_NUM_OPDS], [maximum operand stack size in frames], [100000])
CF_AC_ARG_VAR([MAX_NUM_DICTS], [maximum dict stack size in frames], [10000])
CF_AC_ARG_VAR([MAX_NUM_EXECS], [maximum exec stack size in frame], [5000])
CF_AC_ARG_VAR([MAX_MEM_SIZE], [maximum vm allocation size in mb], [4000])
CF_AC_ARG_VAR([MAX_USER_DICT_SIZE], [maximum user dict size], [5000])

CF_AC_ARG_VAR([DVT_NUM_OPDS], 
              [dvt operand stack size in frames], [1000])
CF_AC_ARG_VAR([DVT_NUM_DICTS], 
              [dvt dict stack size in frames], [20])
CF_AC_ARG_VAR([DVT_NUM_EXECS], [dvt exec stack size in frame], [100])
CF_AC_ARG_VAR([DVT_MEM_SIZE], [dvt vm allocation size in mb], [10])
CF_AC_ARG_VAR([DVT_USER_DICT_SIZE], [dvt user dict size], [500])

CF_AC_ARG_VAR([DM_IPPORT_USERRESERVED],
              [Symbol or integer value to use for default port base if dnode not in services], 
	      [IPPORT_USERRESERVED])
CF_AC_ARG_VAR([DM_IPPORT_USERRESERVED_STANDARD],
	      [Fail if DM_IPPORT_USERRESERVED_SET != DM_IPPORT_USERRESERVED],
	      [5000])

CF_AC_ARG_VAR([ROLLBITS], [unroll math loops to 2^rollbits], [3])
CF_AC_ARG_VAR([BYTECORRECT], [align to this many bytes], [8])
CF_AC_ENABLE([debug-dmnum], [enable extra debug in dmnum], [no], [yes])

CF_AM_ENABLE([daemon], [compile and install daemonizer], [no], [yes])
CF_AM_ENABLE([setuid], [compile or install setuid wrapper], [yes])
CF_AC_ENABLE([threads], 
             [compile and install threads, value is default number], 
             [1])
CF_IF_ENABLED([threads], [
  CF_AC_ARG_VAR([THREADNUM], [max number of threads], [8])
  CF_ON_TARGET([*-*-darwin*], [
    CF_AC_ARG_VAR([THREADMUL], [min unrolls per thread], [560])
  ], [
    CF_AC_ARG_VAR([THREADMUL], [min unrolls per thread], [56])
  ])
])

CF_AC_ENABLE([rthreads], [Activate mpi], [yes])
CF_IF_ENABLED([threads], [CF_ACX_PTHREAD_REQUIRE([no, please disable threads])])
CF_IF_ENABLED([rthreads], [
  CF_ACX_PTHREAD_REQUIRE([no, please disable rthreads])
])

CF_AC_ARG_DIR([pkgdatadir], [location for data files], 
  [${datadir}/${PACKAGE}])
CF_AC_ARG_DIR([dmstartdir], [location for dm scripts], [${pkgdatadir}])
CF_AC_ARG_DIR([dmconfdir], [locations for per-host configuration],
  [${sysconfdir}/${PACKAGE}])
CF_AC_ARG_DIR([m4datadir], [location for m4 files],
  [${pkgdatadir}/m4])
CF_AC_ARG_DIR([plugindatadir], [location for plugin compile files],
  [${pkgdatadir}/plugin])
CF_AC_ARG_DIR([pkgpsdatadir], [location for ps files], 
  [${pkgdatadir}/ps])
CF_AC_ARG_DIR([pkglispdir], [location for lisp files],
  [${lispdir}/${PACKAGE}])
CF_AC_ARG_DIR([tmp], [location of temp dir], [/tmp])
CF_AC_ARG_DIR([real_tmp], [location of real temp dir], [${tmp}])
CF_AC_ARG_DIR([pkgconfiglibdir], [location for pkg-config pc files],
  [${libdir}/pkgconfig])
CF_AC_ARG_DIR([pkglibdir], [location for plugins], [${libdir}/${PACKAGE}])
CF_AC_ARG_DIR([pkglibexecdir], [location for internal executables], 
  [${libexecdir}/${PACKAGE}])
CF_AC_ARG_DIR([pkgincludedir], [location for package includes], 
  [${includedir}/${PACKAGE}])

CF_AC_ENABLE([unix-sockets], [compile and install unix domain sockets], [yes])
CF_IF_ENABLED([unix-sockets], [
  CF_AC_ARG_DIR([dmsockdir], 
    [location for dm unix sockets], 
    [${real_tmp}/${PACKAGE}/sockets])
])

CF_AC_ENABLE([doc], [compile and install documentation], [yes])
CF_IF_ENABLED([doc], [
  CF_PROG_PDFLATEX
  CF_PROG_BIBTEX
  CF_PROG_MAKEINDEX
  CF_LATEX_BIBSTYLE([plainnat])
  CF_LATEX_CLASS_ARTICLE
  CF_LATEX_CLASS_BOOK
  CF_LATEX_PACKAGE_AMSMATH
  CF_LATEX_PACKAGES([array,makeidx,supertabular,pdf,options,hyperref,newbook,caption,calc,avant,mathpazo,relsize], 
                    [book], [`cd $srcdir; pwd -P`/doc])
  CF_LATEX_PACKAGES([pdf,hyperref,natbib,redefs], 
                    [article], [`cd $srcdir; pwd -P`/doc])
  CF_LATEX_PACKAGE_OPT([geometry], [article], [pdftex])
  CF_LATEX_PACKAGES([verbdef], [book])
  CF_LATEX_PACKAGES([verbdef], [article])
])

AC_PROG_CC
gl_EARLY
AM_PROG_CC_C_O
AC_PROG_CC_STDC
AC_C_CONST
AC_C_RESTRICT
CF_C_INLINE
CF_BASIC_DEFS

CF_AC_ENABLE([plugins-support], [compile or install plugin support], [yes])
CF_AM_ENABLE([plugins], [compile or install plugins], [yes])
CF_AM_ENABLE([cppplugin], 
             [compile or install cpp memory library for plugins], 
	     [no], [yes])
CF_AM_ENABLE([dmflex], 
             [compile or install dmflex plugin], 
             [no], [yes])
CF_AC_ENABLE([sem],
	     [compile d with cross-process semaphores],
	     [yes])

CF_AC_ARG_VAR_SUBST_EVAL([STRINGBUF], 
      [Max buffer size for dmflex, must be at least $NAMEBYTES + 2], 
      [8092])
CF_AC_SUBST_EVAL([NAMEBYTES_1], [`expr $NAMEBYTES - 1`])

CF_IF_ENABLED([plugins], [
  CF_IF_ENABLED([dmflex], [
    CF_PROG_LEX
  ])
])

CF_IF_ENABLED([sem], [
  CF_SEM
  CF_AC_ARG_VAR([SEM_DNODE_NAME], 
                [name for semaphore shared by processes (without leading slash)],
	        [dnode])
])

CF_ON_TARGET([*-*-darwin*], [
  AC_CHECK_PROG([guipdf], [launch], [launch -i com.apple.Preview])
])
AC_CHECK_PROG([guipdf], [xdg-open], [xdg-open])
AC_CHECK_PROG([guipdf], [kfmclient], [kfmclient exec])
AC_CHECK_PROG([guipdf], [gnome-open], [gnome-open])
AC_CHECK_PROG([guipdf], [kpdf], [kpdf])
AC_CHECK_PROG([guipdf], [xpdf], [xpdf])
AC_CHECK_PROG([guipdf], [gv], [gv])
AC_CHECK_PROG([guipdf], [gs], [gs])

CF_ATLAS()
CF_IF_ENABLED([rthreads], [
  AC_ARG_WITH([dpawn-command],
              [Program and arguments (before dpawn) to run ]
[dpawn under (like valgrind)],
              [dpawn_command="$withval"])
  CF_AC_SUBST([DPAWN_COMMAND], [$dpawn_command])

  CF_MPI([], [
    AC_MSG_ERROR([No mpi found... needed for rthreads])
  ])
], [AM_CONDITIONAL([ENABLE_MPI], [false])])
CF_PETSC()
CF_IF_ENABLED([petsc], [
  CF_CHECK_PETSC_SIZEOF([PetscInt], [4])
  CF_CHECK_PETSC_SIZEOF([PetscScalar], [8])
])

CF_AM_PROG([BASH], [bash])
CF_AM_PROG([ENV], [env], [/usr/bin])
CF_AM_PROG([PERL], [perl])
CF_AM_PROG([TAIL], [tail])
CF_AM_PROG([XTERM], [xterm])
CF_AM_PROG([LP], [lp])
CF_AM_PROG([ATPS], [a2ps])
CF_AM_PROG([PSTPDF], [ps2pdf])
CF_AM_PROG([EPSTPDF], [epstopdf])
CF_AM_PROG([PDFTPS], [pdf2ps])
CF_AM_PROG([DVIPDF], [dvipdf])
CF_AM_PROG([XDVI], [xdvi])
CF_AM_PROG([GS], [gs])
CF_AM_PROG([PDFLATEX], [pdflatex])
CF_AM_PROG([LATEX], [latex])
CF_AM_PROG([EMACSCLIENT], [emacsclient])

CF_AC_ARG_VAR_QUOTE([guipdf],
 [name of program to display pdf's, eg gnome-open or launch -i com.apple.Preview],
 [xpdf])

CF_EMACS_ENABLED([
  CF_EMACS_SOURCE([server])
])

AC_CHECK_HEADERS([unistd.h])
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LN_S

CF_SET_TOP_DIR

CF_LIBTOOL

AC_CHECK_LIBM
CF_X11_ENABLE
CF_PREPEND_VARS([LIBS], [XLIB LIBM])
AC_C_BIGENDIAN
dnl check pid_t for plugins/process.c

CF_IF_ENABLED([process], [CF_AC_CHECK_SIZEOF([pid_t], [4])])
CF_IF_ENABLED([process], [CF_AC_CHECK_SIZEOF([int], [4])])

AC_DEFINE([_POSIX_C_SOURCE], [200112L], [Define to 200112L - we are POSIXY])
AC_DEFINE([_XOPEN_SOURCE], [(_XOPEN_SOURCE < 500 ? 500 : _XOPEN_SOURCE)], 
  [Define to >500 - we are POSIXY])

AC_CHECK_HEADERS([features.h])
CF_AC_CHECK_HEADERS([endian.h machine/endian.h], [ENDIAN_HDR], 
  [Header for endianness], [NO_ENDIAN_HDR])
CF_AC_CHECK_HEADER([math.h])
CF_AC_CHECK_HEADER([stdlib.h])
CF_AC_CHECK_HEADER([stdio.h])
CF_AC_CHECK_HEADER([sys/socket.h])
CF_AC_CHECK_HEADER([limits.h])
CF_AC_CHECK_HEADER([sys/un.h])
CF_AC_CHECK_HEADER([sys/select.h])
CF_AC_CHECK_HEADER([netinet/in.h])
CF_AC_CHECK_HEADER([errno.h])
CF_AC_CHECK_HEADER([netdb.h])
CF_AC_CHECK_HEADER([fcntl.h])
CF_AC_CHECK_HEADER([unistd.h])
CF_AC_CHECK_HEADER([string.h])
CF_AC_CHECK_HEADER([dirent.h])
CF_AC_CHECK_HEADER([time.h])
CF_AC_CHECK_HEADER([sys/stat.h])
CF_AC_CHECK_HEADER([fnmatch.h])
CF_AC_CHECK_HEADER([sys/types.h])
CF_AC_CHECK_HEADER([signal.h])
CF_AC_CHECK_HEADER([time.h])
CF_AC_CHECK_HEADER([sys/time.h])
CF_AC_CHECK_HEADER([sys/resource.h])
CF_X11_ENABLED([
  CF_AC_CHECK_HEADER([X11/Xlib.h])
  CF_AC_CHECK_HEADER([X11/Xutil.h])
])
CF_AC_CHECK_HEADER([strings.h])
CF_AC_CHECK_HEADER([inttypes.h])
CF_AC_CHECK_HEADER([sys/wait.h])
CF_IF_ENABLED([plugins-support], [CF_AC_CHECK_HEADER([stdarg.h])])
CF_IF_ENABLED([regex], [CF_AC_CHECK_HEADER([regex.h])])
CF_IF_ENABLED([daemon], [
  AC_CHECK_HEADERS([sys/prctl.h])
  AC_CHECK_FUNCS([prctl])
])

AC_CHECK_FUNCS([setsid])
AC_CHECK_FUNCS([srandomdev])

CF_IF_UNDEF([CFLAGS], [
  CF_ON_TARGET([*-pc-cygwin], [], [
    CF_GCC_COMPILER_OPTION([-g])
  ])
  CF_GCC_COMPILER_OPTION([-O2])
])

AX_CFLAGS_STRICT_PROTOTYPES()

CF_GCC_COMPILER_OPTION([-ftree-vectorize])
CF_GCC_COMPILER_OPTION([-fno-strict-aliasing])

CF_GCC_COMPILER_OPTION([-pipe], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fno-unsigned-char], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-ffast-math], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fno-finite-math-only], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fno-unsafe-math-optimizations], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-Wno-pointer-sign], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-fprefetch-loop-arrays], [GCC_CFLAGS])
CF_GCC_COMPILER_OPTION([-std=gnu99])

CF_IF_ENABLED([cppplugin], [
  CF_IF_UNDEF([CXXFLAGS], [
    CF_ON_TARGET([*-pc-cygwin], [], [
      CF_GXX_COMPILER_OPTION([-g])
    ])
    CF_GXX_COMPILER_OPTION([-O2])
  ])

  CF_GXX_COMPILER_OPTION([-ftree-vectorize])
  CF_GXX_COMPILER_OPTION([-fno-strict-aliasing])

  CF_GXX_COMPILER_OPTION([-pipe], [GCC_CXXFLAGS])
  CF_GXX_COMPILER_OPTION([-fno-unsigned-char], [GCC_CXXFLAGS])
  CF_GXX_COMPILER_OPTION([-ffast-math], [GCC_CXXFLAGS])
  CF_GXX_COMPILER_OPTION([-fno-finite-math-only], [GCC_CXXFLAGS])
  CF_GXX_COMPILER_OPTION([-fno-unsafe-math-optimizations], [GCC_CXXFLAGS])
  CF_GXX_COMPILER_OPTION([-Wno-pointer-sign], [GCC_CXXFLAGS])
  CF_GXX_COMPILER_OPTION([-fprefetch-loop-arrays], [GCC_CXXFLAGS])
])

CF_DEFINE_IF_CODE_GCC([HOT], [__attribute__ ((__hot__)) void x(void) {}])
CF_DEFINE_IF_CODE_GCC([UNUSED], [__attribute__ ((__unused__)) void x(void) {}])
CF_DEFINE_IF_CODE_GCC([NORETURN], [
  #include <stdlib.h>
  __attribute__ ((__noreturn__)) void x(void) {exit(0);}
])

CF_WIN_DLL_IMPORT([DM])
CF_WIN_DLL_IMPORT([PLUGIN])

LIB_SOCKET_NSL

CF_AC_SUBST([PCLIBS], [ ${LTLIBOBJS} -ldmglobals])
CF_AC_SUBST([PCCFLAGS], [${CFLAGS} ${GCC_CFLAGS} ${LIBDM_DLL_IMPORT} ${X_CFLAGS}])
CF_AC_SUBST([PCLDFLAGS], [-module -no-undefined])
CF_IF_ENABLED([cppplugin], [
  CF_AC_SUBST([PCCXXFLAGS], 
    [${CXXFLAGS} ${GCC_CXXFLAGS} ${LIBDM_DLL_IMPORT} ${X_CFLAGS}])
], [
  CF_AC_SUBST([PCCXXFLAGS])
  PCCXXFLAGS=''
])

gl_INIT

AC_CONFIG_LINKS([
  src/dsp1f.h:src/codegen/dsp1f.h.built
  src/dmnuminc.h:src/codegen/dmnuminc.h.built
  src/dsp2def.h:src/codegen/dsp2def.h.built
  src/dgen_0.h:src/codegen/dgen_0.h.built
  src/dnode_0.h:src/codegen/dnode_0.h.built
  src/dpawn_0.h:src/codegen/dpawn_0.h.built
  src/dm-errs.h:src/codegen/dm-errs.h.built
  src/dvt_0.h:src/codegen/dvt_0.h.built
  src/codegen/dm.h:src/dm.h
  src/codegen/dmglobals.c:src/dmglobals.c
  src/codegen/dsp1.c:src/dsp1.c
  src/codegen/dm1.c:src/dm1.c
  src/codegen/dm2.c:src/dm2.c
  src/codegen/dm2.h:src/dm2.h
  src/codegen/dm3.h:src/dm3.h
  src/codegen/dm-prop.h:src/dm-prop.h
  src/codegen/dm-signals.h:src/dm-signals.h
  src/codegen/dm-vm.h:src/dm-vm.h
  src/codegen/dm-swapbytes.h:src/dm-swapbytes.h
  src/codegen/dm-conv.c:src/dm-conv.c
  src/codegen/dm3.c:src/dm3.c
  src/codegen/dm-prop.cin:src/dm-prop.cin
  src/codegen/dm-signals.c:src/dm-signals.c
  src/codegen/dm-vm.c:src/dm-vm.c
  src/codegen/dm4.c:src/dm4.c
  src/codegen/dm5.c:src/dm5.c
  src/codegen/dm5.h:src/dm5.h
  src/codegen/dm6.c:src/dm6.c
  src/codegen/dm6.h:src/dm6.h
  src/codegen/dm7.c:src/dm7.c
  src/codegen/dm8.c:src/dm8.c
  src/codegen/dm8.h:src/dm8.h
  src/codegen/dmnum.c:src/dmnum.c
  src/codegen/dsp2.c:src/dsp2.c
  src/codegen/dm-types.h:src/dm-types.h
  src/codegen/dm-types.c:src/dm-types.c
  src/codegen/paths.hin:src/paths.hin
  src/codegen/dgen.c:src/dgen.c
  src/codegen/dgen_1.h:src/dgen_1.h
  src/codegen/dgen-build:src/dgen-build
  src/codegen/dmnum.h:src/dmnum.h
  src/codegen/dm-nextevent.c:src/dm-nextevent.c
  src/codegen/dm-nextevent.h:src/dm-nextevent.h
  plugins/plugin.d:dcode/plugin.d
  dcode/errors.d:src/codegen/errors.d.built
  plugins/startup_dgen.d:src/codegen/startup_dgen.d
  emacs/d-mode-ops.el:src/codegen/d-mode-ops.el
  plugins/new-plugin.din:dcode/new-plugin.din
  doc/dvt-3.0.bib:doc/dvt.bib
  doc/dvt-emacs.bib:doc/dvt.bib
  doc/newbook.bib:doc/dvt.bib
])

AC_CONFIG_FILES([
  Makefile 
  src/Makefile 
  src/codegen/Makefile 
  src/dm.pc
  plugins/Makefile plugins/dmflex.l 
  ps/Makefile 
  sh/Makefile
  emacs/Makefile
  dcode/Makefile 
  test/Makefile
  m4/Makefile
  test/arr.d
  lib/Makefile
  doc/Makefile
])

AC_OUTPUT
