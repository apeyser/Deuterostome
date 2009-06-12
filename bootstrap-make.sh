#!/bin/bash

INSTALL=false
BOOTSTRAP=false
CONFIG=false
for i ; do
    case "$i" in
	--install)   INSTALL=:   ;;
	--bootstrap) BOOTSTRAP=: ;;
	--config)    CONFIG=:    ;;
	*) break ;;
    esac
    shift
done

! $BOOTSTRAP || ./bootstrap "$@"

! $CONFIG || ./configure --enable-plugins --enable-plugins-support \
    --enable-maintainer-mode --enable-petsc --enable-rthreads \
    --with-petsc=/opt/petsc/ --with-petsc-arch=linux_deb_d \
    CPPFLAGS="-Wall -g -O2"

for i in clean gencode clean -j9 clean distcheck clean -j9; do
    make $i || exit $?
done

! $INSTALL || make install

