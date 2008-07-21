# Copyright 1999-2007 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

#EAPI="prefix"

inherit eutils elisp-common

# Short one-line description of this package.
DESCRIPTION="The World Famous D-Machine"

# Homepage, not used by Portage directly but handy for developer reference
HOMEPAGE="http://foo.bar.com/"

# Point to any required sources; these will be automatically downloaded by
# Portage.
#SRC_URI=""
SRC_URI="rsync://deli1/gentoo-local/distfiles/${P}.tar.gz"
#ESVN_REPO_URI="svn://klutz0.nonnerlab/svn/dm-4.0/trunk"

# License of the package.  This must match the name of file(s) in
# /usr/portage/licenses/.  For complex license combination see the developer
# docs on gentoo.org for details.
LICENSE="GPL-2"

SLOT="0"

KEYWORDS="~x86 ~amd64 ~ppc"

IUSE="daemon emacs atlas setuid threads formats xclient X memory xauth petsc mpi"

# A space delimited list of portage features to restrict. man 5 ebuild
# for details.  Usually not needed.
RESTRICT="strip mirror primaryuri"

DEPEND="
emacs? (virtual/emacs)
formats? (app-text/a2ps)
formats? (virtual/ghostscript)
formats? (dev-lang/perl)
X? (app-text/xdvik)
X? (x11-terms/xterm)
X? (x11-base/xorg-server)
atlas? (sci-libs/blas-atlas)
xclient? (x11-libs/libX11)
xauth? (x11-libs/libXext)
mpi? (virtual/mpi)
mpi? (!sys-cluster/mpich)
petsc? (>=sci-mathematics/petsc-2.3.3_p11)
sys-libs/glibc
"

RDEPEND="${DEPEND}"

add_myconf() {
	myconf=("${myconf[@]}" "$@")
}

add_use() {
	local on_flag="$1"; shift
	local off_flag="$1"; shift
	local state=false
	for i ; do
		use $i && state=:
	done

	if $state ; then
		add_myconf "$on_flag"
	else
		add_myconf "$off_flag"
	fi
}

add_with() {
	local flag=$1; shift
	local val=$1; shift
	if [[ -n $val ]] ; then
		add_use --with-$flag="$val" --without-$flag "$@"
	else
		add_use --with-$flag --without-$flag "$@"
	fi
}

add_enable() {
	local flag=$1; shift
	add_use --enable-$flag --disable-$flag "$@"
}

src_compile() {
		local myconf

	add_with \
		atlas \
		"`pkg-config --libs-only-L cblas`" \
		atlas

	add_with \
		atlas-flags \
		"-I`pkg-config --variable=includedir cblas`/atlas" \
		atlas

	add_with \
		x \
		'' \
		xclient X

	add_enable rthreads mpi

	if use petsc && ! use mpi ; then
		eerror "Use flag petsc requires use flag mpi"
		die
	fi

	use petsc && add_myconf --enable-plugins

	add_with \
		petsc \
		"${PETSC_DIR}" \
		petsc

	add_with \
		petsc-arch \
		"${PETSC_ARCH}" \
		petsc

	add_myconf \
		$(use_with emacs) \
		$(use_enable setuid) \
		$(use_enable daemon) \

	add_myconf REAL_TMP=/tmp

	if use threads; then
		local threads=$(grep -E -e '^processor[[:space:]]+:' /proc/cpuinfo | wc -l)
		add_myconf --enable-threads=${threads}
	else
		add_myconf --disable-threads
	fi

	if use memory; then
		local memory=$(grep -E -e '^MemTotal:' /proc/meminfo | sed -re 's/.*:[[:space:]]*//g' -e 's/[^0-9]*$//g')
		add_myconf MAX_MEM_SIZE=$((memory/1024))
	fi

	econf  "${myconf[@]}" || die "econf failed"
	emake || die "emake failed"
}

src_install() {
	emake DESTDIR="${D}" install || die "emake install failed"
	elisp-site-file-install "${FILESDIR}/${SITEFILE}" || die
	if use daemon ; then
		newinitd "${FILESDIR}"/dnoded.rc6 dnoded
		newconfd "${FILESDIR}"/dnoded.conf dnoded
		fowners dnode:dnode /usr/bin/dnode-daemon
		fperms ug+s /usr/bin/dnode-daemon
	fi
}

pkg_setup() {
	if use daemon ; then
		enewgroup dnode
		enewuser dnode -1 -1 -1 dnode
	fi
}

pkg_postinst() {
	elisp-site-regen
}

pkg_postrm() {
	elisp-site-regen
}
