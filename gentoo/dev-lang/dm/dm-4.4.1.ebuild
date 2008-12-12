# Copyright 1999-2007 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

EAPI="1"

inherit eutils elisp-common

# Short one-line description of this package.
DESCRIPTION="The World Famous D-Machine"

# Homepage, not used by Portage directly but handy for developer reference
HOMEPAGE="http://foo.bar.com/"

# Point to any required sources; these will be automatically downloaded by
# Portage.
#SRC_URI=""
SRC_URI="rsync://gentoo/gentoo-local/distfiles/${P}.tar.gz"
#ESVN_REPO_URI="svn://klutz0.nonnerlab/svn/dm-4.0/trunk"

# License of the package.  This must match the name of file(s) in
# /usr/portage/licenses/.  For complex license combination see the developer
# docs on gentoo.org for details.
LICENSE="GPL-2"

SLOT="0"

KEYWORDS="~x86 ~amd64 ~ppc"

IUSE="+daemon +emacs +atlas +setuid +threads +formats
	  +xclient X +memory +xauth +petsc +mpi doc
	  plugins +plugins-support"

# A space delimited list of portage features to restrict. man 5 ebuild
# for details.  Usually not needed.
RESTRICT="strip mirror primaryuri"

RDEPEND="
emacs? ( virtual/emacs )
formats? ( app-text/a2ps )
formats? ( virtual/ghostscript )
formats? ( dev-lang/perl )
formats? ( xclient? ( app-text/xdvik ) )
formats? ( xclient? ( x11-terms/xterm ) )
xclient? ( app-shells/bash )
formats? ( xclient? ( sys-apps/coreutils ) )
formats? ( xclient? ( net-print/cups ) )
formats? ( xclient? ( app-text/texlive-core ) )
formats? ( xclient? ( dev-texlive/texlive-latex ) )
X? ( x11-base/xorg-server )
formats? ( xclient? ( || (
		  kde-base/konqueror
		  gnome-base/libgnome
		  kde-base/kpdf
		  app-text/xpdf
		  app-text/gv
		  virtual/ghostscript
		 )
	)
)
atlas? ( sci-libs/blas-atlas )
xclient? ( x11-libs/libX11 )
xauth? ( x11-libs/libXext )
mpi? ( virtual/mpi )
mpi? ( !sys-cluster/mpich )
petsc? ( >=sci-mathematics/petsc-2.3.3_p11 )
sys-libs/glibc
plugins-support? ( dev-util/pkgconfig )
"

DEPEND="${RDEPEND}
plugins? ( sys-devel/flex )
sys-devel/libtool
doc? ( app-text/texlive-core )
doc? ( dev-texlive/texlive-latex )
doc? ( dev-texlive/texlive-latexextra )
sys-devel/gcc
"

SITEFILE="50${PN}-gentoo.el"
RCFILE="dnoded.rc6"
CONFFILE="dnoded.conf"

add_myconf() {
	myconf=("${myconf[@]}" "$@")
}

add_use() {
	local on_flag="$1"; shift
	local off_flag="$1"; shift
	local state=false
	local i
	for i; do
		use $i && state=:
	done

	if $state; then
		add_myconf "$on_flag"
	else
		add_myconf "$off_flag"
	fi
}

add_with() {
	local flag=$1; shift
	local val=$1; shift
	local useflags=("$@")
	[[ -z "${useflags[*]}" ]] && useflags=($flag)

	if [[ -n $val ]] ; then
		add_use --with-$flag="$val" --without-$flag "${useflags[@]}"
	else
		add_use --with-$flag --without-$flag "${useflags[@]}"
	fi
}

add_enable() {
	local flag=$1; shift
	local useflags=("$@")
	[[ -z "${useflags[*]}" ]] && useflags=($flag)

	add_use --enable-$flag --disable-$flag "${useflags[@]}"
}

test_dep() {
	local on=$1; shift
	local req

	if use $on; then
		for req; do
			if ! use $req; then
				eerror "USE flag $on requires use flag $req"
				die
			fi
		done
	fi
}

src_compile() {
	local myconf

	add_with atlas "`pkg-config --libs-only-L cblas`"
	add_with atlas-flags \
		"-I`pkg-config --variable=includedir cblas`/atlas" \
		atlas
	add_with x '' xclient X

	add_enable rthreads mpi

	test_dep petsc mpi plugins-support
	test_dep plugins plugins-support

	add_with petsc "${PETSC_DIR}"
	add_with petsc-arch "${PETSC_ARCH}" petsc

	add_myconf \
		$(use_with emacs) \
		$(use_enable setuid) \
		$(use_enable daemon) \
		$(use_enable plugins) \
		$(use_enable plugins-support) \
		$(use_enable doc)

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
		newinitd "${FILESDIR}/${RCFILE}" dnoded || die
		newconfd "${FILESDIR}/${CONFFILE}" dnoded || die
		fowners dnode:dnode /usr/bin/dnode-daemon || die
		fperms ug+s /usr/bin/dnode-daemon || die
	fi
}

pkg_setup() {
	if use plugins \
		&& built_with_use --missing false sys-devel/gcc nocxx ; then
		eerror "Please reemerge sys-devel/gcc without the nocxx USE flag enabled"
		die
	fi

	if use daemon ; then
		enewgroup dnode || die
		enewuser dnode -1 -1 -1 dnode || die
	fi
}

pkg_postinst() {
	elisp-site-regen
}

pkg_postrm() {
	elisp-site-regen
}
