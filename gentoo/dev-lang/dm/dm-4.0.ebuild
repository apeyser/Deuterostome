# Copyright 1999-2007 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

#EAPI="prefix"

inherit eutils subversion elisp-common

# Short one-line description of this package.
DESCRIPTION="The World Famous D-Machine"

# Homepage, not used by Portage directly but handy for developer reference
HOMEPAGE="http://foo.bar.com/"

# Point to any required sources; these will be automatically downloaded by
# Portage.
SRC_URI="rsync://deli1/gentoo-local/distfiles/${P}.tar.gz"
#ESVN_REPO_URI="svn://klutz0.nonnerlab/svn/dm-4.0/trunk"

# License of the package.  This must match the name of file(s) in
# /usr/portage/licenses/.  For complex license combination see the developer
# docs on gentoo.org for details.
LICENSE="GPL-2"

SLOT="0"

KEYWORDS="~x86 ~amd64 ~ppc"

IUSE="daemon emacs atlas setuid threads formats xclient xserver"

# A space delimited list of portage features to restrict. man 5 ebuild
# for details.  Usually not needed.
RESTRICT="strip"

DEPEND="
emacs? (virtual/emacs)
formats? (app-text/a2ps)
formats? (virtual/ghostscript)
xserver? (app-text/xdvik)
xserver? (x11-terms/xterm)
atlas? (sci-libs/blas-atlas)
xclient? (x11-libs/libX11)
xserver? (x11-base/xorg-server)
sys-libs/glibc
"

RDEPEND="${DEPEND}"

#src_unpack() {
#	subversion_src_unpack
#}

add_myconf() {
    myconf=("$myconf[@]" "$@")
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

src_compile() {
        local myconf
	add_with atlas "`pkg-config --libs-only-L cblas`" atlas
	add_with atlas-flags \
	    "-I`pkg-config --variable=includedir cblas`/atlas" \
	    atlas
	add_with x '' xclient xserver xterm
	add_myconf $(use_with emacs) $(use_enable setuid threads daemon)
	
	econf  "${myconf[@]}" || die "econf failed"
	emake || die "emake failed"
}

src_install() {
	emake DESTDIR="${D}" install || die "emake install failed"
	elisp-site-file-install "${FILESDIR}/${SITEFILE}" || die
	if use daemon ; then
	    newinitd "${FILESDIR}"/dnoded.rc6 dnoded
	    newinitd "${FILESDIR}"/dnoded.conf dnoded
	    enewgroup node
	    enewuser node -1 -1 /home/node node -g node
	    fowners node:node /usr/bin/dnode-daemon
	    fperms ug+s /usr/bin/dnode-daemon
	fi
}

pkg_postinst() {
	elisp-site-regen
}

pkg_postrm() {
	elisp-site-regen
}
