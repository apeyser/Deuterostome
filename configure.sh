#!/bin/bash 

function usage() {
    cat <<EOF 1>&2
Usage: $0 [--prefix <prefix>]? [--shprefix <shprefix>]? \
[--docprefix <docprefix>]? [--elprefix <elprefix>]? \
[--libprefix <libprefix>]? [--type <Makefile suffix>]? \
[--psprefix <psprefix>]?
EOF
    exit 1
}

if [[ $(uname -s) == 'Linux' ]] ; then
    #TYPE="$(uname -s)-$(uname -i | sed 's/,.*//')$(uname -m)"
    TYPE="$(uname -s)-$(uname -m)$(cat /proc/cpuinfo | grep -E '^cpu[[:space:]]+:' | sed -re 's/^cpu[[:space:]]+:[[:space:]]*//' -e 's/,.*//')"
elif [[ $(uname -s) == 'Darwin' ]] ; then
	TYPE="$(uname -s)-$(machine)"
else
	TYPE="$(uname -s)"
fi

while (( $# > 0 )) ; do
    case "$1" in
	--prefix) 
	    shift; PREFIX="$1" ; shift;;
	--shprefix)
	    shift; SHPREFIX="$1"; shift;;
	--docprefix)
	    shift; DOCPREFIX="$1"; shift;;
	--elprefix)
	    shift; ELPREFIX="$1"; shift;;
	--libprefix)
	    shift; LIBPREFIX="$1"; shift;;
	--psprefix)
		shift; PSPREFIX="$1"; shift;;
	--type)
	    shift; TYPE="$1"; shift;;
	*)
	    echo "Unknown flag: $1" 1>&2
	    usage;;
    esac
done

if [[ ! -e "Makefile.type-$TYPE" ]] ; then
	echo "TYPE unknown: $TYPE" 1>&2
	echo "Makefile.type-$TYPE must be created." 1>&2
	exit 1
fi

echo "TYPE is $TYPE"

DEFAULTPRE=/dm

function checkpre() {
    eval "if [[ -z \"\$$1\" ]] ; then $1=\"$2$3\"; fi"
    eval "echo \"$1 is \$$1\""
}

checkpre PREFIX "$DEFAULTPRE" ""
checkpre SHPREFIX "$PREFIX" /sh
checkpre DOCPREFIX "$PREFIX" /doc
checkpre ELPREFIX "$PREFIX" /emacs
checkpre LIBPREFIX "$PREFIX" /lib
checkpre PSPREFIX "$PREFIX" /ps

cat <<EOF >./Makefile
#-*-makefile-*-

PREFIX = $PREFIX
SHPREFIX = $SHPREFIX
DOCPREFIX = $DOCPREFIX
ELPREFIX = $ELPREFIX
LIBPREFIX = $LIBPREFIX
PSPREFIX = $PSPREFIX

include Makefile.type-$TYPE

EOF

for i in $(find . -mindepth 1 -type d) ; do
    echo "$(revpath '$i')Makefile" "$i/Makefile"
    ln -sf "$(revpath '$i')Makefile" "$i/Makefile"
	for j in Makefile.type-* ; do
		echo "$(revpath '$i')$j" "$i/$j"
		ln -sf "$(revpath '$i')$j" "$i/$j"
	done
done
