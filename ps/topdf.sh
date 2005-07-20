#!/bin/bash

#
# usage: topdf.sh monday tuesday ...
#
# runs on MacOSX
#

for i ; do
    gs -dBATCH -dNOPAUSE -sDEVICE=psgray -sOutputFile=gray.ps $i.ps \
	&& ps2pdf gray.ps $i.pdf \
	&& launch -i com.adobe.Reader -p $i.pdf
done
