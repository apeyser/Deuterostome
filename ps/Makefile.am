INFILES = $(nodist_pkgpsdata_DATA)
EXTRA_DIST = $(INFILES:=in) $(IDFILES)
CLEANFILES = $(INFILES)
SUFFIXES =

dist_pkgpsdata_DATA = cajal2.img form_1.ps form_2.ps graf_1.ps gray.ps \
	monday.ps struct.ps text.ps thursday_cajal2.ps

nodist_pkgpsdata_DATA = thursday.ps tuesday.ps

edit = -e 's,[@]pkgpsdatadir[@],$(pkgpsdatadir),g'

include $(top_srcdir)/m4/cf_in.make

