dist_noinst_DATA = benchmark.d arr.d.in
DISTEDITSRCS =  petsc-tester.d matrix-test.d
noinst_DATA = arr.d $(DISTEDITSRCS)

INFILES = $(DISTEDITSRCS)
EXTRA_DIST = $(DISTEDITSRCS:=in)
CLEANFILES = $(INFILES)
SUFFIXES =

include $(top_srcdir)/m4/cf_in.make

