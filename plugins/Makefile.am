AM_CFLAGS =  $(GCC_CFLAGS) @LIBDM_DLL_IMPORT@ $(X_CFLAGS)
AM_LDFLAGS = -module -no-undefined $(X_LDFLAGS)
#@LIBPLUGIN_DLL_IMPORT@
INCLUDE = @INCLTDL@ 
LIBS += $(LTLIBOBJS) $(top_builddir)/src/libdmglobals.la

if ENABLE_PROCESS
PROCESS_LIB=process.la
process_la_SOURCES = process.c process.h
endif ENABLE_PROCESS

if ENABLE_SSH
SSH_LIB=ssh.la
SSH_D = ssh.d
ssh_la_SOURCE = ssh.c ssh.h
ssh_la_LDFLAGS = $(AM_LDFLAGS) -lssh
endif ENABLE_SSH

dist_pkglib_DATA = $(SSH_D)
pkglib_LTLIBRARIES = test.la test2.la $(PROCESS_LIB) throw.la $(SSH_LIB)

test_la_SOURCES = test.c test.h
test2_la_SOURCES = test2.c test2.h
throw_la_SOURCES = throw.c throw.h

