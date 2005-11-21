AM_CFLAGS = $(GCC_CFLAGS) $(X_CFLAGS)
AM_LDFLAGS = -no-undefined $(PTHREAD_LIBS) $(X_LDFLAGS)
INCLUDE = @INCLTDL@

bin_PROGRAMS = ssh-password
ssh_password_SOURCES = ssh-password.c ssh-password.h ssh-password_main.c forms.h
ssh_password_LDFLAGS = $(LDFLAGS) -lforms
