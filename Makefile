##################
# XLISP Makefile #
##################

XLISPDIR=xlisp
SRCDIR=src
HDRDIR=include
OBJDIR=obj
LIBDIR=lib
BINDIR=bin

XLISPOBJDIR=$(OBJDIR)/xlisp
LIBOBJDIR=$(OBJDIR)/lib

DIRS = $(OBJDIR) $(LIBDIR) $(BINDIR)
OBJSUBDIRS = $(XLISPOBJDIR) $(LIBOBJDIR)

ifeq ($(CROSS),)
  PREFIX=
  EXT=
else
  ifeq ($(CROSS),win32)
	PREFIX=i686-w64-mingw32-
    OS=msys
    EXT=.exe
  else
    ifeq ($(CROSS),rpi)
      PREFIX=arm-linux-gnueabihf-
      OS=raspberrypi
      EXT=
    else
      $(error Unknown cross compilation selected)
    endif
  endif
endif

ifeq ($(TARGET),p2llvm)
  CC=/opt/p2llvm/bin/clang -fno-exceptions --target=p2 -Dprintf=__simple_printf
  AR=/opt/p2llvm/bin/llvm-ar
else
  CC=$(PREFIX)gcc
  AR=ar
endif

ECHO=echo
MKDIR=mkdir

CFLAGS=-Wall -DUNIX -I$(HDRDIR)

INC=$(HDRDIR)/xlisp.h

VPATH = $(XLISPDIR):$(SRCDIR)

##################
# DEFAULT TARGET #
##################

.PHONY:	all
all:	xlisp

################
# CLEAN TARGET #
################

.PHONY:	clean
clean:
	rm -f -r $(OBJDIR)
	rm -f -r $(LIBDIR)
	rm -f -r $(BINDIR)

#########
# XLISP #
#########

.PHONY:	xlisp
xlisp:		$(BINDIR) $(BINDIR)/xlisp$(EXT)

XLISPOBJS=\
$(XLISPOBJDIR)/xlisp.o

$(XLISPOBJDIR)/%.o:	%.c $(INC)
	@$(CC) $(CFLAGS) -c $< -o $@
	@$(ECHO) $@

$(BINDIR)/xlisp$(EXT):	$(XLISPOBJDIR) $(XLISPOBJS) library
	@$(CC) $(CFLAGS) $(XLISPOBJS) -L$(LIBDIR) -lxlisp -lm -o $@
	@$(ECHO) $@

###########
# LIBRARY #
###########

.PHONY:	library
library:	$(LIBDIR) $(LIBDIR)/libxlisp.a

LIBOBJS=\
$(LIBOBJDIR)/unstuff.o \
$(LIBOBJDIR)/xlansi.o \
$(LIBOBJDIR)/xlapi.o \
$(LIBOBJDIR)/xlcobj.o \
$(LIBOBJDIR)/xlcom.o \
$(LIBOBJDIR)/xldbg.o \
$(LIBOBJDIR)/xldmem.o \
$(LIBOBJDIR)/xlfasl.o \
$(LIBOBJDIR)/xlftab.o \
$(LIBOBJDIR)/xlfun1.o \
$(LIBOBJDIR)/xlfun2.o \
$(LIBOBJDIR)/xlfun3.o \
$(LIBOBJDIR)/xlimage.o \
$(LIBOBJDIR)/xlinit.o \
$(LIBOBJDIR)/xlint.o \
$(LIBOBJDIR)/xlio.o \
$(LIBOBJDIR)/xlmain.o \
$(LIBOBJDIR)/xlitersq.o \
$(LIBOBJDIR)/xlmath.o \
$(LIBOBJDIR)/xlobj.o \
$(LIBOBJDIR)/xlosint.o \
$(LIBOBJDIR)/xlprint.o \
$(LIBOBJDIR)/xlread.o \
$(LIBOBJDIR)/xlsym.o

$(LIBOBJDIR)/%.o:	%.c $(INC) $(SRCDIR)/xlbcode.h
	@$(CC) $(CFLAGS) -c $< -o $@
	@$(ECHO) $@

$(LIBDIR)/libxlisp.a:	$(LIBOBJDIR) $(LIBOBJS)
	@$(AR) crs $@ $(LIBOBJS)
	@$(ECHO) $@

###############
# DIRECTORIES #
###############

$(OBJSUBDIRS):	$(OBJDIR)

$(DIRS) $(OBJSUBDIRS):
	$(MKDIR) $@

