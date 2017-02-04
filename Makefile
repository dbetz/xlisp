##################
# XLISP Makefile #
##################

XLISPDIR=./xlisp
SRCDIR=./src
HDRDIR=./include
OBJDIR=./obj
LIBDIR=./lib
BINDIR=./bin

XLISPOBJDIR=$(OBJDIR)/xlisp
LIBOBJDIR=$(OBJDIR)/lib

DIRS = $(OBJDIR) $(LIBDIR) $(BINDIR)
OBJSUBDIRS = $(XLISPOBJDIR) $(LIBOBJDIR)

CC=cc
AR=ar
RANLIB=ranlib
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
xlisp:		$(BINDIR) $(BINDIR)/xlisp

XLISPOBJS=\
$(XLISPOBJDIR)/xlisp.o

$(XLISPOBJDIR)/%.o:	%.c $(INC)
	@$(CC) $(CFLAGS) -c $< -o $@
	@$(ECHO) $@

$(BINDIR)/xlisp:	$(XLISPOBJDIR) $(XLISPOBJS) library
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
	@$(RANLIB) $@
	@$(ECHO) $@

###############
# DIRECTORIES #
###############

$(OBJSUBDIRS):	$(OBJDIR)

$(DIRS) $(OBJSUBDIRS):
	$(MKDIR) $@

