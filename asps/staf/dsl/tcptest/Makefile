#
#
#	Makefile for simple data logger
#
#
CC=gcc
CFLAGS= -Wall
CPPFLAGS=-I../inc
############################CPPFLAGS+=-Dcraig
CC68K=cc68k
LD68K=ld68k
CC68FLAGS=-c -DCPU=MC68040 -DVXWORKS -I$(VX_VW_BASE)/h $(CPPFLAGS) -Wall
LIBDS=../lib/$(STAR_ARCH)/libdsl.a
LIBTCP=libtcp.a
LIBOBJ=tcpcodes.o tcpd.o tcplib.o tcpwrite.o tcpread.o 
OBJ= $(LIBOBJ) testd.o tcptest.o tstread.o tstwrited.o
OBJ68K=tcpwrite.68o tcplib.68o
PROGS=testd tcptest tstread tstwrited
VWLIB=dstcp.68o

#-#VPATH += $(STAR_ARCH)

RNCSV1LIBS := -lnsl -lsocket -L/usr/ucblib -lucb -R/usr/ucblib
SUNOS := SunOS
ARCH := $(shell uname)
ifeq ($(SUNOS),$(ARCH))
LOADLIBES += $(RNCSV1LIBS)
endif

progs:		$(PROGS)
all:		$(PROGS) $(VWLIB)

$(PROGS):	$(LIBTCP) $(LIBDS)

$(OBJ):		tcplib.h 

%.68o : %.c
		$(CC68K) $(CC68FLAGS) -c $< -o $@

tcplib.68o:	tcplib.c
		$(CC68K)  $(CC68FLAGS) $(VWINC)  -c tcplib.c -o tcplib.68o

$(VWLIB):	$(OBJ68K)
		$(LD68K) -o $(VWLIB) -r $(OBJ68K)

$(LIBTCP):	$(LIBOBJ)
		ar r $@ $?
		ranlib $@

clean:
		-rm -f $(PROGS) $(OBJ) $(OBJ68K) $(LIBTCP) $(VWLIB)

install:
	mv *.o *.a $(PROGS) $(STAR_ARCH)/
