#
#
#	Makefile for simple data logger
#
#
CC=gcc
CFLAGS= -Wall
CPPFLAGS=-I../inc
CC68K=cc68k
LD68K=ld68k
CC68FLAGS=-c -DCPU=MC68040 -DVXWORKS -I$(VX_VW_BASE)/h $(CPPFLAGS) -Wall
LIBDS=../lib/sun4/libdsl.a
LIBTCP=libtcp.a
LIBOBJ=tcpcodes.o tcpd.o tcplib.o tcpwrite.o
OBJ= $(LIBOBJ) testd.o tcptest.o
OBJ68K=tcpwrite.68o tcplib.68o
PROGS=testd tcptest
VWLIB=dstcp.68o

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
		-rm $(PROGS) $(OBJ) $(OBJ68K) $(LIBTCP) $(VWLIB)
