#
#
#	Makefile for simple data logger
#
#
#CC=gcc
#CFLAGS= -Wall
CPPFLAGS=-I../inc
LIBDS=../lib/sun4/libdsl.a
LIBTCP=libtcp.a
LIBOBJ=tcpcodes.o tcpd.o tcplib.o tcpwrite.o
OBJ= $(LIBOBJ) testd.o tcptest.o
PROGS=testd tcptest

all:		$(PROGS) 

$(PROGS):	$(LIBTCP) $(LIBDS)

$(OBJ):		tcplib.h 

$(LIBTCP):	$(LIBOBJ)
		ar r $@ $?
		ranlib $@

clean:
		-rm $(PROGS) $(OBJ) $(LIBTCP)
