ARFLAGS    = vqs
FFLAGS     = -c -O -w -extend_source
FFLAGS4    = -c -O -w -extend_source -irix4
FFLAGS_DBG = -c -g -w -extend_source
FF         = f77
CC          = cc
CCFLAGS     = -c -O -ansi -fullwarn -prototypes
CCFLAGS4    = -c -O -ansi -fullwarn -prototypes -irix4
CCFLAGS_DBG = -c -g -ansi -fullwarn -prototypes
MSGLIB     = ../lib
STRLIB     = ../../str/lib

msgtest.o : msgtest.f
	$(FF) $(FFLAGS) msgtest.f

all : libmsg.a libmsg_dbg.a

msg4 : libmsg4.a

msg : libmsg.a

dbg : libmsg_dbg.a

doc : msglib.f msgsub.f msgc.c
	cd ../doc; docc   ../src/msgc.c;   mv buttons.html msgc.html           ; cd ../src
	cd ../doc; docfor ../src/msglib.f; mv buttons.html msg_application.html; cd ../src
	cd ../doc; docfor ../src/msgsub.f; mv buttons.html msg_innards.html    ; cd ../src

test : msgtest msgctest

msgtest : msgtest.o $(MSGLIB)/libmsg.a $(STRLIB)/libstr.a
	$(FF) msgtest.o -L$(MSGLIB) -L$(STRLIB) -lmsg -lstr -o msgtest
	msgtest

msgtest.o : msgtest.f ../inc/msgc.h
	$(FF) $(FFLAGS) msgtest.f -I../inc

msgctest : msgctest.o $(MSGLIB)/libmsg.a $(STRLIB)/libstr.a
	$(FF) msgctest.o -L$(MSGLIB) -L$(STRLIB) -lmsg -lstr -lfgl -lgl_s -lsun -o msgctest
	msgctest

msgctest.o : msgctest.c
	$(CC) $(CCFLAGS) msgctest.c -I../inc


libmsg4.a : msglib.o4 msgsub.o4 msgc.o4
	rm -f libmsg4.a
	ar $(ARFLAGS) libmsg4.a msglib.o4 msgsub.o4 msgc.o4
	rm -f msglib.o4
	rm -f msgsub.o4
	rm -f msgc.o4

libmsg.a : msglib.o msgsub.o msgc.o
	rm -f libmsg.a
	ar $(ARFLAGS) libmsg.a msglib.o msgsub.o msgc.o
	rm -f msglib.o
	rm -f msgsub.o
	rm -f msgc.o

libmsg_dbg.a : msglib_dbg.o msgsub_dbg.o msgc_dbg.o
	rm -f libmsg_dbg.a
	ar $(ARFLAGS) libmsg_dbg.a msglib_dbg.o msgsub_dbg.o msgc_dbg.o
	rm -f msglib_dbg.o
	rm -f msgsub_dbg.o
	rm -f msgc_dbg.o


msglib.o4 : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS4) msglib.f -o msglib.o4

msglib.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS) msglib.f

msglib_dbg.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS_DBG) msglib.f -o msglib_dbg.o


msgsub.o4 : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS4) msgsub.f -o msgsub.o4

msgsub.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS) msgsub.f

msgsub_dbg.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS_DBG) msgsub.f -o msgsub_dbg.o


msgc.o4 : msgc.c
	$(CC) $(CCFLAGS4) msgc.c -o msgc.o4

msgc.o : msgc.c
	$(CC) $(CCFLAGS) msgc.c

msgc_dbg.o : msgc.c
	$(CC) $(CCFLAGS_DBG) msgc.c -o msgc_dbg.o



msg_inc : msg.inc
	ln -s msg.inc msg_inc

msg_native_inc : msg_sgi.inc
	ln -s msg_sgi.inc msg_native_inc

msg_ret_inc : msg_ret.inc
	ln -s msg_ret.inc msg_ret_inc
