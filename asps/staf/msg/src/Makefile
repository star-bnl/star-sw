ARFLAGS    = vqs
FFLAGS     = -c -O -w -extend_source
FFLAGS4    = -c -O -w -extend_source -irix4
FFLAGS_DBG = -c -g -w -extend_source
FF         = f77

all : libmsg.a libmsg_dbg.a

msg4 : libmsg4.a

msg : libmsg.a

dbg : libmsg_dbg.a

doc : msglib.f msgsub.f
	cd ../doc; docfor ../src/msglib.f; mv buttons.html msg_application.html; cd ../src
	cd ../doc; docfor ../src/msgsub.f; mv buttons.html msg_innards.html    ; cd ../src

test : msgtest

msgtest : msgtest.o ../lib/libmsg.a ../../str/lib/libstr.a
	$(FF) msgtest.o -L../lib -L../../str/lib -lmsg -lstr -o msgtest
	msgtest

msgtest.o : msgtest.f
	$(FF) $(FFLAGS) msgtest.f


libmsg4.a : msglib.o4 msgsub.o4 Makefile
	rm -f libmsg4.a
	ar $(ARFLAGS) libmsg4.a msglib.o4 msgsub.o4
	rm -f msglib.o4
	rm -f msgsub.o4

libmsg.a : msglib.o msgsub.o Makefile
	rm -f libmsg.a
	ar $(ARFLAGS) libmsg.a msglib.o msgsub.o
	rm -f msglib.o
	rm -f msgsub.o

msglib.o4 : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS4) msglib.f -o msglib.o4

msglib.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS) msglib.f

msgsub.o4 : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS4) msgsub.f -o msgsub.o4

msgsub.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS) msgsub.f


libmsg_dbg.a : msglib_dbg.o msgsub_dbg.o Makefile
	rm -f libmsg_dbg.a
	ar $(ARFLAGS) libmsg_dbg.a msglib_dbg.o msgsub_dbg.o
	rm -f msglib_dbg.o
	rm -f msgsub_dbg.o

msglib_dbg.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS_DBG) msglib.f -o msglib_dbg.o

msgsub_dbg.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	$(FF) $(FFLAGS_DBG) msgsub.f -o msgsub_dbg.o

msg_inc : msg.inc
	ln -s msg.inc msg_inc

msg_native_inc : msg_sgi.inc
	ln -s msg_sgi.inc msg_native_inc

msg_ret_inc : msg_ret.inc
	ln -s msg_ret.inc msg_ret_inc
