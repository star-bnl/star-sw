ARFLAGS    = vqs
FFLAGS     = -c -O -w
FFLAGS_DBG = -c -g -w

all : libmsg.a libmsg_dbg.a

msg : libmsg.a

dbg : libmsg_dbg.a

libmsg.a : msglib.o msgsub.o Makefile
	rm -f libmsg.a
	ar $(ARFLAGS) libmsg.a msglib.o msgsub.o
	rm -f msglib.o
	rm -f msgsub.o

msglib.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 $(FFLAGS) msglib.f

msgsub.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 $(FFLAGS) msgsub.f


libmsg_dbg.a : msglib_dbg.o msgsub_dbg.o Makefile
	rm -f libmsg_dbg.a
	ar $(ARFLAGS) libmsg_dbg.a msglib_dbg.o msgsub_dbg.o
	rm -f msglib_dbg.o
	rm -f msgsub_dbg.o

msglib_dbg.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 $(FFLAGS_DBG) msglib.f -o msglib_dbg.o

msgsub_dbg.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 $(FFLAGS_DBG) msgsub.f -o msgsub_dbg.o

msg_inc : msg.inc
	ln -s msg.inc msg_inc

msg_native_inc : msg_sgi.inc
	ln -s msg_sgi.inc msg_native_inc

msg_ret_inc : msg_ret.inc
	ln -s msg_ret.inc msg_ret_inc
