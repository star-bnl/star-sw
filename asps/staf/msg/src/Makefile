msg : libmsg.a

dbg : libmsg_dbg.a

all : libmsg.a libmsg_dbg.a

libmsg.a : msglib.o msgsub.o Makefile
	rm -f libmsg.a
	ar vq libmsg.a msglib.o msgsub.o
	ar ts libmsg.a

msglib.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 -c -O -w msglib.f

msgsub.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 -c -O -w msgsub.f


libmsg_dbg.a : msglib_dbg.o msgsub_dbg.o Makefile
	rm -f libmsg_dbg.a
	ar vq libmsg_dbg.a msglib_dbg.o msgsub_dbg.o
	ar ts libmsg_dbg.a

msglib_dbg.o : msglib.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 -g -c -w msglib.f -o msglib_dbg.o

msgsub_dbg.o : msgsub.f msg_ret_inc msg_inc msg_native_inc Makefile
	f77 -g -c -w msgsub.f -o msgsub_dbg.o

msg_inc : msg.inc
	ln -s msg.inc msg_inc

msg_native_inc : msg_sgi.inc
	ln -s msg_sgi.inc msg_native_inc

msg_ret_inc : msg_ret.inc
	ln -s msg_ret.inc msg_ret_inc
