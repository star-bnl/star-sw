#!gnumake
ifeq "$(STAR_ARCH)" "sun4os5"
  FF = f77
else
  ifeq "$(STAR_ARCH)" "sun4os5pc"
    FF = f77
  else
    FF = $(CC) -lftn
  endif
endif

clean :
	rm -f *.o
	rm -f *.d

all : run test ftest

# Note:  "msg" is a Tcl/Tk gui.
run : ../bin/$(STAR_ARCH)/msg ../lib/$(STAR_ARCH)/libmsg.a ../bin/$(STAR_ARCH)/msgControl ../bin/$(STAR_ARCH)/rmid

../bin/$(STAR_ARCH)/msg : msg
	chmod u+x msg
	cp msg ../bin/$(STAR_ARCH)/msg

ftest : ../bin/$(STAR_ARCH)/msgtest msgtest.ref
	cd ../wrk/;msgtest 2> /dev/null
	mv ../wrk/msg.jou .
	@echo ""
	@echo "   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo "   + If this fails here, then this architecture probably doesn't have +"
	@echo "   + the maplun_ FORTRAN LUN to file-desriptor mapping routine.       +"
	@echo "   +    (Following such a failure, do an rm msg.jou* by hand.)        +"
	@echo "   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo ""
	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgtest.ref
	rm -f msg.jou
	@echo ""
	@echo "   +++++++++++++++++++++++++++++"
	@echo "   + Automatic test succeeded. +"
	@echo "   +++++++++++++++++++++++++++++"
	@echo ""

# test : msgTest msgtest msgctest msgstest msgstest_so msgTest.ref msgtest.ref msgctest.ref
test : ../bin/$(STAR_ARCH)/msgTest ../bin/$(STAR_ARCH)/msgctest ../bin/$(STAR_ARCH)/msgstest msgTest.ref msgctest.ref
	@echo ""
	@echo "   +++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo "   + Automatic test of some msg calls, using GNU diff. +"
	@echo "   +++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo ""
	cd ../wrk/;msgTest 2> /dev/null
	mv ../wrk/msg.jou .
	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgTest.ref
	rm -f msg.jou

	cd ../wrk/;msgctest 2> /dev/null
	mv ../wrk/msg.jou .
	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgctest.ref
	rm -f msg.jou

#	cd ../wrk/;msgtest 2> /dev/null
#	mv ../wrk/msg.jou .
#	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgtest.ref
#	rm -f msg.jou

	@echo ""
	@echo "   +++++++++++++++++++++++++++++"
	@echo "   + Automatic test succeeded. +"
	@echo "   +++++++++++++++++++++++++++++"
	@echo ""

../bin/$(STAR_ARCH)/rmid : rmid.o
	$(CC) $^ -lc -o $@
	rm -f ../wrk/rmid
	ln -sf $@ ../wrk/rmid

../bin/$(STAR_ARCH)/msgControl : msgControl.o ../lib/$(STAR_ARCH)/libmsg.a
	$(CC) $^ -lc -o $@
	rm -f ../wrk/msgControl
	ln -sf $@ ../wrk/msgControl

../bin/$(STAR_ARCH)/msgTest : msgTest.o ../lib/$(STAR_ARCH)/libmsg.a
	$(CC) $^ -lc -lm -o $@
	rm -f ../wrk/msgTest
	ln -sf $@ ../wrk/msgTest

../bin/$(STAR_ARCH)/msgtest : msgtest.o ../lib/$(STAR_ARCH)/libmsg.a
	$(FF) $^ -lc -o $@;
	rm -f ../wrk/msgtest
	ln -sf $@ ../wrk/msgtest

../bin/$(STAR_ARCH)/msgctest : msgctest.o ../lib/$(STAR_ARCH)/libmsg.a
	$(CC) $^ -lc -lm -o $@
	rm -f ../wrk/msgctest
	ln -sf $@ ../wrk/msgctest

../bin/$(STAR_ARCH)/msgstest : msgstest.o ../lib/$(STAR_ARCH)/libmsg.a
	$(CC) $^ -lc -lm -o $@
	rm -f ../wrk/msgstest
	ln -sf $@ ../wrk/msgstest

../bin/$(STAR_ARCH)/msgstest_so : msgstest.o ../solib/$(STAR_ARCH)/libmsg.so
	$(CC) $^ -lc -lm -o $@
	rm -f ../wrk/msgstest_so
	ln -sf $@ ../wrk/msgstest_so

%.o: %.c
	gcc -M $< -I../inc -I/usr/include > $@.d
	gcc -c -g -Wall $< -I../inc -I/usr/include
