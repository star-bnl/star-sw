#!gnumake

clean :
	rm *.o
	rm *.d

all : run test ftest

# Note:  "msg" is a Tcl/Tk gui.
run : ../bin/$(STAR_ARCH)/msg ../lib/$(STAR_ARCH)/libmsg.a ../bin/$(STAR_ARCH)/msgControl ../bin/$(STAR_ARCH)/rmid

../bin/$(STAR_ARCH)/msgControl : msgControl

../bin/$(STAR_ARCH)/rmid : rmid

../bin/$(STAR_ARCH)/msg : msg
	chmod u+x msg
	cp msg ../bin/$(STAR_ARCH)/msg

ftest : msgtest msgtest.ref
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
	rm msg.jou
	@echo ""
	@echo "   +++++++++++++++++++++++++++++"
	@echo "   + Automatic test succeeded. +"
	@echo "   +++++++++++++++++++++++++++++"
	@echo ""

# test : msgTest msgtest msgctest msgstest msgstest_so msgControl msgTest.ref msgtest.ref msgctest.ref
test : msgTest msgctest msgstest msgControl msgTest.ref msgctest.ref
	@echo ""
	@echo "   +++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo "   + Automatic test of some msg calls, using GNU diff. +"
	@echo "   +++++++++++++++++++++++++++++++++++++++++++++++++++++"
	@echo ""
	cd ../wrk/;msgTest 2> /dev/null
	mv ../wrk/msg.jou .
	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgTest.ref
	rm msg.jou

	cd ../wrk/;msgctest 2> /dev/null
	mv ../wrk/msg.jou .
	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgctest.ref
	rm msg.jou

#	cd ../wrk/;msgtest 2> /dev/null
#	mv ../wrk/msg.jou .
#	diff -I"Elapsed:" -I"--- Message Accounting Summary" -I"--- CPU Usage Summary ---" -I"msgTest-O4  Once    #4" msg.jou msgtest.ref
#	rm msg.jou

	@echo ""
	@echo "   +++++++++++++++++++++++++++++"
	@echo "   + Automatic test succeeded. +"
	@echo "   +++++++++++++++++++++++++++++"
	@echo ""

rmid : rmid.o
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

msgControl : msgControl.o ../lib/$(STAR_ARCH)/libmsg.a
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

msgTest : msgTest.o ../lib/$(STAR_ARCH)/libmsg.a
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -lm -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

msgtest : msgtest.o ../lib/$(STAR_ARCH)/libmsg.a
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -R/opt/SUNWspro/SC4.2/lib -L/opt/SUNWspro/SC4.2/lib -lM77 -lF77 -lsunmath -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

msgctest : msgctest.o ../lib/$(STAR_ARCH)/libmsg.a
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -lm -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

msgstest : msgstest.o ../lib/$(STAR_ARCH)/libmsg.a
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -lm -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

msgstest_so : msgstest.o ../solib/$(STAR_ARCH)/libmsg.so
#	$(CC) $^ -lc -lftn -o ../bin/$(STAR_ARCH)/$@
	$(CC) $^ -lc -lm -o ../bin/$(STAR_ARCH)/$@
	rm -f ../wrk/$@
	ln -sf ../bin/$(STAR_ARCH)/$@ ../wrk/$@

%.o: %.c
	gcc -M $< -I../inc -I/usr/include > $@.d
	gcc -c -g -Wall $< -I../inc -I/usr/include
