all : 
	@echo '****************************************'
	@echo '** Usage: make [ prepare | postpare ] **'
	@echo '****************************************'

NONLIBFILES := cpptest.cpp dsgram.y dsmark.c sample.c tcplib.c \
	tcptest.c testds.c testjoin.c testlib.c testsort.c testtas.c \
	unixtape.c xdrtape.c

prepare :
	-@cd src; \rm -f $(NONLIBFILES)

postpare :
	-@cd src; cvs update $(NONLIBFILES)

