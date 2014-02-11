#  We need the makefile for several things:
#
#  1.  Build/install WritePDFToDB
#  2.  Install l4HistoMother
#  3.  Install HistoMother
#

HOSTNAME := $(shell hostname -s)
ifeq ($(HOSTNAME),l4evp) 
	RTSCACHE := /RTScache/bin/LINUX/x86_64
	HISTOMOTHER := l4HistoMother
else
	RTSCACHE := /RTScache/bin/LINUX/i686
	HISTOMOTHER := jevpHistoMother
endif

CVSDIR=../../..

CC = g++

CPPFLAGS += -I/usr/include/mysql -I$(CVSDIR)/StRoot/RTS/include -I$(CVSDIR)/online/RTS/src
LDLIBS += -L /usr/lib64/mysql -lmysqlclient


vpath %.c $(CVSDIR)/StRoot/RTS/src/LOG

WritePDFToDB:	WritePDFToDB.o rtsLogUnix.o

install:
	/RTS/bin/install.sh WritePDFToDB $(RTSCACHE)
	/RTS/bin/install.sh $(HISTOMOTHER) ~

clean:
	rm *.o
	rm WritePDFToDB

