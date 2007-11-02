
LIBNAME  =  L2UpsilonAlgo
HDRS     =  L2upsilon.hh 
SRCS     =  L2upsilon.cc

CXX    = g++
AR     = ar rul

DEBUG  =-g -O 

# some details may differ for the real L2-machine
ifndef STAR
  DEFINES = -DIS_REAL_L2
  INCL =-I  /home/startrg/trg/trg_soft_dev/include/	
else
  DEFINES=
  INCL =  -I ../L2algoUtil -I ../../../ -I ../L2algoUtil -I/afs/rhic.bnl.gov/star/packages/DEV/StRoot
endif

INSTALPATH=../../../../onlineL2

#
#INCL =  -I ../L2algoUtil  


OBJS = $(notdir $(patsubst %.cc,%.o,$(SRCS)))

#
CFLAGS   = -Wall $(DEBUG) $(DEFINES) $(INCL)
LDFLAGS  = -shared 

all:	$(LIBNAME) 

$(LIBNAME): $(OBJS)
	$(AR)  lib$(LIBNAME).a $^
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so
	@echo install path=$(INSTALPATH)

install:
	cp -p lib$(LIBNAME).a  $(INSTALPATH)/alg_lib
	cp -p   $(HDRS)   $(INSTALPATH)/alg_include

clean:
	-rm -f *.o $(LINK)  lib$(LIBNAME).*  .depend


depend .depend : $(SRCS) 
	$(CXX) -M $(CFLAGS) $^ > ./.depend
 

%.o: %.cc
	$(CXX) $(CFLAGS) -c $< -o $@

#
-include .depend

#*********************************************************************
#
# $Log: Makefile,v $
# Revision 1.1  2007/11/02 03:03:54  balewski
# modified L2VirtualAlgo
#
#
