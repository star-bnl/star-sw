
LIBNAME  =  L2upsilonAlgo
HDRS     =  L2upsilon2006.hh 
SRCS     =  L2upsilon2006.cc

CXX    = g++
AR     = ar rul

DEBUG  =-g -O 

INCL =-I ../L2algoUtil

# some details may differ for the real L2-machine
ifndef STAR
  DEFINES = -DIS_REAL_L2
  INCL +=-I  /home/startrg/trg/trg_soft_dev/include/	
else
  DEFINES=
  INCL += -I../../../ -I/afs/rhic.bnl.gov/star/packages/DEV/StRoot 
endif

INSTALPATH=../../../../onlineL2



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
	cp -p L2upsilon2006.hh     $(INSTALPATH)/alg_include

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
# Revision 1.3  2008/01/30 15:09:39  balewski
# Added upsilon2006, needs cleaunp in few days
#
# Revision 1.2  2007/11/08 04:02:36  balewski
# run on l2ana as well
#
# Revision 1.1  2007/11/02 03:03:54  balewski
# modified L2VirtualAlgo
#
#
