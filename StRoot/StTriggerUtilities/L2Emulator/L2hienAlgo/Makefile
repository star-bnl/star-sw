

LIBNAME  =  L2hienAlgo
HDRS     =  L2hienAlgo08.h 
SRCS     =  L2hienAlgo08.cxx 


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


OBJS = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))

#
CFLAGS   = -Wall $(DEBUG) $(DEFINES) $(INCL)
LDFLAGS  = -shared 

all:	$(LIBNAME) 

$(LIBNAME): $(HOSTN) $(OBJS)
	$(AR)  lib$(LIBNAME).a $^
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so
	@echo install path=$(INSTALPATH)

install:
	cp -p lib$(LIBNAME).a  $(INSTALPATH)/alg_lib
	cp -p  L2hienAlgo08.h  $(INSTALPATH)/alg_include


clean:
	-rm -f *.o $(LINK)  lib*$(LIBNAME).*  .depend


depend .depend : $(SRCS) 
	$(CXX) -M $(CFLAGS) $^ > ./.depend


%.o: %.cxx
	$(CXX) $(CFLAGS) -c $< -o $@

#
-include .depend

#*********************************************************************
#
# $Log: Makefile,v $
# Revision 1.1  2008/01/29 00:17:21  balewski
# new algo filtering high-energy towers
#
# Revision 1.5  2007/11/19 22:18:28  balewski
# most L2algos provide triggerID's
#
# Revision 1.4  2007/11/08 04:02:31  balewski
# run on l2ana as well
#
# Revision 1.3  2007/11/02 03:03:48  balewski
# modified L2VirtualAlgo
#
# Revision 1.2  2007/10/25 02:07:03  balewski
# added L2upsilon & binary event dump
#
# Revision 1.1  2007/10/11 00:33:20  balewski
# L2algo added
#
# Revision 1.4  2006/03/28 19:46:49  balewski
# ver16b, in l2new
#
# Revision 1.3  2006/03/11 17:08:34  balewski
# now CVS comments should work
#
#
