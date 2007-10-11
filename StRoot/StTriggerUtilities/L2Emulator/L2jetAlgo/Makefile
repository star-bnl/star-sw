

LIBNAME  =  L2jetAlgo
HDRS     =  L2jetAlgo.h L2jetResults2006.h Map_DeltaPhiJets.h
SRCS     =  L2jetAlgo.cxx


CXX    = g++
AR     = ar rul

DEBUG  =-g -O 

# some details may differ for the real L2-machine
ifndef STAR
  DEFINES = -DIS_REAL_L2
  INCL =-I  /home/startrg/trg/trg_soft_dev/include/	
else
  DEFINES=
  INCL =-I ../Tonko/
endif


INSTALPATH=../main

INCL +=   -I ../L2janUtil 


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
	cp -p  L2jetAlgo.h   $(INSTALPATH)/alg_include


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
