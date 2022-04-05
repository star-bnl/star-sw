# some details may differ for  the real  L2-machine
ifndef STAR
  DEFINES = -DIS_REAL_L2
  INCL =-I  /home/startrg/trg/trg_soft_dev/include/
else
  DEFINES=-I../../../ -I/afs/rhic.bnl.gov/star/packages/DEV/StRoot 
  INCL =
endif

INSTALPATH=../../../../onlineL2
ROOT_LIB  = `root-config --libs`
ROOT_INC  = -I `root-config --incdir`


LIBNAME  = L2algoUtil
HDRS     = L2Histo.h  L2EmcDb.h L2VirtualAlgo.h L2VirtualAlgo2008.h L2btowCalAlgo08.h  L2etowCalAlgo08.h L2EmcGeom.h L2exampleAlgo08.h L2exampleResult08.h L2eventStream2008.h

SRCS     = L2Histo.cxx  L2EmcDb.cxx L2EmcGeom.cxx  L2VirtualAlgo.cxx  L2VirtualAlgo2008.cxx L2btowCalAlgo08.cxx  L2etowCalAlgo08.cxx L2exampleAlgo08.cxx


CXX    = g++
AR     = ar rul

DEBUG  =-g  
# -O
#

OBJS   = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))


#
CFLAGS   = -Wall $(DEBUG) $(DEFINES) $(INCL)
LDFLAGS  = -shared 

all:	$(LIBNAME) 

$(LIBNAME): $(OBJS)
	$(AR)  lib$(LIBNAME).a $^
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so
	@echo install path=$(INSTALPATH)
	echo $(OBJS)

install:
	cp -p lib$(LIBNAME).a  $(INSTALPATH)/alg_lib
	cp -p $(HDRS)  $(INSTALPATH)/alg_include


binH2rootH: binH2rootH.cxx L2Histo.cxx
	$(CXX) $(CFLAGS) $(ROOT_INC) -o $@ $^ $(LIBS) $(ROOT_LIB)



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
# Revision 1.11  2008/01/30 00:47:17  balewski
# Added L2-Etow-calib
#
# Revision 1.10  2008/01/18 23:29:14  balewski
# now L2result is exported
#
# Revision 1.9  2008/01/16 23:32:36  balewski
# toward token dependent compute()
#
# Revision 1.8  2007/12/19 02:30:19  balewski
# new L2-btow-calib-2008
#
# Revision 1.7  2007/11/18 21:58:56  balewski
# L2algos triggerId list fixed
#
# Revision 1.6  2007/11/08 21:29:13  balewski
# now L2emu runs on M-C
#
# Revision 1.5  2007/11/08 04:02:27  balewski
# run on l2ana as well
#
# Revision 1.4  2007/11/06 22:07:24  balewski
# added timeStamp controlled L2 setup from Jason
#
# Revision 1.3  2007/11/02 03:03:41  balewski
# modified L2VirtualAlgo
#
# Revision 1.2  2007/10/25 02:06:59  balewski
# added L2upsilon & binary event dump
#
# Revision 1.1  2007/10/11 00:33:15  balewski
# L2algo added
#
# Revision 1.3  2006/03/11 17:08:32  balewski
# now CVS comments should work
#
#
