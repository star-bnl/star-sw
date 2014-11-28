# some details may differ for  the real  L2-machine
ifndef STAR
  INSTALPATH=../../../onlineL2
else
  INSTALPATH=../../../onlineL2
endif


LIBNAME  = L2binEve
HDRS     =JanEventReader.h JanEvent.h
SRCS     =JanEventReader.cxx JanEvent.cxx


CXX    = g++
AR     = ar rul

DEBUG  =-g -O 
DEFINES=
#

OBJS   = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))


#
CFLAGS   = -Wall $(DEBUG) $(DEFINES) 
LDFLAGS  = -shared 

all:	$(LIBNAME) 

$(LIBNAME): $(OBJS)
	$(AR)  lib$(LIBNAME).a $^
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so
	@echo install path=$(INSTALPATH)

install:
	cp -p lib$(LIBNAME).a  $(INSTALPATH)/alg_lib
	cp -p $(HDRS)  $(INSTALPATH)/alg_include


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
# Revision 1.2  2007/11/08 04:02:37  balewski
# run on l2ana as well
#
# Revision 1.1  2007/10/25 02:07:17  balewski
# added L2upsilon & binary event dump
#
# Revision 1.1  2007/10/11 00:33:15  balewski
# L2algo added
#
# Revision 1.3  2006/03/11 17:08:32  balewski
# now CVS comments should work
#
#
