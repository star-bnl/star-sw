

LIBNAME   = L2wAlgo
HDRS      = L2wResult2009.h   L2wBemc2009.h   L2wEemc2009.h 
SRCS      = L2wBemc2009.cxx L2wEemc2009.cxx
CXX    = g++
AR     = ar rul

DEBUG  =-g -O 
INCL =-I ../L2algoUtil

# some details may differ for the real L2-machine
ifndef STAR
  DEFINES = -DIS_REAL_L2
  INCL +=-I  /home/startrg/trg/trg_soft_dev/include/ -I /RTS_cvs/StRoot/RTS/include	
else
  DEFINES=
  INCL += -I../../../ -I/afs/rhic.bnl.gov/star/packages/DEV/StRoot 
endif

 INSTALPATH=../../../../onlineL2

#
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
	cp -p  $(HDRS)  $(INSTALPATH)/alg_include


clean:
	-rm -f *.o $(LINK)  lib*$(LIBNAME).*  .depend


depend .depend : $(SRCS) 
	$(CXX) -M $(CFLAGS) $^ > ./.depend


%.o: %.cxx
	echo $(CFLAGS)
	$(CXX) $(CFLAGS) -c $< -o $@

#
-include .depend
