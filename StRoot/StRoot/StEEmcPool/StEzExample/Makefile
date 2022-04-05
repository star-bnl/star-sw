LIBNAME  = EzEEtower
HDRS     = $(wildcard EE*.h) EzEEtowerExample.h
SRCS     = $(wildcard EE*.cxx)  EzEEtowerExample.cxx


CXX    = g++
LD     = g++
AR     = ar rul

DEBUG  =-g -O 
DEFINES=

ROOTCFLAGS    = $(shell root-config --cflags) 
ROOTLIBS      = $(shell root-config --libs) -lNew

#
LINK   = LinkDef.h
DICT   = Dict_$(LIBNAME)
SRCS  += $(DICT).cxx
OBJS   = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))

#
CFLAGS   = -Wall $(DEBUG) $(DEFINES) -fPIC $(ROOTCFLAGS)
CXXFLAGS = $(CFLAGS)  -I../../../StRoot -I../../../ -DStRootFREE

LDFLAGS  = -shared 

all:	$(LIBNAME)
#	-rm  $(LINK) $(DICT).*	

$(LIBNAME): $(OBJS)
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so
	$(AR)  lib$(LIBNAME).a $^

clean:
	-rm -f *.o $(LINK) $(DICT).* lib$(LIBNAME).* 


# ROOT STUFF
$(DICT).cxx: $(HDRS) $(LINK)
	rootcint -f $@ -c $(CXXFLAGS) $^

$(LINK):  $(HDRS)
	./mklinkdef.pl $(HDRS) > $(LINK)



%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@

