LIBNAME  = StEmcDecoder

HDRS     = StEmcDecoder.h
SRCS     = StEmcDecoder.cxx

CXX    = g++
LD     = g++

DEBUG  =-g -O 
DEFINES=

ROOTCFLAGS    = $(shell root-config --cflags) 
ROOTLIBS      = $(shell root-config --libs) -lNew
OBJS   = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))

CFLAGS   = -Wall $(DEBUG) $(DEFINES) -fPIC $(ROOTCFLAGS) -I../../St_base

CXXFLAGS += $(CFLAGS)

LDFLAGS  = -shared 

all:	$(LIBNAME)

$(LIBNAME): $(OBJS)
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so

clean:
	-rm -f *.o lib$(LIBNAME).*

%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@
