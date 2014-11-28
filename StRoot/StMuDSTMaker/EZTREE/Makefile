LIBNAME  = EztBranch
HDRS     = $(wildcard Ezt*.h) 
SRCS     = $(wildcard Ezt*.cxx)

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

all:    $(LIBNAME)
#       -rm  $(LINK) $(DICT).*

$(LIBNAME): $(OBJS)
	$(AR)  lib$(LIBNAME).a $^
ifndef IN_PANITKIN
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so
endif

clean:
	-rm -f *.o $(LINK) $(DICT).* lib$(LIBNAME).* 


# ROOT STUFF
$(DICT).cxx: $(HDRS) $(LINK)
	rootcint -f $@ -c $(CXXFLAGS) $^

$(LINK):  $(HDRS)
	./mklinkdef.pl $(HDRS) > $(LINK)


%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@

