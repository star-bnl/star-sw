LIBNAME  = EEevent
HDRS     = $(wildcard EE*.h)
SRCS     = $(wildcard EE*.cxx) 


CXX    = g++
LD     = g++

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
CXXFLAGS = $(CFLAGS)
LDFLAGS  = -shared 

all:	$(LIBNAME)


$(LIBNAME): $(OBJS)
	$(CXX) $(LDFLAGS) $^ -o lib$(LIBNAME).so

clean:
	-rm -f *.o $(LINK) $(DICT).* lib$(LIBNAME).* 


# ROOT STUFF
$(DICT).cxx: $(HDRS) $(LINK)
	rootcint -f $@ -c $(CXXFLAGS) $^

$(LINK):  $(HDRS)
	./mklinkdef.pl $(HDRS) > $(LINK)



%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@

#
#EEfeeRawEvent.o:	EEfeeRawEvent.h
