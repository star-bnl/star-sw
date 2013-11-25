# **********************************************************************
#  
#  Very simple Makefile to build StDbLib.so & StDbLib.a
#
#  header file dependencies are extremely dumb -
#  - touch a header file & you rebuild the entire library.
#  but the rebuild is also very quick ~1minute.
#
#  flags & control information reside in file 'rules.make'
#
# **********************************************************************

STDBLIBSO  = StDbLib.so
STDBLIBA   = StDbLib.a
TARGETS	   = $(STDBLIBSO) $(STDBLIBA)  

all:: $(TARGETS)

include rules.make

SRCFILES1 := $(wildcard *.cc)
SRCFILES2 := $(wildcard *.cxx)
SRCFILES3 := $(wildcard *.cpp)

SRCFILES := $(SRCFILES1)
SRCFILES += $(SRCFILES2)
SRCFILES += $(SRCFILES3)

HEADERS  += $(wildcard *.h)
HHEADERS += $(wildcard *.hh)
LINK_OBJS1 := $(subst .cc,.o, $(SRCFILES1))
LINK_OBJS2 := $(subst .cxx,.o, $(SRCFILES2))
LINK_OBJS3 := $(subst .cpp,.o, $(SRCFILES3))
LINK_OBJS := $(LINK_OBJS1)
LINK_OBJS += $(LINK_OBJS2)
LINK_OBJS += $(LINK_OBJS3)



$(STDBLIBSO): $(LINK_OBJS) 
	rm -f $@
	$(CXX) $(SOFLAGS) -o $@ $(LINK_OBJS) $(CLIBS) $(CXXFLAGS)  $(SHARED_LIBS) 

$(STDBLIBA): $(LINK_OBJS) 
	rm -f $@
	$(AR) $(LDFLAGS) $@ $(LINK_OBJS)  
#	$(AR) $(LDFLAGS) $@ $(LINK_OBJS)  

$(LINK_OBJS): $(HEADERS) $(HHEADERS)

clean::
	rm -f $(TARGETS)
	rm -f *.o

#include .depend
# DO NOT DELETE THIS LINE -- make depend depends on it.










