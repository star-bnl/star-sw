LIBNAME  = EEmcDsm

HDRS     = $(wildcard EE*.h) 
SRCS     = $(wildcard EE*.cxx) 

ifdef IN_PANITKIN
# nothing
endif


CXX    = g++
LD     = g++
AR     = ar rul

DEBUG  =-g -O 
DEFINES=

ifdef IN_PANITKIN
DEFINES = -DIN_PANITKIN	
endif


ROOTCFLAGS    = $(shell root-config --cflags) 
ROOTLIBS      = $(shell root-config --libs) -lNew
#
ifndef IN_PANITKIN
LINK   = LinkDef.h
DICT   = Dict_$(LIBNAME)
SRCS  += $(DICT).cxx
endif
OBJS   = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))

#
CFLAGS   = -Wall $(DEBUG) $(DEFINES) -fPIC $(ROOTCFLAGS)


ifdef IN_PANITKIN
  CXXFLAGS = $(CFLAGS)  -I../../../StRoot  -I/RTS/src/EVP_READER -I/RTS/include
else
  CXXFLAGS = $(CFLAGS)  -I../StRoot  -I../online/RTS/src/EVP_READER
endif

LDFLAGS  = -shared 


all:	$(LIBNAME)
#	-rm  $(LINK) $(DICT).*	

$(LIBNAME): $(OBJS)
	@echo Panitkin=$(IN_PANITKIN)	
	rm -f  lib$(LIBNAME).*
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
	rm -f *.a *.so;
	$(CXX) $(CXXFLAGS) -c $< -o $@

