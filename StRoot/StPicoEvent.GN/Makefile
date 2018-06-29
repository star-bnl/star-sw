# Define compiler
CXX = g++

# Define flags. -D_VANILLA_ROOT_ is needed to avoid StMessMgr confusion
CFLAGS = $(shell root-config --cflags) -g -fPIC -Wall -std=c++11 -D_VANILLA_ROOT_ -I.
LIBS = $(shell root-config --libs) 

# Define output library
STPICODST = libStPicoDst.so

# Compile all *.cxx classes in the directory
SRC = $(shell find . -name "*.cxx")

all: $(STPICODST)

# $(SRC:.cc=.o) 
$(STPICODST): $(SRC:.cxx=.o) StPicoDict.cxx
	$(CXX) $(CFLAGS) -shared $^ -o $(STPICODST) $(LIBS)

%.o: %.cxx
	$(CXX) -fPIC $(CFLAGS) -c -o $@ $<

StPicoDict.cxx: $(shell find . -name "*.h" ! -name "*LinkDef*")
	rootcint -f $@ -c $(CFLAGS) -p $^ StPicoDstLinkDef.h

clean:
	rm -vf *.o StPicoDict*

distclean:
	rm -vf *.o StPicoDict* $(STPICODST)
