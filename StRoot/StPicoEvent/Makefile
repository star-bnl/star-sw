# Define compiler
CXX = g++

# Define flags. -D_VANILLA_ROOT_ is needed to avoid StMessMgr confusion
CFLAGS = $(shell root-config --cflags) -O2 -fPIC -Wall -W -Woverloaded-virtual -Wno-deprecated-declarations
CFLAGS += -pipe -std=c++11 -D_VANILLA_ROOT_ -I.
LIBS = $(shell root-config --libs)
INCS = $(shell root-config --incdir)

# Define output library
STPICODST = libStPicoDst.so

# Compile all *.cxx classes in the directory
SRC = $(shell find . -name "*.cxx")

all: $(STPICODST)

# $(SRC:.cc=.o)
$(STPICODST): $(SRC:.cxx=.o) StPicoDst_Dict.C
	$(CXX) $(CFLAGS) -shared $^ -o $(STPICODST) $(LIBS)

%.o: %.cxx
	$(CXX) -fPIC $(CFLAGS) -c -o $@ $<

StPicoDst_Dict.C: $(shell find . -name "*.h" ! -name "*LinkDef*")
	rootcint -f $@ -c -D_VANILLA_ROOT_ -DROOT_CINT -D__ROOT__ -I. -I$(INCS) $^ StPicoDstLinkDef.h

.PHONY: clean distclean

clean:
	rm -vf *.o StarRoot/*.o StPicoDst_Dict*

distclean:
	rm -vf *.o StarRoot/*.o StPicoDst_Dict* $(STPICODST)
