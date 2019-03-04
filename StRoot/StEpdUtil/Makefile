# Define compiler
CXX = g++

# Define flags. -D_VANILLA_ROOT_ is needed to avoid StMessMgr confusion
CFLAGS = $(shell root-config --cflags) -O2 -fPIC -Wall -pipe -std=c++11 -D_VANILLA_ROOT_ -I. -I..
LIBS = $(shell root-config --libs)
INCS = $(shell root-config --incdir)

# Define output library
STEPDUTIL = libStEpdUtil.so

# Compile all *.cxx classes in the directory
SRC = $(shell find . -name "*.cxx")

all: $(STEPDUTIL)

# $(SRC:.cc=.o)
$(STEPDUTIL): $(SRC:.cxx=.o) StEpdUtil_Dict.C
	$(CXX) $(CFLAGS) -shared $^ -o $(STEPDUTIL) $(LIBS)

%.o: %.cxx
	$(CXX) -fPIC $(CFLAGS) -c -o $@ $<

StEpdUtil_Dict.C: $(shell find . -name "*.h" ! -name "*LinkDef*")
	rootcint -f $@ -c -D_VANILLA_ROOT_ -DROOT_CINT -D__ROOT__ -I. -I.. -I$(INCS) $^ StEpdUtilLinkDef.h

.PHONY: clean distclean

clean:
	rm -vf *.o StarRoot/*.o StEpdUtil_Dict*

distclean:
	rm -vf *.o StarRoot/*.o StEpdUtil_Dict* $(STEPDUTIL)
