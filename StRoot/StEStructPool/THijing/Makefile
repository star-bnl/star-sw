# This Makefile tries to call Hijing from C++
# Chunhui Han (chunhuih@u.washington.edu)
ROOTARCH   = $(shell root-config --arch)
ROOTCFLAGS = $(shell root-config --cflags)
ROOTLIBS   = $(shell root-config --libs)
ROOTGLIBS  = $(shell root-config --glibs)
ROOTINCDIR = $(shell root-config --incdir)


# -fno-automatic: treat local variables and common blocks as if they are
# in the SAVE statement
# When the compiler option '-fPIC' is added to compile fortran code,
# the result is different from without.
# This switch is removed to have identical result as the Hijing fortran program.
CERNLIBS = -L$(CERN_ROOT)/lib -lpacklib -lkernlib -lmathlib -lpawlib -lgraflib
CCFLAGS  = -g -Wall -O -fno-second-underscore -fno-automatic
FORFLAGS = -g -m32 -Wall -fPIC -O -fno-second-underscore -fno-automatic
CXXFLAGS = -g -m32 -Wall -fPIC -O -fno-automatic 
SOFLAGS  = -fPIC -shared -m32
CXXLDFLAGS = -lg2c
CXX      = g++
FOR      = g77
LD       = g++
FORLD    = g77

OBJS  = aranlux.o hijev.o  hijing.o hipyset.o
all: THijing.so
# mix fortran and C++ in the linking.
# Note that the library -lg2c is needed if g++ is used to link
# fortran and C++ objects. This argument has to be at the end.
THijing.so: $(OBJS) THijing.o THijingDict.o StEStructHijing.o StEStructPoolTHijing_Cint.o
	g77 $(SOFLAGS) -o $@ $^

THijing.o: THijing.cxx THijing.h
	$(CXX) $(CXXFLAGS) -c THijing.cxx
THijingDict.o: THijingDict.C
	$(CXX) $(CXXFLAGS) -I$(ROOTINCDIR) -c $^
THijingDict.C: THijing.h
	rootcint -f $@ -c $^
StEStructHijing.o: StEStructHijing.cxx StEStructHijing.h
	$(CXX) $(CXXFLAGS) -c StEStructHijing.cxx
StEStructHijingDict.o: StEStructHijingDict.C
	$(CXX) $(CXXFLAGS) -I$(ROOTINCDIR) -c $^
StEStructHijingDict.C: StEStructHijing.h
	rootcint -f $@ -c $^
aranlux.o:  aranlux.f
	$(FOR) $(FORFLAGS) -c $^
hijev.o:  hijev.f
	$(FOR) $(FORFLAGS) -c $^
hipyset.o: hipyset.f
	$(FOR) $(FORFLAGS) -c $^
hijing.o: hijing.f
	$(FOR) $(FORFLAGS) -c $^
clean:
	rm -f *o core *~
