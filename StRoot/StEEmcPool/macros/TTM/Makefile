# Hey emacs this IS a -*-Makefile-*-
# =============================================================================
# (c) Piotr A. Zolnierczuk (IUCF)
# A nightmare of Makefile and 1 billion of libraries
# =============================================================================
CXXFLAGS = -Wall -g -O2 -DDEBUG
LDFLAGS  = 
SRCS     = 
PROGS    = mipcalib
VPATH    = macros:StRoot/StEEmcPool/macros/TTM
# =============================================================================
# ROOT FLAGS/LIBS
ROOTCFLAGS    = $(shell root-config --cflags)
ROOTLIBS      = $(shell root-config --glibs ) -lNew

# =============================================================================
CXXFLAGS +=  $(EEMCINC) $(ROOTCFLAGS) 

LDFLAGS  +=
LDLIBS    := $(EEMCLIB) $(ROOTLIBS) 

# ==============================================================================
OBJS   = $(notdir $(patsubst %.C,%.o,$(SRCS)))


# ==============================================================================
# real stuff
all :	$(PROGS) 

mipcalib: mipcalib.o   $(OBJS)
	 $(CXX) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean::
	rm -f *.o .depend
	rm -f $(PROGS) 

depend .depend : $(SRCS) $(notdir $(patsubst %,%.C,$(PROGS))) 
	$(CXX) -M $(CXXFLAGS) $^ > ./.depend

%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@

-include .depend
