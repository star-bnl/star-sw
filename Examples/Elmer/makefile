OBJDIR = $(GARFIELD_HOME)/Object
SRCDIR = $(GARFIELD_HOME)/Source
INCDIR = $(GARFIELD_HOME)/Include
HEEDDIR = $(GARFIELD_HOME)/Heed
LIBDIR = $(GARFIELD_HOME)/Library

# Compiler flags
CFLAGS = -Wall -Wextra -Wno-long-long \
	`root-config --cflags` \
	-O3 -fno-common -c \
	-I$(INCDIR) -I$(HEEDDIR)

# Debug flags
CFLAGS += -g

LDFLAGS = `root-config --glibs` -lGeom -lgfortran -lm
LDFLAGS += -L$(LIBDIR) -lGarfield
#LDFLAGS += -g

avalanche:	avalanche.cc
	$(CXX) $(CFLAGS) avalanche.cc
	$(CXX) -o avalanche avalanche.o $(LDFLAGS)
	rm avalanche.o
