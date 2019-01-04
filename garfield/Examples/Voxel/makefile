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
#CFLAGS += -g

LDFLAGS = -L$(LIBDIR) -lGarfield
LDFLAGS += `root-config --glibs` -lGeom -lgfortran -lm
#LDFLAGS += -g

plotfield: plotfield.C 
	$(CXX) $(CFLAGS) plotfield.C
	$(CXX) -o plotfield plotfield.o $(LDFLAGS)
	rm plotfield.o

savefield: savefield.C 
	$(CXX) $(CFLAGS) savefield.C
	$(CXX) -o savefield savefield.o $(LDFLAGS)
	rm savefield.o

