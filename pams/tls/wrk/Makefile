EXEC =	test_sorts

FFLAGS = -extend_source -O
#FFLAGS = +e -O +ppu

INCLUDE =	-I../inc

OBJECTS =	test_sort_routines.o

LIBS =	-L../lib/irix -ltls
#LIBS =	-L../lib/hpux -ltls

.f.o:
	f77 $(FFLAGS) $(INCLUDE) -c  $*.F

$(EXEC): $(OBJECTS)
	f77 $(FFLAGS) -o $(EXEC) $(OBJECTS) $(LIBS)

