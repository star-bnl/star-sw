include ../make.inc

SOURCES_C=photosC.cxx forZ-MEc.cxx forW-MEc.cxx pairs.cxx HEPEVT_struct.cxx
OBJECTS_C=$(SOURCES_C:.cxx=.o)

%.o: %.cxx
	$(CC) $(CFLAGS) $(DEBUG) -I. -I../photosCInterfaces -I../utilities -c $< -o $@

all: $(OBJECTS_C)

clean:
	rm -f *.o *~;
