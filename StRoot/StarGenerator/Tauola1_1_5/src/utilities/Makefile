include ../make.inc

SOURCES_CC=Log.cxx Plots.cxx
OBJECTS_CC=$(SOURCES_CC:.cxx=.o)

%.o: %.cxx
	$(CC) $(CFLAGS) $(DEBUG) -I. -I../tauolaCInterfaces -I../tauolaFortranInterfaces -I../eventRecordInterfaces -c $< -o $@

all:   $(OBJECTS_CC)

clean:
	rm -f *.o; rm -f *~;
