include ../make.inc

SOURCES_CC=TauolaParticle.cxx TauolaParticlePair.cxx \
	   TauolaEvent.cxx Tauola.cxx \
	   DecayList.cxx
OBJECTS_CC=$(SOURCES_CC:.cxx=.o)

%.o: %.cxx
	$(CC) $(CFLAGS) $(DEBUG) -I. -I../utilities -I../tauolaFortranInterfaces -I../eventRecordInterfaces -c $< -o $@

all:   $(OBJECTS_CC)

clean:
	rm -f *.o; rm -f *~;
