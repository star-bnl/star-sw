include ../make.inc

SOURCES_CC = TauolaHEPEVTEvent.cxx TauolaHEPEVTParticle.cxx

ifneq ($(HEPMCLOCATION), )
  SOURCES_CC += TauolaHepMCParticle.cxx TauolaHepMCEvent.cxx
endif

OBJECTS_CC=$(SOURCES_CC:.cxx=.o)

%.o: %.cxx
	$(CC) $(CFLAGS) $(DEBUG) -I. -I../tauolaCInterfaces -I../tauolaFortranInterfaces -I../utilities -I$(HEPMCLOCATION)/include -c $< -o $@

all:   $(OBJECTS_CC)

clean:                                                     
	rm -f *.o; rm -f *~;	

