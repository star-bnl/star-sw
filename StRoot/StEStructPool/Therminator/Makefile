CC	= g++
FF	= g77
LD	= g++


BINARIES = therm_events therm_tree
HSOURCES = Parser.cxx ParticleType.cxx DecayChannel.cxx Integrator.cxx ParticleDB.cxx DecayTable.cxx Particle.cxx Event.cxx ParticleDecayer.cxx ReadPar.cxx
HEADERS  = $(HSOURCES:.cxx=.h)
SOURCES  = $(HSOURCES) therm_events.cxx
OBJECTS  = $(SOURCES:.cxx=.o) 

CPPOPT = -Wno-deprecated `root-config --cflags`
COPTN = -c -O0 -g ${CPPOPT} -D_DEBUG_LEVEL_=1
COPT = -c -O0 -g ${CPPOPT}  -D_DEBUG_LEVEL_=1
LLIB = `root-config --libs` -L$(ROOTSYS)/lib -lm  -lgcc -g
#CPPOPT = -Wno-deprecated `root-config --cflags`
#COPTN = -c -O3 ${CPPOPT} -D_DEBUG_LEVEL_=1
#COPT = -c -O3 ${CPPOPT}  -D_DEBUG_LEVEL_=1
#LLIB = `root-config --libs` -L$(ROOTSYS)/lib -lm  -lgcc
LOPT = 

all: $(BINARIES)

therm_events: $(OBJECTS)
	$(LD) $(LOPT) -o $@ $(OBJECTS) $(LLIB)

therm_tree: therm_tree.o
	$(LD) $(LOPT) -o $@ therm_tree.o $(LLIB) 

therm_tree_tlast.o: therm_tree_tlast.C
	$(CC) $(COPT) -o $@ $^

therm_tree_tlast: therm_tree_tlast.o
	$(LD) $(LOPT) -o $@ therm_tree_tlast.o $(LLIB) 

therm_tree.o: therm_tree.C
	$(CC) $(COPT) -o $@ $^

clean:
	rm -f *o therm_events therm_tree therm_events.exe therm_tree.exe *.d 

%.o: %.cxx
	$(CC) -o $@ $< $(COPT)

%.d: %.cxx
	$(SHELL) -ec '$(CC) -MM $(CPPOPT) $< | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; [ -s $@ ] || rm -f $@'

include $(SOURCES:.cxx=.d) 

package: $(SOURCES) $(HEADERS) Makefile  therminator.in therm_tree.C ./share/particles.data ./share/decays.data THGlobal.h
	tar zcvf therminator.tar.gz $^

