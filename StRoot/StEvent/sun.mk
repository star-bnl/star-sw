CXX          := CC
STDHOME      := /afs/rhic/star/packages/ObjectSpace/2.0m
CPPFLAGS     := -I$(STDHOME) -I$(STDHOME)/ospace/std 
CPPFLAGS     += -DST_NO_MEMBER_TEMPLATES -DST_NO_NUMERIC_LIMITS
CPPFLAGS     += -DST_NO_EXCEPTIONS -DST_NO_TEMPLATE_DEF_ARGS
CPPFLAGS     += -DST_NO_NAMESPACES
CPPFLAGS     += -I/afs/rhic/star/ROOT/2.20/root/include
CPPFLAGS     += -I/afs/rhic/star/packages
CPPFLAGS     += -I/afs/rhic/star/packages/dev/inc
CPPFLAGS     += -I$(BFWORK)/include
CPPFLAGS     += -I..
CXXFLAGS     := -pic -Qoption ld -t
LD	     := $(CXX)
LDFLAGS      := -G
LIBRARY      = libtdm.sl

OBJS	      = StCtbCounter.o \
		StDedx.o \
		StEvent.o \
		StDstEventSummary.o \
		StFtpcHit.o \
		StGlobalTrack.o \
		StHit.o \
		StL0Trigger.o \
		StMwcSector.o \
		StRun.o \
		StRunSummary.o \
		StSvtHit.o \
		StTpcHit.o \
		StTrack.o \
		StTrackFitTraits.o \
		StTrigger.o \
		StTriggerDetectorCollection.o \
		StV0Vertex.o \
		StVertex.o \
		StVpdCounter.o \
		StVpdSummary.o \
		StZdcSegment.o \
		StZdcSummary.o

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS)
		@echo "Loading $(LIBRARY) ..."
		@ar cru $(LIBRARY) $(OBJS)
		@echo "done"

