CXX           = aCC -I/usr/local/root/include -I/home/star/ullrich/wrk/StarClassLibrary/scl/include

LD            = $(CXX)

CXXFLAGS      =

DEST	      = .

EXTHDRS	      =

HDRS	      =

INSTALL	      = /usr/sbin/install

LIBRARY	      = libtdm.a

MAKEFILE      = Makefile

OBJS	      = StCtbCounter.o \
		StDedx.o \
		StEvent.o \
		StEventSummary.o \
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

PRINT	      = pr

SHELL	      = /usr/bin/sh

SRCS	      = StCtbCounter.cc \
		StDedx.cc \
		StEvent.cc \
		StEventSummary.cc \
		StFtpcHit.cc \
		StGlobalTrack.cc \
		StHit.cc \
		StL0Trigger.cc \
		StMwcSector.cc \
		StRun.cc \
		StRunSummary.cc \
		StSvtHit.cc \
		StTpcHit.cc \
		StTrack.cc \
		StTrackFitTraits.cc \
		StTrigger.cc \
		StTriggerDetectorCollection.cc \
		StV0Vertex.cc \
		StVertex.cc \
		StVpdCounter.cc \
		StVpdSummary.cc \
		StZdcSegment.cc \
		StZdcSummary.cc

SYSHDRS	      = /usr/include/.unsupp/sys/_types.h \
		/usr/include/float.h \
		/usr/include/sys/_inttypes.h \
		/usr/include/sys/sigevent.h \
		/usr/include/sys/stdsyms.h \
		/usr/include/sys/time.h \
		/usr/include/sys/types.h \
		/usr/include/time.h

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS)
		@echo "Loading $(LIBRARY) ..."
		@ar cru $(LIBRARY) $(OBJS)
		@echo "done"

clean:;		@rm -f $(OBJS) core

clobber:;	@rm -f $(OBJS) $(LIBRARY) core tags

depend:;	@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

echo:;		@echo $(HDRS) $(SRCS)

extract:;	@ar x $(DEST)/$(LIBRARY)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
		@echo Installing $(LIBRARY) in $(DEST)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(LIBRARY); $(INSTALL) -f $(DEST) $(LIBRARY)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@$(MAKE) -f $(MAKEFILE) ROOT=$(ROOT) DEST=$(DEST) install
###
StCtbCounter.o: StCtbCounter.hh
StDedx.o: StDedx.hh
StEvent.o: StEvent.hh /usr/include/time.h /usr/include/sys/stdsyms.h \
	/usr/include/sys/time.h /usr/include/sys/types.h \
	/usr/include/sys/_inttypes.h /usr/include/.unsupp/sys/_types.h \
	/usr/include/sys/sigevent.h StEventSummary.hh StTHDefs.hh StRun.hh \
	StRunSummary.hh StEnumerations.hh StTrackCollection.hh \
	StGlobalTrack.hh StFtpcHit.hh StHit.hh StTpcHit.hh StSvtHit.hh \
	StDedx.hh StTrack.hh StTrackPidTraits.hh StTrackFitTraits.hh \
	StEmcHit.hh StSmdHit.hh StFtpcHitCollection.hh StVertexCollection.hh \
	StVertex.hh StSvtHitCollection.hh StTpcHitCollection.hh \
	StEmcHitCollection.hh StSmdHitCollection.hh StL0Trigger.hh \
	StTrigger.hh StTriggerDetectorCollection.hh StZdcSummary.hh \
	StVpdSummary.hh StZdcSegment.hh StVpdCounter.hh StMwcSector.hh \
	StCtbCounter.hh
StEventSummary.o: StEventSummary.hh StTHDefs.hh
StFtpcHit.o: StFtpcHit.hh StHit.hh StGlobalTrack.hh StTpcHit.hh StSvtHit.hh \
	StDedx.hh StTrack.hh StTrackPidTraits.hh StTrackFitTraits.hh \
	StEmcHit.hh StSmdHit.hh StTrackCollection.hh
StGlobalTrack.o: StGlobalTrack.hh StFtpcHit.hh StHit.hh StTpcHit.hh \
	StSvtHit.hh StDedx.hh StTrack.hh StTrackPidTraits.hh \
	StTrackFitTraits.hh StEmcHit.hh StSmdHit.hh
StHit.o: StHit.hh StGlobalTrack.hh StFtpcHit.hh StTpcHit.hh StSvtHit.hh \
	StDedx.hh StTrack.hh StTrackPidTraits.hh StTrackFitTraits.hh \
	StEmcHit.hh StSmdHit.hh StTrackCollection.hh
StL0Trigger.o: StL0Trigger.hh StTrigger.hh
StMwcSector.o: StMwcSector.hh
StRun.o: StRun.hh StRunSummary.hh /usr/include/time.h \
	/usr/include/sys/stdsyms.h /usr/include/sys/time.h \
	/usr/include/sys/types.h /usr/include/sys/_inttypes.h \
	/usr/include/.unsupp/sys/_types.h /usr/include/sys/sigevent.h \
	StTHDefs.hh StEnumerations.hh
StRunSummary.o: StRunSummary.hh /usr/include/time.h \
	/usr/include/sys/stdsyms.h /usr/include/sys/time.h \
	/usr/include/sys/types.h /usr/include/sys/_inttypes.h \
	/usr/include/.unsupp/sys/_types.h /usr/include/sys/sigevent.h \
	StTHDefs.hh
StSvtHit.o: StSvtHit.hh StHit.hh StGlobalTrack.hh StFtpcHit.hh StTpcHit.hh \
	StDedx.hh StTrack.hh StTrackPidTraits.hh StTrackFitTraits.hh \
	StEmcHit.hh StSmdHit.hh StTrackCollection.hh
StTpcHit.o: StTpcHit.hh StHit.hh StGlobalTrack.hh StFtpcHit.hh StSvtHit.hh \
	StDedx.hh StTrack.hh StTrackPidTraits.hh StTrackFitTraits.hh \
	StEmcHit.hh StSmdHit.hh StTrackCollection.hh
StTrack.o: StTrack.hh StTrackPidTraits.hh StTrackFitTraits.hh
StTrackFitTraits.o: StTrackFitTraits.hh
StTrigger.o: StTrigger.hh
StTriggerDetectorCollection.o: StTriggerDetectorCollection.hh StZdcSummary.hh \
	StEnumerations.hh StVpdSummary.hh StZdcSegment.hh StVpdCounter.hh \
	StMwcSector.hh StCtbCounter.hh
StV0Vertex.o: StV0Vertex.hh StVertex.hh StEnumerations.hh \
	/usr/include/float.h /usr/include/sys/stdsyms.h
StVertex.o: StVertex.hh StEnumerations.hh
StVpdCounter.o: StVpdCounter.hh
StVpdSummary.o: StVpdSummary.hh StEnumerations.hh
StZdcSegment.o: StZdcSegment.hh
StZdcSummary.o: StZdcSummary.hh StEnumerations.hh
