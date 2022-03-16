
RTS_DIR_PATH = ../
include ../src/rtsmakefile.def

# disable the loading of obsolete daqlibs from /RTS
LDLIBS := 

include ../src/rtsplus.def

DEFINES += -DRTS_LOG_COLORED

VOBJS += libbsmd.a libbtow.a libemc.a libesmd.a libfpd.a libftp.a libl3.a libpmd.a libpp2pp.a \
	libric.a libsc.a libssd.a libsvt.a libtof.a libtpc.a libtpcfcf.a libtpx.a libtrg.a libetow.a \
	libdaqReader.a libsfs.a librtslog.a

VPATH += ../src/DAQ_BSMD ../src/DAQ_BTOW ../src/DAQ_EMC ../src/DAQ_ESMD ../src/DAQ_FPD ../src/DAQ_FTP ../src/DAQ_L3 ../src/DAQ_PMD \
	../src/DAQ_PP2PP ../src/DAQ_RIC ../src/DAQ_SC ../src/DAQ_SSD ../src/DAQ_SVT ../src/DAQ_TOF ../src/DAQ_TPC ../src/FCF ../src/DAQ_ETOW \
	../src/DAQ_TPX ../src/DAQ_TRG \
	../src/DAQ_READER  ../src/SFS ../src/LOG 


EVENT_TRACKER_OBJS = 	FtfFinder.o \
			l3CoordinateTransformer.o \
			FtfSl3.o \
			FtfDedx.o \
			FtfPara.o \
			FtfTrack.o \
			FtfBaseTrack.o \
			l3Swap.o \
			FtfBaseHit.o \
			FtfHit.o \
			FtfUtilities.o \
			gl3Event.o \
			gl3EMC.o \
			gl3Track.o \
			l3TrgReader.o \
			l3TrgReaderV20.o \
			l3TrgReaderV21.o \
			l3TrgReaderV12.o \
			gl3LMVertexFinder.o \
			gl3Hit.o \
			gl3Histo.o \
			l3BankUtils.o \
			l3EmcCalibration.o \
			gl3HistoManager.o \
			eventTrackerLib.o


LIBS = libEventTracker.a

BINS = eventTracker

all: $(LIBS) $(BINS)

libs: $(LIBS)

libEventTracker.a: $(EVENT_TRACKER_OBJS) $(DAQREADER_OBJS)
	$(RTS_AR)

eventTracker: eventTracker.o libEventTracker.a $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wno-write-strings -Wl,--whole-archive $^ -Wl,--no-whole-archive



#depend:
#	$(MAKEDEPEND)


#clean:
#	rm -f core* *.o *.a *~ displayMap eventTracker

