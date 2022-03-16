################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_ssd.o ssd_reader.o

LIBS = libssd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libssd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_ssd.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_ssd.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_ssd.o: ../DAQ_READER/daqReader.h
daq_ssd.o: ../DAQ_READER/daqConfig.h
daq_ssd.o: ../DAQ_READER/daq_dta.h
daq_ssd.o: ../../../../StRoot/RTS/include/rts.h
daq_ssd.o: ../DAQ_READER/daq_dta_structs.h
daq_ssd.o: daq_ssd.h
daq_ssd.o: ../DAQ_READER/daq_det.h
ssd_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
ssd_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ssd_reader.o: ../../trg/include/trgDataDefs_46.h
ssd_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ssd_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
ssd_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
ssd_reader.o: ../../../../StRoot/RTS/include/rts.h
ssd_reader.o: daq_ssd.h
ssd_reader.o: ../DAQ_READER/daq_det.h
ssdReader.o: ../DAQ_READER/daq_dta.h
ssdReader.o: ../../../../StRoot/RTS/include/rts.h
ssdReader.o: ../DAQ_READER/daq_dta_structs.h
ssdReader.o: ../DAQ_READER/daq_det.h
ssdReader.o: ssdReader.h
ssdReader.o: daq_ssd.h
daq_ssd.o: ../DAQ_READER/daq_det.h
ssdReader.o: daq_ssd.h
ssdReader.o: ../DAQ_READER/daq_det.h
