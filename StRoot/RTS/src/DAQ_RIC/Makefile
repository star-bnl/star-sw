################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_ric.o ric_reader.o

LIBS = libric.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libric.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_ric.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_ric.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_ric.o: ../DAQ_READER/daqReader.h
daq_ric.o: ../DAQ_READER/daqConfig.h
daq_ric.o: ../DAQ_READER/daq_dta.h
daq_ric.o: ../../../../StRoot/RTS/include/rts.h
daq_ric.o: ../DAQ_READER/daq_dta_structs.h
daq_ric.o: daq_ric.h
daq_ric.o: ../DAQ_READER/daq_det.h
ric_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
ric_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
ric_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
ric_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ric_reader.o: ../../trg/include/trgDataDefs_46.h
ric_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ric_reader.o: ../../../../StRoot/RTS/include/rts.h
ric_reader.o: daq_ric.h
ric_reader.o: ../DAQ_READER/daq_det.h
ricReader.o: ../DAQ_READER/daqReader.h
ricReader.o: ../DAQ_READER/daqConfig.h
ricReader.o: ../DAQ_READER/daq_dta.h
ricReader.o: ../../../../StRoot/RTS/include/rts.h
ricReader.o: ../DAQ_READER/daq_dta_structs.h
ricReader.o: ../DAQ_READER/daq_det.h
ricReader.o: ricReader.h
ricReader.o: daq_ric.h
daq_ric.o: ../DAQ_READER/daq_det.h
ricReader.o: daq_ric.h
ricReader.o: ../DAQ_READER/daq_det.h
