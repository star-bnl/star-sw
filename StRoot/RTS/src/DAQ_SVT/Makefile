################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_svt.o svt_reader.o

LIBS = libsvt.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libsvt.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_svt.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_svt.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_svt.o: ../DAQ_READER/daqReader.h
daq_svt.o: ../DAQ_READER/daqConfig.h
daq_svt.o: ../DAQ_READER/daq_dta.h
daq_svt.o: ../../../../StRoot/RTS/include/rts.h
daq_svt.o: ../DAQ_READER/daq_dta_structs.h
daq_svt.o: daq_svt.h
daq_svt.o: ../DAQ_READER/daq_det.h
svt_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
svt_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
svt_reader.o: ../../trg/include/trgDataDefs_46.h
svt_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
svt_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
svt_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
svt_reader.o: ../../../../StRoot/RTS/include/rts.h
svt_reader.o: ../DAQ_READER/daq_det.h
svt_reader.o: ../../../../StRoot/RTS/include/SVT/key_map.h
svt_reader.o: daq_svt.h
svtReader.o: ../DAQ_READER/daqReader.h
svtReader.o: ../DAQ_READER/daqConfig.h
svtReader.o: ../DAQ_READER/daq_dta.h
svtReader.o: ../../../../StRoot/RTS/include/rts.h
svtReader.o: ../DAQ_READER/daq_dta_structs.h
svtReader.o: ../DAQ_READER/daq_det.h
svtReader.o: svtReader.h
svtReader.o: daq_svt.h
daq_svt.o: ../DAQ_READER/daq_det.h
svtReader.o: daq_svt.h
svtReader.o: ../DAQ_READER/daq_det.h
