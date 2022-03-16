################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_fpd.o fpd_reader.o

LIBS = libfpd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libfpd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_fpd.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_fpd.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_fpd.o: ../DAQ_READER/daqReader.h
daq_fpd.o: ../DAQ_READER/daqConfig.h
daq_fpd.o: ../DAQ_READER/daq_dta.h
daq_fpd.o: ../../../../StRoot/RTS/include/rts.h
daq_fpd.o: ../DAQ_READER/daq_dta_structs.h
daq_fpd.o: daq_fpd.h
daq_fpd.o: ../DAQ_READER/daq_det.h
fpd_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
fpd_reader.o: ../../../../StRoot/RTS/include/rts.h
fpd_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
fpd_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
fpd_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
fpd_reader.o: ../../trg/include/trgDataDefs_46.h
fpd_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
fpd_reader.o: daq_fpd.h
fpd_reader.o: ../DAQ_READER/daq_det.h
fpdReader.o: ../DAQ_READER/daqReader.h
fpdReader.o: ../DAQ_READER/daqConfig.h
fpdReader.o: ../DAQ_READER/daq_dta.h
fpdReader.o: ../../../../StRoot/RTS/include/rts.h
fpdReader.o: ../DAQ_READER/daq_dta_structs.h
fpdReader.o: ../DAQ_READER/daq_det.h
fpdReader.o: fpdReader.h
fpdReader.o: daq_fpd.h
daq_fpd.o: ../DAQ_READER/daq_det.h
fpdReader.o: daq_fpd.h
fpdReader.o: ../DAQ_READER/daq_det.h
