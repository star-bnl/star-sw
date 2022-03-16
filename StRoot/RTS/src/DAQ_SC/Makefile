################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_sc.o sc_reader.o

LIBS = libsc.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libsc.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_sc.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_sc.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_sc.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_sc.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_sc.o: ../../trg/include/trgDataDefs_46.h
daq_sc.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_sc.o: ../SFS/sfs_index.h
daq_sc.o: ../SFS/fs_index.h
daq_sc.o: ../SFS/sfs_base.h
daq_sc.o: ../DAQ_READER/daqReader.h
daq_sc.o: ../DAQ_READER/daqConfig.h
daq_sc.o: ../DAQ_READER/daq_dta.h
daq_sc.o: ../../../../StRoot/RTS/include/rts.h
daq_sc.o: ../DAQ_READER/daq_dta_structs.h
daq_sc.o: daq_sc.h
daq_sc.o: ../DAQ_READER/daq_det.h
sc_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
sc_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
sc_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
sc_reader.o: ../../trg/include/trgDataDefs_46.h
sc_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
sc_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
sc_reader.o: ../../../../StRoot/RTS/include/rts.h
sc_reader.o: daq_sc.h
sc_reader.o: ../DAQ_READER/daq_det.h
scReader.o: ../DAQ_READER/daqReader.h
scReader.o: ../DAQ_READER/daqConfig.h
scReader.o: ../DAQ_READER/daq_dta.h
scReader.o: ../../../../StRoot/RTS/include/rts.h
scReader.o: ../DAQ_READER/daq_dta_structs.h
scReader.o: ../DAQ_READER/daq_det.h
scReader.o: scReader.h
scReader.o: daq_sc.h
daq_sc.o: ../DAQ_READER/daq_det.h
sc.o: daq_sc.h
sc.o: ../DAQ_READER/daq_det.h
scReader.o: daq_sc.h
scReader.o: ../DAQ_READER/daq_det.h
