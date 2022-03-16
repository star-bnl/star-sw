################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_rhicf.o

LIBS = librhicf.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

librhicf.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_rhicf.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_rhicf.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_rhicf.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_rhicf.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_rhicf.o: ../../trg/include/trgDataDefs_46.h
daq_rhicf.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_rhicf.o: ../SFS/sfs_index.h
daq_rhicf.o: ../SFS/fs_index.h
daq_rhicf.o: ../SFS/sfs_base.h
daq_rhicf.o: ../DAQ_READER/daqReader.h
daq_rhicf.o: ../DAQ_READER/daqConfig.h
daq_rhicf.o: ../DAQ_READER/daq_dta.h
daq_rhicf.o: ../../../../StRoot/RTS/include/rts.h
daq_rhicf.o: ../DAQ_READER/daq_dta_structs.h
daq_rhicf.o: daq_rhicf.h
daq_rhicf.o: ../DAQ_READER/daq_det.h
daq_rhicf.o: ../DAQ_READER/daq_det.h
