################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_esmd.o emc_reader.o

LIBS = libesmd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libesmd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_esmd.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_esmd.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_esmd.o: ../SFS/sfs_index.h
daq_esmd.o: ../SFS/fs_index.h
daq_esmd.o: ../SFS/sfs_base.h
daq_esmd.o: ../RTS_READER/rts_reader.h
daq_esmd.o: ../RTS_READER/daq_dta.h
daq_esmd.o: ../../../../StRoot/RTS/include/rts.h
daq_esmd.o: ../RTS_READER/daq_dta_structs.h
daq_esmd.o: daq_esmd.h
daq_esmd.o: ../RTS_READER/daq_det.h
emc_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
emc_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
emc_reader.o: ../../../../StRoot/RTS/include/rts.h
emc_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
emc_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
emc_reader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
emc_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
emc_reader.o: ../DAQ_LEGACY/daq_legacy.h
daq_esmd.o: ../RTS_READER/daq_det.h
