################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_esmd.o emcReader.o

LIBS = libesmd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libesmd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_esmd.o: ../../include/rtsLog.h
daq_esmd.o: ../../include/rtsSystems.h
daq_esmd.o: ../SFS/sfs_index.h
daq_esmd.o: ../SFS/fs_index.h
daq_esmd.o: ../SFS/sfs_base.h
daq_esmd.o: ../RTS_READER/rts_reader.h
daq_esmd.o: ../RTS_READER/daq_dta.h
daq_esmd.o: ../../include/rts.h
daq_esmd.o: ../RTS_READER/daq_dta_structs.h
daq_esmd.o: daq_esmd.h
daq_esmd.o: ../RTS_READER/daq_det.h
emcReader.o: ../../include/rtsLog.h
emcReader.o: ../../include/rtsSystems.h
emcReader.o: ../../include/rts.h
emcReader.o: ../../include/daqFormats.h
emcReader.o: ../../include/SECTOR/sector.h
emcReader.o: ../../trg/include/trgStructures.h
emcReader.o: ../../include/L3/L3Formats.h
emcReader.o: ../DAQ_LEGACY/daq_legacy.h
daq_esmd.o: ../RTS_READER/daq_det.h
