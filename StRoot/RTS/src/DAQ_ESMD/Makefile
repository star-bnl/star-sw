################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_esmd.o

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
daq_esmd.o: ../DAQ_READER/daqReader.h
daq_esmd.o: ../DAQ_READER/daqConfig.h
daq_esmd.o: ../DAQ_READER/daq_dta.h
daq_esmd.o: ../../../../StRoot/RTS/include/rts.h
daq_esmd.o: ../DAQ_READER/daq_dta_structs.h
daq_esmd.o: daq_esmd.h
daq_esmd.o: ../DAQ_READER/daq_det.h
daq_esmd.o: ../DAQ_EMC/daq_emc.h
daq_esmd.o: ../DAQ_READER/daq_det.h
daq_esmd.o: ../DAQ_EMC/daq_emc.h
