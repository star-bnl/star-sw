################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_btow.o

LIBS = libbtow.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libbtow.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_btow.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_btow.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_btow.o: ../SFS/sfs_index.h
daq_btow.o: ../SFS/fs_index.h
daq_btow.o: ../SFS/sfs_base.h
daq_btow.o: ../DAQ_READER/daqReader.h
daq_btow.o: ../DAQ_READER/daqConfig.h
daq_btow.o: ../DAQ_READER/daq_dta.h
daq_btow.o: ../../../../StRoot/RTS/include/rts.h
daq_btow.o: ../DAQ_READER/daq_dta_structs.h
daq_btow.o: ../DAQ_EMC/daq_emc.h
daq_btow.o: ../DAQ_READER/daq_det.h
daq_btow.o: daq_btow.h
daq_btow.o: ../DAQ_READER/daq_det.h
daq_btow.o: ../DAQ_EMC/daq_emc.h
