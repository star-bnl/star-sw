################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_sst.o sstPed.o

LIBS = libsst.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libsst.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_sst.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_sst.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_sst.o: ../SFS/sfs_index.h
daq_sst.o: ../SFS/fs_index.h
daq_sst.o: ../SFS/sfs_base.h
daq_sst.o: ../DAQ_READER/daqReader.h
daq_sst.o: ../DAQ_READER/daqConfig.h
daq_sst.o: ../DAQ_READER/daq_dta.h
daq_sst.o: ../../../../StRoot/RTS/include/rts.h
daq_sst.o: ../DAQ_READER/daq_dta_structs.h
daq_sst.o: daq_sst.h
daq_sst.o: ../DAQ_READER/daq_det.h
sstPed.o: ../../../../StRoot/RTS/include/rtsLog.h
sstPed.o: ../../../../StRoot/RTS/include/daqModes.h
sstPed.o: ../../../../StRoot/RTS/include/rtsSystems.h
sstPed.o: ../DAQ_READER/daq_dta.h
sstPed.o: ../../../../StRoot/RTS/include/rts.h
sstPed.o: ../DAQ_READER/daq_dta_structs.h
sstPed.o: daq_sst.h
sstPed.o: ../DAQ_READER/daq_det.h
sstPed.o: sstPed.h
daq_sst.o: ../DAQ_READER/daq_det.h
sstPed.o: daq_sst.h
sstPed.o: ../DAQ_READER/daq_det.h
