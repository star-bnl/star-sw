################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_ss2.o

LIBS = libss2.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libss2.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_ss2.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_ss2.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_ss2.o: ../SFS/sfs_index.h
daq_ss2.o: ../SFS/fs_index.h
daq_ss2.o: ../SFS/sfs_base.h
daq_ss2.o: ../DAQ_READER/daqReader.h
daq_ss2.o: ../DAQ_READER/daqConfig.h
daq_ss2.o: ../DAQ_READER/daq_dta.h
daq_ss2.o: ../../../../StRoot/RTS/include/rts.h
daq_ss2.o: ../DAQ_READER/daq_dta_structs.h
daq_ss2.o: daq_ss2.h
daq_ss2.o: ../DAQ_READER/daq_det.h
daq_ss2.o: ../DAQ_READER/daq_det.h
