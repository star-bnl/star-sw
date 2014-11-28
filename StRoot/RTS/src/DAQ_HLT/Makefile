################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_hlt.o

LIBS = libhlt.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libhlt.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_hlt.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_hlt.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_hlt.o: ../SFS/sfs_index.h
daq_hlt.o: ../SFS/fs_index.h
daq_hlt.o: ../SFS/sfs_base.h
daq_hlt.o: ../DAQ_READER/daqReader.h
daq_hlt.o: ../DAQ_READER/daqConfig.h
daq_hlt.o: ../DAQ_READER/daq_dta.h
daq_hlt.o: ../../../../StRoot/RTS/include/rts.h
daq_hlt.o: ../DAQ_READER/daq_dta_structs.h
daq_hlt.o: daq_hlt.h
daq_hlt.o: ../DAQ_READER/daq_det.h
daq_hlt.o: ../DAQ_READER/daq_det.h
hlt_gl3.o: daq_hlt.h
hlt_gl3.o: ../DAQ_READER/daq_det.h
