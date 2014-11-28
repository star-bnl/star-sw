################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_pxl.o

LIBS = libpxl.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libpxl.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_pxl.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_pxl.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_pxl.o: ../SFS/sfs_index.h
daq_pxl.o: ../SFS/fs_index.h
daq_pxl.o: ../SFS/sfs_base.h
daq_pxl.o: ../DAQ_READER/daqReader.h
daq_pxl.o: ../DAQ_READER/daqConfig.h
daq_pxl.o: ../DAQ_READER/daq_dta.h
daq_pxl.o: ../../../../StRoot/RTS/include/rts.h
daq_pxl.o: ../DAQ_READER/daq_dta_structs.h
daq_pxl.o: daq_pxl.h
daq_pxl.o: ../DAQ_READER/daq_det.h
daq_pxl.o: ../DAQ_READER/daq_det.h
