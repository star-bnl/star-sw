################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_gmt.o

LIBS = libgmt.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libgmt.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_gmt.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_gmt.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_gmt.o: ../SFS/sfs_index.h
daq_gmt.o: ../SFS/fs_index.h
daq_gmt.o: ../SFS/sfs_base.h
daq_gmt.o: ../DAQ_READER/daqReader.h
daq_gmt.o: ../DAQ_READER/daqConfig.h
daq_gmt.o: ../DAQ_READER/daq_dta.h
daq_gmt.o: ../../../../StRoot/RTS/include/rts.h
daq_gmt.o: ../DAQ_READER/daq_dta_structs.h
daq_gmt.o: daq_gmt.h
daq_gmt.o: ../DAQ_READER/daq_det.h
daq_gmt.o: ../DAQ_READER/daq_det.h
