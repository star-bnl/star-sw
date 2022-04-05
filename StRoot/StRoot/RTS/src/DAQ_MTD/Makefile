################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_mtd.o

LIBS = libmtd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libmtd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_mtd.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_mtd.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_mtd.o: ../SFS/sfs_index.h
daq_mtd.o: ../SFS/fs_index.h
daq_mtd.o: ../SFS/sfs_base.h
daq_mtd.o: ../DAQ_READER/daqReader.h
daq_mtd.o: ../DAQ_READER/daqConfig.h
daq_mtd.o: ../DAQ_READER/daq_dta.h
daq_mtd.o: ../../../../StRoot/RTS/include/rts.h
daq_mtd.o: ../DAQ_READER/daq_dta_structs.h
daq_mtd.o: daq_mtd.h
daq_mtd.o: ../DAQ_READER/daq_det.h
daq_mtd.o: ../DAQ_READER/daq_det.h
