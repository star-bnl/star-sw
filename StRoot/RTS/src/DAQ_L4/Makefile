################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_l4.o

LIBS = libl4.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libl4.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_l4.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_l4.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_l4.o: ../SFS/sfs_index.h
daq_l4.o: ../SFS/fs_index.h
daq_l4.o: ../SFS/sfs_base.h
daq_l4.o: ../DAQ_READER/daqReader.h
daq_l4.o: ../DAQ_READER/daqConfig.h
daq_l4.o: ../DAQ_READER/daq_dta.h
daq_l4.o: ../../../../StRoot/RTS/include/rts.h
daq_l4.o: ../DAQ_READER/daq_dta_structs.h
daq_l4.o: daq_l4.h
daq_l4.o: ../DAQ_READER/daq_det.h
daq_l4.o: ../DAQ_READER/daq_det.h
l4_gl3.o: daq_l4.h
l4_gl3.o: ../DAQ_READER/daq_det.h
