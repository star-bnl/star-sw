################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_fgt.o fgtPed.o

LIBS = libfgt.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libfgt.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_fgt.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_fgt.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_fgt.o: ../SFS/sfs_index.h
daq_fgt.o: ../SFS/fs_index.h
daq_fgt.o: ../SFS/sfs_base.h
daq_fgt.o: ../DAQ_READER/daqReader.h
daq_fgt.o: ../DAQ_READER/daqConfig.h
daq_fgt.o: ../DAQ_READER/daq_dta.h
daq_fgt.o: ../../../../StRoot/RTS/include/rts.h
daq_fgt.o: ../DAQ_READER/daq_dta_structs.h
daq_fgt.o: daq_fgt.h
daq_fgt.o: ../DAQ_READER/daq_det.h
fgtPed.o: ../../../../StRoot/RTS/include/rtsLog.h
fgtPed.o: ../../../../StRoot/RTS/include/daqModes.h
fgtPed.o: ../../../../StRoot/RTS/include/rtsSystems.h
fgtPed.o: ../DAQ_READER/daq_dta.h
fgtPed.o: ../../../../StRoot/RTS/include/rts.h
fgtPed.o: ../DAQ_READER/daq_dta_structs.h
fgtPed.o: fgtPed.h
fgtPed.o: daq_fgt.h
fgtPed.o: ../DAQ_READER/daq_det.h
daq_fgt.o: ../DAQ_READER/daq_det.h
fgtPed.o: daq_fgt.h
fgtPed.o: ../DAQ_READER/daq_det.h
