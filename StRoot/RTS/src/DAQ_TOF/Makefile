################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_tof.o tof_reader.o

LIBS = libtof.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtof.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_tof.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_tof.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_tof.o: ../SFS/sfs_index.h
daq_tof.o: ../SFS/fs_index.h
daq_tof.o: ../SFS/sfs_base.h
daq_tof.o: ../DAQ_READER/daqReader.h
daq_tof.o: ../DAQ_READER/daqConfig.h
daq_tof.o: ../DAQ_READER/daq_dta.h
daq_tof.o: ../../../../StRoot/RTS/include/rts.h
daq_tof.o: ../DAQ_READER/daq_dta_structs.h
daq_tof.o: daq_tof.h
daq_tof.o: ../DAQ_READER/daq_det.h
tof_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
tof_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tof_reader.o: ../../../../StRoot/RTS/include/rts.h
tof_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
tof_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tof_reader.o: ../../trg/include/trgDataDefs_46.h
tof_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tof_reader.o: daq_tof.h
tof_reader.o: ../DAQ_READER/daq_det.h
tofReader.o: ../DAQ_READER/daqReader.h
tofReader.o: ../DAQ_READER/daqConfig.h
tofReader.o: ../DAQ_READER/daq_dta.h
tofReader.o: ../../../../StRoot/RTS/include/rts.h
tofReader.o: ../DAQ_READER/daq_dta_structs.h
tofReader.o: ../DAQ_READER/daq_det.h
tofReader.o: tofReader.h
tofReader.o: daq_tof.h
daq_tof.o: ../DAQ_READER/daq_det.h
tofReader.o: daq_tof.h
tofReader.o: ../DAQ_READER/daq_det.h
