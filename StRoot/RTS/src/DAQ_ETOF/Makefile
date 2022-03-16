################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_etof.o

LIBS = libetof.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libetof.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_etof.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_etof.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_etof.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_etof.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_etof.o: ../../trg/include/trgDataDefs_46.h
daq_etof.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_etof.o: ../SFS/sfs_index.h
daq_etof.o: ../SFS/fs_index.h
daq_etof.o: ../SFS/sfs_base.h
daq_etof.o: ../DAQ_READER/daqReader.h
daq_etof.o: ../DAQ_READER/daqConfig.h
daq_etof.o: ../DAQ_READER/daq_dta.h
daq_etof.o: ../../../../StRoot/RTS/include/rts.h
daq_etof.o: ../DAQ_READER/daq_dta_structs.h
daq_etof.o: daq_etof.h
daq_etof.o: ../DAQ_READER/daq_det.h
daq_etof.o: ../DAQ_READER/daq_det.h
