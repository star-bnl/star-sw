################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_tpc.o tpc_reader.o tpc_FCFReader.o

LIBS = libtpc.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtpc.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_tpc.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_tpc.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_tpc.o: ../SFS/sfs_index.h
daq_tpc.o: ../SFS/fs_index.h
daq_tpc.o: ../SFS/sfs_base.h
daq_tpc.o: ../DAQ_READER/daqReader.h
daq_tpc.o: ../DAQ_READER/daqConfig.h
daq_tpc.o: ../DAQ_READER/daq_dta.h
daq_tpc.o: ../../../../StRoot/RTS/include/rts.h
daq_tpc.o: ../DAQ_READER/daq_dta_structs.h
daq_tpc.o: daq_tpc.h
daq_tpc.o: ../DAQ_READER/daq_det.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/daqFormats.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tpc_FCFReader.o: ../../trg/include/trgDataDefs_46.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/fcfClass.hh
tpc_FCFReader.o: ../../../../StRoot/RTS/include/TPC/padfinder.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpc_FCFReader.o: ../../../../StRoot/RTS/include/adcLogTable.h
tpc_FCFReader.o: daq_tpc.h
tpc_FCFReader.o: ../DAQ_READER/daq_det.h
tpc_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
tpc_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tpc_reader.o: ../../trg/include/trgDataDefs_46.h
tpc_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tpc_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpc_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc_reader.o: ../../../../StRoot/RTS/include/rts.h
tpc_reader.o: ../../../../StRoot/RTS/include/TPC/offsets.h
tpc_reader.o: ../../../../StRoot/RTS/include/TPC/trans_table.hh
tpc_reader.o: daq_tpc.h
tpc_reader.o: ../DAQ_READER/daq_det.h
tpc_reader.o: ../../../../StRoot/RTS/include/fcfClass.hh
tpcReader.o: ../DAQ_READER/daqReader.h
tpcReader.o: ../DAQ_READER/daqConfig.h
tpcReader.o: ../DAQ_READER/daq_dta.h
tpcReader.o: ../../../../StRoot/RTS/include/rts.h
tpcReader.o: ../DAQ_READER/daq_dta_structs.h
tpcReader.o: ../DAQ_READER/daq_det.h
tpcReader.o: tpcReader.h
tpcReader.o: daq_tpc.h
daq_tpc.o: ../DAQ_READER/daq_det.h
tpcReader.o: daq_tpc.h
tpcReader.o: ../DAQ_READER/daq_det.h
