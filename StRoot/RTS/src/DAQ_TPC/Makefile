################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_tpc.o tpc_reader.o

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
daq_tpc.o: ../RTS_READER/rts_reader.h
daq_tpc.o: ../RTS_READER/daq_dta.h
daq_tpc.o: ../../../../StRoot/RTS/include/rts.h
daq_tpc.o: ../RTS_READER/daq_dta_structs.h
daq_tpc.o: daq_tpc.h
daq_tpc.o: ../RTS_READER/daq_det.h
tpc_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
tpc_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tpc_reader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
tpc_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tpc_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpc_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc_reader.o: ../../../../StRoot/RTS/include/TPC/offsets.h
tpc_reader.o: ../../../../StRoot/RTS/include/TPC/trans_table.hh
tpc_reader.o: ../../../../StRoot/RTS/include/fcfClass.hh
tpc_reader.o: ../RTS_READER/daq_dta.h
tpc_reader.o: ../../../../StRoot/RTS/include/rts.h
tpc_reader.o: ../RTS_READER/daq_dta_structs.h
tpc_reader.o: ../DAQ_LEGACY/daq_legacy.h
daq_tpc.o: ../RTS_READER/daq_det.h
