################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_l3.o l3_reader.o

LIBS = libl3.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libl3.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_l3.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_l3.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_l3.o: ../SFS/sfs_index.h
daq_l3.o: ../SFS/fs_index.h
daq_l3.o: ../SFS/sfs_base.h
daq_l3.o: ../DAQ_READER/daqReader.h
daq_l3.o: ../DAQ_READER/daqConfig.h
daq_l3.o: ../DAQ_READER/daq_dta.h
daq_l3.o: ../../../../StRoot/RTS/include/rts.h
daq_l3.o: ../DAQ_READER/daq_dta_structs.h
daq_l3.o: daq_l3.h
daq_l3.o: ../DAQ_READER/daq_det.h
daq_l3.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_l3.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_l3.o: ../../trg/include/trgDataDefs_46.h
daq_l3.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
l3_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
l3_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
l3_reader.o: ../../../../StRoot/RTS/include/rts.h
l3_reader.o: daq_l3.h
l3_reader.o: ../DAQ_READER/daq_det.h
l3_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
l3_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
l3_reader.o: ../../trg/include/trgDataDefs_46.h
l3_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
l3Reader.o: ../DAQ_READER/daqReader.h
l3Reader.o: ../DAQ_READER/daqConfig.h
l3Reader.o: ../DAQ_READER/daq_dta.h
l3Reader.o: ../../../../StRoot/RTS/include/rts.h
l3Reader.o: ../DAQ_READER/daq_dta_structs.h
l3Reader.o: ../DAQ_READER/daq_det.h
l3Reader.o: l3Reader.h
l3Reader.o: daq_l3.h
l3Reader.o: ../../../../StRoot/RTS/include/daqFormats.h
l3Reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
l3Reader.o: ../../trg/include/trgDataDefs_46.h
l3Reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_l3.o: ../DAQ_READER/daq_det.h
daq_l3.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_l3.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_l3.o: ../../trg/include/trgDataDefs_46.h
daq_l3.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
l3Reader.o: daq_l3.h
l3Reader.o: ../DAQ_READER/daq_det.h
l3Reader.o: ../../../../StRoot/RTS/include/daqFormats.h
l3Reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
l3Reader.o: ../../trg/include/trgDataDefs_46.h
l3Reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
