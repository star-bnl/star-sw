################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_pmd.o pmd_reader.o

LIBS = libpmd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libpmd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_pmd.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_pmd.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_pmd.o: ../SFS/sfs_index.h
daq_pmd.o: ../SFS/fs_index.h
daq_pmd.o: ../SFS/sfs_base.h
daq_pmd.o: ../DAQ_READER/daqReader.h
daq_pmd.o: ../DAQ_READER/daqConfig.h
daq_pmd.o: ../DAQ_READER/daq_dta.h
daq_pmd.o: ../../../../StRoot/RTS/include/rts.h
daq_pmd.o: ../DAQ_READER/daq_dta_structs.h
daq_pmd.o: daq_pmd.h
daq_pmd.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_pmd.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_pmd.o: ../../trg/include/trgDataDefs_46.h
daq_pmd.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_pmd.o: ../DAQ_READER/daq_det.h
pmd_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
pmd_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
pmd_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
pmd_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pmd_reader.o: ../../trg/include/trgDataDefs_46.h
pmd_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pmd_reader.o: ../../../../StRoot/RTS/include/rts.h
pmd_reader.o: daq_pmd.h
pmd_reader.o: ../DAQ_READER/daq_det.h
pmdReader.o: ../DAQ_READER/daqReader.h
pmdReader.o: ../DAQ_READER/daqConfig.h
pmdReader.o: ../DAQ_READER/daq_dta.h
pmdReader.o: ../../../../StRoot/RTS/include/rts.h
pmdReader.o: ../DAQ_READER/daq_dta_structs.h
pmdReader.o: ../DAQ_READER/daq_det.h
pmdReader.o: pmdReader.h
pmdReader.o: daq_pmd.h
pmdReader.o: ../../../../StRoot/RTS/include/daqFormats.h
pmdReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pmdReader.o: ../../trg/include/trgDataDefs_46.h
pmdReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_pmd.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_pmd.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_pmd.o: ../../trg/include/trgDataDefs_46.h
daq_pmd.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_pmd.o: ../DAQ_READER/daq_det.h
pmd.o: daq_pmd.h
pmd.o: ../../../../StRoot/RTS/include/daqFormats.h
pmd.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pmd.o: ../../trg/include/trgDataDefs_46.h
pmd.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pmd.o: ../DAQ_READER/daq_det.h
pmdReader.o: daq_pmd.h
pmdReader.o: ../../../../StRoot/RTS/include/daqFormats.h
pmdReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pmdReader.o: ../../trg/include/trgDataDefs_46.h
pmdReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pmdReader.o: ../DAQ_READER/daq_det.h
