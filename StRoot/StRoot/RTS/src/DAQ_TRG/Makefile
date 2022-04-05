################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_trg.o trg_reader.o trgReader10.o trgReader12.o trgReader20.o trgReader21.o trgReader22.o trgReader30.o trgReader32.o

LIBS = libtrg.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtrg.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_trg.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_trg.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_trg.o: ../SFS/sfs_index.h
daq_trg.o: ../SFS/fs_index.h
daq_trg.o: ../SFS/sfs_base.h
daq_trg.o: ../DAQ_READER/daqReader.h
daq_trg.o: ../DAQ_READER/daqConfig.h
daq_trg.o: ../DAQ_READER/daq_dta.h
daq_trg.o: ../../../../StRoot/RTS/include/rts.h
daq_trg.o: ../DAQ_READER/daq_dta_structs.h
daq_trg.o: daq_trg.h
daq_trg.o: ../DAQ_READER/daq_det.h
trgReader10.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader10.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader10.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader10.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader10.o: ../../trg/include/trgStructures_32.h
trgReader10.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader10.o: ../../../../StRoot/RTS/include/rts.h
trgReader10.o: daq_trg.h
trgReader10.o: ../DAQ_READER/daq_det.h
trgReader12.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader12.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader12.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader12.o: ../../trg/include/trgStructures_32.h
trgReader12.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader12.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader12.o: daq_trg.h
trgReader12.o: ../DAQ_READER/daq_det.h
trgReader20.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader20.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader20.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader20.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader20.o: ../../trg/include/trgStructures_32.h
trgReader20.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader20.o: ../../../../StRoot/RTS/include/rts.h
trgReader20.o: daq_trg.h
trgReader20.o: ../DAQ_READER/daq_det.h
trgReader21.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader21.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader21.o: ../../../../StRoot/RTS/include/rts.h
trgReader21.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader21.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader21.o: ../../trg/include/trgStructures_32.h
trgReader21.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader21.o: daq_trg.h
trgReader21.o: ../DAQ_READER/daq_det.h
trgReader22.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader22.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader22.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader22.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader22.o: ../../trg/include/trgStructures_32.h
trgReader22.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader22.o: ../../../../StRoot/RTS/include/rts.h
trgReader22.o: daq_trg.h
trgReader22.o: ../DAQ_READER/daq_det.h
trgReader30.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader30.o: ../../../../StRoot/RTS/include/rts.h
trgReader30.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader30.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader30.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader30.o: ../../trg/include/trgStructures_32.h
trgReader30.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader30.o: daq_trg.h
trgReader30.o: ../DAQ_READER/daq_det.h
trgReader32.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader32.o: ../../../../StRoot/RTS/include/rts.h
trgReader32.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader32.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader32.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader32.o: ../../trg/include/trgStructures_32.h
trgReader32.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader32.o: daq_trg.h
trgReader32.o: ../DAQ_READER/daq_det.h
trg_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
trg_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
trg_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trg_reader.o: ../../trg/include/trgStructures_32.h
trg_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trg_reader.o: ../../../../StRoot/RTS/include/rts.h
trg_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
trg_reader.o: daq_trg.h
trg_reader.o: ../DAQ_READER/daq_det.h
trgReader.o: ../DAQ_READER/daqReader.h
trgReader.o: ../DAQ_READER/daqConfig.h
trgReader.o: ../DAQ_READER/daq_dta.h
trgReader.o: ../../../../StRoot/RTS/include/rts.h
trgReader.o: ../DAQ_READER/daq_dta_structs.h
trgReader.o: ../DAQ_READER/daq_det.h
trgReader.o: trgReader.h
trgReader.o: daq_trg.h
daq_trg.o: ../DAQ_READER/daq_det.h
trgReader.o: daq_trg.h
trgReader.o: ../DAQ_READER/daq_det.h
