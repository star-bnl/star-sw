################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_emc.o emc_reader.o emc_single_reader.o

LIBS = libemc.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libemc.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_emc.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_emc.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_emc.o: ../DAQ_READER/daqReader.h
daq_emc.o: ../DAQ_READER/daqConfig.h
daq_emc.o: ../DAQ_READER/daq_dta.h
daq_emc.o: ../../../../StRoot/RTS/include/rts.h
daq_emc.o: ../DAQ_READER/daq_dta_structs.h
daq_emc.o: daq_emc.h
daq_emc.o: ../DAQ_READER/daq_det.h
daq_emc.o: ../DAQ_BTOW/daq_btow.h
daq_emc.o: ../DAQ_EMC/daq_emc.h
daq_emc.o: ../DAQ_ETOW/daq_etow.h
daq_emc.o: ../DAQ_ESMD/daq_esmd.h
daq_emc.o: ../DAQ_BSMD/daq_bsmd.h
emc_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
emc_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
emc_reader.o: ../../../../StRoot/RTS/include/rts.h
emc_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
emc_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
emc_reader.o: ../../trg/include/trgStructures_32.h
emc_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
emc_reader.o: ../../trg/include/trgStructures.h
emc_reader.o: daq_emc.h
emc_reader.o: ../DAQ_READER/daq_det.h
emcReader.o: ../DAQ_READER/daqReader.h
emcReader.o: ../DAQ_READER/daqConfig.h
emcReader.o: ../DAQ_READER/daq_dta.h
emcReader.o: ../../../../StRoot/RTS/include/rts.h
emcReader.o: ../DAQ_READER/daq_dta_structs.h
emcReader.o: ../DAQ_READER/daq_det.h
emcReader.o: emcReader.h
emcReader.o: daq_emc.h
emc_single_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
emc_single_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
emc_single_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
emc_single_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
emc_single_reader.o: ../../trg/include/trgStructures_32.h
emc_single_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
emc_single_reader.o: ../../../../StRoot/RTS/include/rts.h
emc_single_reader.o: ../DAQ_READER/daq_det.h
daq_emc.o: ../DAQ_READER/daq_det.h
emcReader.o: daq_emc.h
emcReader.o: ../DAQ_READER/daq_det.h
