################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_bsmd.o bsmd_reader.o bsmdPed.o

LIBS = libbsmd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libbsmd.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

bsmdPed.o: ../../../../StRoot/RTS/include/rtsLog.h
bsmdPed.o: ../../../../StRoot/RTS/include/daqModes.h
bsmdPed.o: bsmdPed.h
bsmd_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
bsmd_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
bsmd_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
bsmd_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
bsmd_reader.o: ../../trg/include/trgDataDefs_46.h
bsmd_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
bsmd_reader.o: ../../../../StRoot/RTS/include/rts.h
bsmd_reader.o: ../DAQ_READER/daq_det.h
bsmd_reader.o: daq_bsmd.h
bsmd_reader.o: ../DAQ_EMC/daq_emc.h
daq_bsmd.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_bsmd.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_bsmd.o: ../SFS/sfs_index.h
daq_bsmd.o: ../SFS/fs_index.h
daq_bsmd.o: ../SFS/sfs_base.h
daq_bsmd.o: ../DAQ_READER/daqReader.h
daq_bsmd.o: ../DAQ_READER/daqConfig.h
daq_bsmd.o: ../DAQ_READER/daq_dta.h
daq_bsmd.o: ../../../../StRoot/RTS/include/rts.h
daq_bsmd.o: ../DAQ_READER/daq_dta_structs.h
daq_bsmd.o: daq_bsmd.h
daq_bsmd.o: ../DAQ_READER/daq_det.h
daq_bsmd.o: ../DAQ_EMC/daq_emc.h
daq_bsmd.o: ../DAQ_READER/daq_det.h
daq_bsmd.o: ../DAQ_EMC/daq_emc.h
