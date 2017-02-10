################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def


# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -g -funroll-loops
DEFINES += -DRTS_LOG_COLORED

# If I'm in Zagreb...
ifeq ($(HOST_NAME),lvedaq.irb.lo)
	DEFINES +=  -DTEST_RDO
endif
ifeq ($(HOST_NAME),localhost.localdomain)
	DEFINES +=  -DTEST_RDO
endif
ifeq ($(HOST_NAME),ettie.star.bnl.gov)
	DEFINES +=  -DTEST_ETTIE
endif


#DEFINES += -DDO_DBG


OBJS := itpcCore.o daq_itpc.o

LIBS = libitpc.a


all: $(LIBS) $(BINS)

libs: $(LIBS)

libitpc.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_itpc.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_itpc.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_itpc.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_itpc.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_itpc.o: ../../../../StRoot/RTS/trg/include/trgDataDefs.h
daq_itpc.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_itpc.o: ../SFS/sfs_index.h
daq_itpc.o: ../SFS/fs_index.h
daq_itpc.o: ../SFS/sfs_base.h
daq_itpc.o: ../DAQ_READER/daqReader.h
daq_itpc.o: ../DAQ_READER/daqConfig.h
daq_itpc.o: ../DAQ_READER/daq_dta.h
daq_itpc.o: ../../../../StRoot/RTS/include/rts.h
daq_itpc.o: ../DAQ_READER/daq_dta_structs.h
daq_itpc.o: daq_itpc.h
daq_itpc.o: ../DAQ_READER/daq_det.h
itpcCore.o: itpc_maps.h
itpcCore.o: itpc_rowlen.h
itpcCore.o: itpc_padplane.h
itpcCore.o: itpcCore.h
daq_itpc.o: ../DAQ_READER/daq_det.h
