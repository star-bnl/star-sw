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


OBJS := itpcCore.o itpcPed.o itpcInterpreter.o itpcFCF.o daq_itpc.o

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
daq_itpc.o: ../../trg/include/trgDataDefs_46.h
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
daq_itpc.o: itpcCore.h
daq_itpc.o: ../DAQ_READER/daq_dta_structs.h
daq_itpc.o: itpcInterpreter.h
daq_itpc.o: ../../../../StRoot/RTS/include/I386/atomic.h
daq_itpc.o: itpcPed.h
daq_itpc.o: itpcFCF.h
daq_itpc.o: ../DAQ_ITPC/itpcPed.h
itpcCore.o: ../../../../StRoot/RTS/include/rtsLog.h
itpcCore.o: itpc_maps.h
itpcCore.o: itpc_rowlen.h
itpcCore.o: itpc_padplane.h
itpcCore.o: itpcCore.h
itpcCore.o: ../DAQ_READER/daq_dta_structs.h
itpcFCF.o: ../../../../StRoot/RTS/include/rtsLog.h
itpcFCF.o: ../../../../StRoot/RTS/include/rtsSystems.h
itpcFCF.o: ../DAQ_READER/daq_dta.h
itpcFCF.o: ../../../../StRoot/RTS/include/rts.h
itpcFCF.o: ../DAQ_READER/daq_dta_structs.h
itpcFCF.o: ../DAQ_TPX/daq_tpx.h
itpcFCF.o: ../DAQ_READER/daq_det.h
itpcFCF.o: ../DAQ_TPX/tpxFCF_flags.h
itpcFCF.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
itpcFCF.o: ../DAQ_ITPC/daq_itpc.h
itpcFCF.o: ../DAQ_ITPC/itpcCore.h
itpcFCF.o: ../DAQ_READER/daq_dta_structs.h
itpcFCF.o: ../DAQ_ITPC/itpcPed.h
itpcFCF.o: ../DAQ_ITPC/itpcInterpreter.h
itpcFCF.o: ../../../../StRoot/RTS/include/I386/atomic.h
itpcFCF.o: ../DAQ_ITPC/itpc_rowlen.h
itpcFCF.o: ../DAQ_ITPC/itpcFCF.h
itpcInterpreter.o: ../../../../StRoot/RTS/include/rtsLog.h
itpcInterpreter.o: ../DAQ_READER/daq_dta.h
itpcInterpreter.o: ../../../../StRoot/RTS/include/rts.h
itpcInterpreter.o: ../DAQ_READER/daq_dta_structs.h
itpcInterpreter.o: ../../../../StRoot/RTS/include/I386/atomic.h
itpcInterpreter.o: itpcInterpreter.h
itpcInterpreter.o: itpcPed.h
itpcPed.o: ../../../../StRoot/RTS/include/rtsLog.h
itpcPed.o: ../DAQ_ITPC/itpcCore.h
itpcPed.o: ../DAQ_READER/daq_dta_structs.h
itpcPed.o: itpcPed.h
daq_itpc.o: ../DAQ_READER/daq_det.h
itpcCore.o: ../DAQ_READER/daq_dta_structs.h
itpcFCF.o: ../DAQ_ITPC/itpcPed.h
itpcInterpreter.o: ../../../../StRoot/RTS/include/I386/atomic.h
