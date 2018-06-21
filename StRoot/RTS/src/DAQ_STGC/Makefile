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



OBJS := stgcPed.o daq_stgc.o

LIBS = libstgc.a


all: $(LIBS) $(BINS)

libs: $(LIBS)

libstgc.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_stgc.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_stgc.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_stgc.o: ../SFS/sfs_index.h
daq_stgc.o: ../SFS/fs_index.h
daq_stgc.o: ../SFS/sfs_base.h
daq_stgc.o: ../DAQ_READER/daqReader.h
daq_stgc.o: ../DAQ_READER/daqConfig.h
daq_stgc.o: ../DAQ_READER/daq_dta.h
daq_stgc.o: ../../../../StRoot/RTS/include/rts.h
daq_stgc.o: ../DAQ_READER/daq_dta_structs.h
daq_stgc.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
daq_stgc.o: daq_stgc.h
daq_stgc.o: ../DAQ_READER/daq_det.h
daq_stgc.o: ../DAQ_TPX/tpxCore.h
daq_stgc.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
stgcPed.o: ../../../../StRoot/RTS/include/rtsLog.h
stgcPed.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
stgcPed.o: ../../../../StRoot/RTS/include/daqModes.h
stgcPed.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
stgcPed.o: ../SFS/sfs_index.h
stgcPed.o: ../SFS/fs_index.h
stgcPed.o: ../SFS/sfs_base.h
stgcPed.o: stgcPed.h
stgcPed.o: ../DAQ_TPX/tpxCore.h
stgcPed.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
daq_stgc.o: ../DAQ_READER/daq_det.h
stgcPed.o: ../DAQ_TPX/tpxCore.h
stgcPed.o: ../../../../StRoot/RTS/include/rtsLog.h
stgcPed.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
