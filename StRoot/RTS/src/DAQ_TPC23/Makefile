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


OBJS := tpc23_base.o itpc23.o tpx23.o

LIBS = libtpc23.a


all: $(LIBS) $(BINS)

libs: $(LIBS)

libtpc23.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

itpc23.o: ../../../../StRoot/RTS/include/rtsLog.h
itpc23.o: ../../../../StRoot/RTS/include/rts.h
itpc23.o: ../../../../StRoot/RTS/include/rtsSystems.h
itpc23.o: ../DAQ_READER/daq_dta.h
itpc23.o: ../DAQ_READER/daq_dta_structs.h
itpc23.o: ../DAQ_ITPC/itpcCore.h
itpc23.o: ../DAQ_READER/daq_dta_structs.h
itpc23.o: ../DAQ_ITPC/itpc_rowlen.h
itpc23.o: ../DAQ_ITPC/itpcPed.h
itpc23.o: itpc23.h
itpc23.o: tpc23_base.h
tpc23_base.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc23_base.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpc23_base.o: ../DAQ_READER/daqReader.h
tpc23_base.o: ../DAQ_READER/daqConfig.h
tpc23_base.o: ../DAQ_READER/daq_dta.h
tpc23_base.o: ../../../../StRoot/RTS/include/rts.h
tpc23_base.o: ../DAQ_READER/daq_dta_structs.h
tpc23_base.o: ../DAQ_READER/daq_det.h
tpc23_base.o: ../DAQ_TPX/daq_tpx.h
tpc23_base.o: ../DAQ_TPX/tpxFCF_flags.h
tpc23_base.o: ../DAQ_TPX/tpxPed.h
tpc23_base.o: ../DAQ_TPX/tpxCore.h
tpc23_base.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpc23_base.o: ../DAQ_ITPC/daq_itpc.h
tpc23_base.o: ../DAQ_ITPC/itpcFCF.h
tpc23_base.o: ../DAQ_ITPC/itpcPed.h
tpc23_base.o: tpc23_base.h
tpx23.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx23.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpx23.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpx23.o: ../../../../StRoot/RTS/include/DAQ1000/rdo_cmds.h
tpx23.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpx23.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpx23.o: ../DAQ_TPX/tpxCore.h
tpx23.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpx23.o: ../DAQ_TPX/tpxPed.h
tpx23.o: ../DAQ_TPX/tpxCore.h
tpx23.o: ../DAQ_TPX/tpxGain.h
tpx23.o: ../DAQ_ITPC/itpcPed.h
tpx23.o: tpx23.h
tpx23.o: ../DAQ_READER/daq_dta.h
tpx23.o: ../../../../StRoot/RTS/include/rts.h
tpx23.o: ../DAQ_READER/daq_dta_structs.h
tpx23.o: tpc23_base.h
itpc23.o: tpc23_base.h
tpx23.o: ../DAQ_READER/daq_dta.h
tpx23.o: ../../../../StRoot/RTS/include/rts.h
tpx23.o: ../DAQ_READER/daq_dta_structs.h
tpx23.o: tpc23_base.h
