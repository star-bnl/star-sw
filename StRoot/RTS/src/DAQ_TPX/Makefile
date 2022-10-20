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



OBJS := tpxCore.o tpxPed.o tpxGain.o tpxFCF.o tpxFCF_2D.o tpxStat.o daq_tpx.o

LIBS = libtpx.a
#BINS = sim_tester

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtpx.a : $(OBJS)
	@$(RTS_AR)

sim_tester: sim_tester.o ../RTS_READER/librts_reader.a libtpx.a  ../DAQ_ESMD/libesmd.a ../DAQ_TPC/libtpc.a ../DAQ_TOF/libtof.a ../DAQ_PP2PP/libpp2pp.a ../DAQ_LEGACY/liblegacy.a ../SFS/libsfs.a ../LOG/librtslog.a


# DO NOT DELETE

daq_tpx.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_tpx.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_tpx.o: ../SFS/sfs_index.h
daq_tpx.o: ../SFS/fs_index.h
daq_tpx.o: ../SFS/sfs_base.h
daq_tpx.o: ../DAQ_READER/daqReader.h
daq_tpx.o: ../DAQ_READER/daqConfig.h
daq_tpx.o: ../DAQ_READER/daq_dta.h
daq_tpx.o: ../../../../StRoot/RTS/include/rts.h
daq_tpx.o: ../DAQ_READER/daq_dta_structs.h
daq_tpx.o: ../DAQ_TPC/daq_tpc.h
daq_tpx.o: ../DAQ_READER/daq_det.h
daq_tpx.o: ../../../../StRoot/RTS/include/TPC/trans_table.hh
daq_tpx.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
daq_tpx.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
daq_tpx.o: daq_tpx.h
daq_tpx.o: tpxCore.h
daq_tpx.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
daq_tpx.o: tpxGain.h
daq_tpx.o: tpxPed.h
daq_tpx.o: tpxFCF.h
daq_tpx.o: tpxFCF_flags.h
daq_tpx.o: tpxFCF_2D.h
daq_tpx.o: ../DAQ_TPX/tpxFCF.h
daq_tpx.o: tpxStat.h
tpxCore.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxCore.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpxCore.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpxCore.o: ../../../../StRoot/RTS/include/TPC/fee_readout.h
tpxCore.o: tpxCore.h
tpxCore.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxCore.o: tpx_fee_position.h
tpxFCF_2D.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxFCF_2D.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxFCF_2D.o: ../DAQ_TPX/tpxCore.h
tpxFCF_2D.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxFCF_2D.o: ../DAQ_TPX/tpxFCF_flags.h
tpxFCF_2D.o: ../DAQ_TPX/tpxGain.h
tpxFCF_2D.o: tpxCore.h
tpxFCF_2D.o: tpxFCF_2D.h
tpxFCF_2D.o: ../DAQ_TPX/tpxFCF.h
tpxFCF_2D.o: ../DAQ_READER/daq_dta_structs.h
tpxFCF.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxFCF.o: ../../../../StRoot/RTS/include/I386/i386Lib.h
tpxFCF.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxFCF.o: tpxGain.h
tpxFCF.o: tpxCore.h
tpxFCF.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxFCF.o: tpxFCF.h
tpxFCF.o: tpxFCF_flags.h
tpxFCF.o: ../DAQ_READER/daq_dta_structs.h
tpxGain.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxGain.o: ../../../../StRoot/RTS/include/daqModes.h
tpxGain.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxGain.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpxGain.o: tpxCore.h
tpxGain.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxGain.o: tpxGain.h
tpxPed.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxPed.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxPed.o: ../../../../StRoot/RTS/include/daqModes.h
tpxPed.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpxPed.o: ../SFS/sfs_index.h
tpxPed.o: ../SFS/fs_index.h
tpxPed.o: ../SFS/sfs_base.h
tpxPed.o: tpxCore.h
tpxPed.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxPed.o: tpxPed.h
tpxPed.o: tpxGain.h
tpxStat.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxStat.o: ../../../../StRoot/RTS/include/daqModes.h
tpxStat.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpxStat.o: tpxCore.h
tpxStat.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxStat.o: tpxStat.h
daq_tpx.o: ../DAQ_READER/daq_det.h
tpxCore.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxCore.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxFCF_2D.o: ../DAQ_TPX/tpxFCF.h
tpxFCF.o: tpxFCF_flags.h
tpxGain.o: tpxCore.h
tpxGain.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxGain.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxPed.o: tpxCore.h
tpxPed.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxPed.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
