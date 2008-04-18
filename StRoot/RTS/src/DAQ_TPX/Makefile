################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED

# If I'm in Zagreb...
ifeq ($(HOST_NAME),lvedaq.irb.lo)
	DEFINES +=  -DTEST_RDO
endif
ifeq ($(HOST_NAME),localhost.localdomain)
	DEFINES +=  -DTEST_RDO
endif
ifeq ($(HOST_NAME),lx2.star.bnl.gov)
	DEFINES +=  -DTEST_RDO
endif



OBJS := tpxCore.o tpxPed.o tpxGain.o tpxFCF.o tpxStat.o daq_tpx.o

LIBS = libtpx.a
#BINS = sim_tester

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtpx.a : $(OBJS)
	@$(RTS_AR)

#sim_tester: sim_tester.o ../RTS_READER/librts_reader.a libtpx.a ../DAQ_TOF/libtof.a ../DAQ_PP2PP/libpp2pp.a ../SFS/libsfs.a ../LOG/librtslog.a


# DO NOT DELETE

daq_tpx.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_tpx.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_tpx.o: ../SFS/sfs_index.h
daq_tpx.o: ../SFS/fs_index.h
daq_tpx.o: ../SFS/sfs_base.h
daq_tpx.o: ../RTS_READER/rts_reader.h
daq_tpx.o: ../RTS_READER/daq_dta.h
daq_tpx.o: ../../../../StRoot/RTS/include/rts.h
daq_tpx.o: ../RTS_READER/daq_dta_structs.h
daq_tpx.o: daq_tpx.h
daq_tpx.o: ../RTS_READER/daq_det.h
daq_tpx.o: ../DAQ_TPX/tpxCore.h
daq_tpx.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
daq_tpx.o: ../DAQ_TPX/tpxGain.h
daq_tpx.o: tpxCore.h
daq_tpx.o: ../DAQ_TPX/tpxPed.h
daq_tpx.o: ../DAQ_TPX/tpxFCF.h
daq_tpx.o: ../DAQ_TPX/tpxStat.h
daq_tpx.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpxCore.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxCore.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpxCore.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpxCore.o: ../../../../StRoot/RTS/include/TPC/fee_readout.h
tpxCore.o: ../DAQ_TPX/tpxCore.h
tpxCore.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxFCF.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxFCF.o: ../../../../StRoot/RTS/include/I386/i386Lib.h
tpxFCF.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxFCF.o: ../DAQ_TPX/tpxGain.h
tpxFCF.o: tpxCore.h
tpxFCF.o: ../DAQ_TPX/tpxFCF.h
tpxFCF.o: ../RTS_READER/daq_dta_structs.h
tpxGain.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxGain.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxGain.o: ../../../../StRoot/RTS/include/TPX/tpx_altro_to_pad.h
tpxGain.o: ../DAQ_TPX/tpxCore.h
tpxGain.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxGain.o: ../DAQ_TPX/tpxGain.h
tpxGain.o: tpxCore.h
tpx_ped.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx_ped.o: ../DAQ_TPX/tpxCore.h
tpx_ped.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpx_ped.o: ../RTS_READER/daq_algo.h
tpx_ped.o: ../RTS_READER/daq_dta.h
tpx_ped.o: ../../../../StRoot/RTS/include/rts.h
tpx_ped.o: ../RTS_READER/daq_dta_structs.h
tpxPed.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxPed.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpxPed.o: ../DAQ_TPX/tpxCore.h
tpxPed.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxPed.o: ../DAQ_TPX/tpxPed.h
tpxPed.o: tpxCore.h
tpxStat.o: ../../../../StRoot/RTS/include/rtsLog.h
tpxStat.o: ../../../../StRoot/RTS/include/daqModes.h
tpxStat.o: ../DAQ_TPX/tpxCore.h
tpxStat.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxStat.o: ../DAQ_TPX/tpxStat.h
sim_tester.o: ../../../../StRoot/RTS/include/rtsLog.h
sim_tester.o: ../RTS_READER/rts_reader.h
sim_tester.o: ../RTS_READER/daq_dta.h
sim_tester.o: ../../../../StRoot/RTS/include/rts.h
sim_tester.o: ../RTS_READER/daq_dta_structs.h
sim_tester.o: ../DAQ_TPX/daq_tpx.h
sim_tester.o: ../RTS_READER/daq_det.h
daq_tpx.o: ../RTS_READER/daq_det.h
tpxCore.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpxGain.o: tpxCore.h
tpxPed.o: tpxCore.h
