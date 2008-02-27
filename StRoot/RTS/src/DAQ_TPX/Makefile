################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED


ifdef IS_STROOT
	DEFINES += -DRTS_DISABLE_LOG
else
endif


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



OBJS := tpxCore.o tpxPed.o tpxGain.o tpxFCF.o tpxStat.o daq_tpx.o tpx_ped.o

LIBS = libtpx.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtpx.a : $(OBJS)
	@$(RTS_AR)


# DO NOT DELETE

daq_tpx.o: daq_tpx.h
daq_tpx.o: ../RTS_READER/rts_reader.h
daq_tpx.o: ../../include/rts.h
daq_tpx.o: ../../include/rtsLog.h
daq_tpx.o: ../../include/rtsSystems.h
daq_tpx.o: ../RTS_READER/daq_det.h
daq_tpx.o: ../RTS_READER/daq_dta.h
daq_tpx.o: ../SFS/sfs_index.h
daq_tpx.o: ../SFS/fs_index.h
daq_tpx.o: ../DAQ_TPX/tpxCore.h
daq_tpx.o: ../../include/TPX/tpx_rdo.h
daq_tpx.o: ../DAQ_TPX/tpxGain.h
daq_tpx.o: tpxCore.h
daq_tpx.o: ../DAQ_TPX/tpxPed.h
daq_tpx.o: ../DAQ_TPX/tpxFCF.h
daq_tpx.o: ../DAQ_TPX/tpxStat.h
daq_tpx.o: ../../include/DAQ1000/ddl_struct.h
tpxCore.o: ../../include/rtsLog.h
tpxCore.o: ../../include/TPX/tpx_altro_to_pad.h
tpxCore.o: ../../include/DAQ1000/ddl_struct.h
tpxCore.o: ../../include/TPC/fee_readout.h
tpxCore.o: ../DAQ_TPX/tpxCore.h
tpxCore.o: ../../include/TPX/tpx_rdo.h
tpxFCF.o: ../../include/rtsLog.h
tpxFCF.o: ../../include/I386/i386Lib.h
tpxFCF.o: ../../include/TPC/rowlen.h
tpxFCF.o: ../DAQ_TPX/tpxGain.h
tpxFCF.o: tpxCore.h
tpxFCF.o: ../DAQ_TPX/tpxFCF.h
tpxGain.o: ../../include/rtsLog.h
tpxGain.o: ../../include/TPC/rowlen.h
tpxGain.o: ../../include/TPX/tpx_altro_to_pad.h
tpxGain.o: ../DAQ_TPX/tpxCore.h
tpxGain.o: ../../include/TPX/tpx_rdo.h
tpxGain.o: ../DAQ_TPX/tpxGain.h
tpxGain.o: tpxCore.h
tpx_ped.o: ../../include/rtsLog.h
tpx_ped.o: ../DAQ_TPX/tpxCore.h
tpx_ped.o: ../../include/TPX/tpx_rdo.h
tpx_ped.o: ../RTS_READER/daq_algo.h
tpx_ped.o: ../RTS_READER/daq_dta.h
tpx_ped.o: ../../include/rts.h
tpx_ped.o: ../RTS_READER/rts_reader.h
tpx_ped.o: ../../include/rtsSystems.h
tpx_ped.o: ../RTS_READER/daq_det.h
tpxPed.o: ../../include/rtsLog.h
tpxPed.o: ../../include/TPC/rowlen.h
tpxPed.o: ../DAQ_TPX/tpxCore.h
tpxPed.o: ../../include/TPX/tpx_rdo.h
tpxPed.o: ../DAQ_TPX/tpxPed.h
tpxPed.o: tpxCore.h
tpxStat.o: ../../include/rtsLog.h
tpxStat.o: ../../include/daqModes.h
tpxStat.o: ../DAQ_TPX/tpxCore.h
tpxStat.o: ../../include/TPX/tpx_rdo.h
tpxStat.o: ../DAQ_TPX/tpxStat.h
daq_tpx.o: ../RTS_READER/rts_reader.h
daq_tpx.o: ../../include/rts.h
daq_tpx.o: ../../include/rtsLog.h
daq_tpx.o: ../../include/rtsSystems.h
daq_tpx.o: ../RTS_READER/daq_det.h
daq_tpx.o: ../RTS_READER/daq_dta.h
tpxCore.o: ../../include/TPX/tpx_rdo.h
tpxFCF.o: tpxCore.h
tpxGain.o: tpxCore.h
tpxPed.o: tpxCore.h
