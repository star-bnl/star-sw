RTS_ST_ROOT = ../../../../StRoot/RTS

include $(RTS_ST_ROOT)/src/rtsmakefile.def
include $(RTS_ST_ROOT)/src/rtsplus.def


S_DATE := $(shell date +%Y%m%d)
S_TIME := $(shell date +%H%M%S)
S_UNIX := $(shell date +%s)

INCLUDES = -I.. -I$(RTS_ST_ROOT)/include -I$(RTS_ST_ROOT)/src -I$(RTS_ST_ROOT)/trg/include
DEFINES += -DRTS_LOG_COLORED

VOBJS += libitpc.a libtpx.a libdaqReader.a libsfs.a librtslog.a libsunRT.a
VPATH += $(RTS_ST_ROOT)/src/DAQ_ITPC $(RTS_ST_ROOT)/src/DAQ_TPX $(RTS_ST_ROOT)/src/DAQ_READER  $(RTS_ST_ROOT)/src/SFS \
	$(RTS_ST_ROOT)/src/LOG ../SUNRT ../TEF_RDO  ../DDL_LIB

LDLIBS := -lm -lpthread

BINS = itpc_rerun tpc23_offline fcf_new tpc23_test tpc23_simulation copyover irdo_fast 
#tpx

all:   $(BINS)

#DEFINES += -DDBG_PRINT

tpc23_test: tpc23_test.o tpx23.o itpc23.o tpc23_base.o $(VOBJS)
#	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

tpc23_simulation: tpc23_simulation.o tpx23.o itpc23.o tpc23_base.o $(VOBJS)

itpc_rerun: itpc_rerun.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

tpc23_offline: tpc23_offline.o itpc23.o tpx23.o tpc23_base.o libitpc.a libtpx.a libdaqReader.a libsfs.a librtslog.a 
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

copyover: copyover.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

irdo_fast: irdo_fast.o itpc23.o tpc23_base.o tef_c.o rb_c.o libitpc.a libtpx.a librtslog.a ipcQLib.o libdaqReader.a libsfs.a
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

tpx: tpx.o tpx23.o tpc23_base.o ../DDB/rb_tpx_c.o tef_c.o rb_c.o ddl_lib.o librtslog.a libsunRT.a libtpx.a

# DO NOT DELETE

copyover.o: ../../../../StRoot/RTS/include/rtsLog.h
copyover.o: ../../../../StRoot/RTS/src/DAQ_READER/daqReader.h
copyover.o: ../../../../StRoot/RTS/src/DAQ_READER/daqConfig.h
copyover.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta.h
copyover.o: ../../../../StRoot/RTS/include/rts.h
copyover.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
copyover.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_det.h
irdo_fast.o: ../../../../StRoot/RTS/include/rtsLog.h
irdo_fast.o: ../../../../StRoot/RTS/include/DAQ1000/rb.hh
irdo_fast.o: ../../../../StRoot/RTS/include/RORC/rorc_lib.h
irdo_fast.o: ../../../../StRoot/RTS/include/RORC/ddl_def.h
irdo_fast.o: ../../../../StRoot/RTS/include/SUNRT/ipcQLib.hh
irdo_fast.o: ../../../../StRoot/RTS/include/SUNRT/clock.h
irdo_fast.o: ../TEF_RDO/rb_c.h
irdo_fast.o: ../TEF_RDO/rb_main_c.h
irdo_fast.o: ../TEF_RDO/tef_c.h
irdo_fast.o: itpc23.h
irdo_fast.o: tpc23_base.h
itpc23.o: ../../../../StRoot/RTS/include/rtsLog.h
itpc23.o: ../../../../StRoot/RTS/include/rts.h
itpc23.o: ../../../../StRoot/RTS/include/rtsSystems.h
itpc23.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcCore.h
itpc23.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
itpc23.o: itpc23.h
itpc23.o: tpc23_base.h
itpcFCF2.o: ../../../../StRoot/RTS/include/rtsLog.h
itpcFCF2.o: ../../../../StRoot/RTS/include/rtsSystems.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta.h
itpcFCF2.o: ../../../../StRoot/RTS/include/rts.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_TPX/daq_tpx.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_det.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_TPX/tpxFCF_flags.h
itpcFCF2.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_ITPC/daq_itpc.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcCore.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcPed.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcInterpreter.h
itpcFCF2.o: ../../../../StRoot/RTS/include/I386/atomic.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpc_rowlen.h
itpcFCF2.o: itpcFCF2.h
tpc23_base.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc23_base.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_READER/daqReader.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_READER/daqConfig.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta.h
tpc23_base.o: ../../../../StRoot/RTS/include/rts.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_det.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_TPX/daq_tpx.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_TPX/tpxFCF_flags.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_ITPC/daq_itpc.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcFCF.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcPed.h
tpc23_base.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpc23_base.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpc_rowlen.h
tpc23_base.o: tpc23_base.h
tpc23_fcf.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc23_fcf.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpc23_fcf.o: ../../../../StRoot/RTS/src/DAQ_TPX/tpxCore.h
tpc23_fcf.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpc23_fcf.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
tpc23_fcf.o: support.h
tpc23_fcf.o: tpc23_fcf.h
tpc23_offline.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_READER/daqReader.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_READER/daqConfig.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta.h
tpc23_offline.o: ../../../../StRoot/RTS/include/rts.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_det.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_ITPC/daq_itpc.h
tpc23_offline.o: ../../../../StRoot/RTS/src/DAQ_TPX/daq_tpx.h
tpc23_offline.o: ../TPC23/tpx23.h
tpc23_offline.o: tpc23_base.h
tpc23_offline.o: ../TPC23/itpc23.h
tpc23_simulation.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc23_simulation.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
tpc23_simulation.o: ../../../../StRoot/RTS/src/DAQ_TPX/tpxFCF_flags.h
tpc23_simulation.o: ../TPC23/tpx23.h
tpc23_simulation.o: tpc23_base.h
tpc23_simulation.o: ../TPC23/itpc23.h
tpc23_test.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_READER/daqReader.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_READER/daqConfig.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta.h
tpc23_test.o: ../../../../StRoot/RTS/include/rts.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_dta_structs.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_READER/daq_det.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcFCF.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcPed.h
tpc23_test.o: ../../../../StRoot/RTS/src/DAQ_TPX/tpxFCF_flags.h
tpc23_test.o: ../DDB/ddb.h
tpc23_test.o: ../../../../StRoot/RTS/include/rtsMonitor.h
tpc23_test.o: ../../../../StRoot/RTS/include/RC_Config.h
tpc23_test.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpc23_test.o: ../TEF_RDO/rb_c.h
tpc23_test.o: ../TEF_RDO/rb_main_c.h
tpc23_test.o: ../TEF_RDO/tef_c.h
tpc23_test.o: ../DDB/l_atomic.h
tpc23_test.o: ../../../../StRoot/RTS/include/I386/atomic.h
tpc23_test.o: support.h
tpc23_test.o: tpx23.h
tpc23_test.o: tpc23_base.h
tpc23_test.o: itpc23.h
tpx23.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx23.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpx23.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpx23.o: ../../../../StRoot/RTS/include/DAQ1000/rdo_cmds.h
tpx23.o: tpx23.h
tpx23.o: tpc23_base.h
tpx23.o: ../../../../StRoot/RTS/src/DAQ_TPX/tpxCore.h
tpx23.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
tpx.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx.o: ../../../../StRoot/RTS/include/SUNRT/ipcQLib.hh
tpx.o: ../../../../StRoot/RTS/include/SUNRT/clock.h
tpx.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpx.o: ../../../../StRoot/RTS/include/DAQ1000/rdo_cmds.h
tpx.o: ../../../../StRoot/RTS/include/DAQ1000/ddl_struct.h
tpx.o: ../DDB/rb_tpx_c.h
tpx.o: ../TEF_RDO/rb_c.h
tpx.o: ../TEF_RDO/rb_main_c.h
tpx.o: ../TEF_RDO/tef_c.h
tpx.o: ../../../../StRoot/RTS/include/RORC/rorc_lib.h
tpx.o: ../../../../StRoot/RTS/include/RORC/ddl_def.h
tpx.o: tpx23.h
tpx.o: tpc23_base.h
itpc23.o: tpc23_base.h
itpcFCF2.o: ../../../../StRoot/RTS/src/DAQ_ITPC/itpcPed.h
tpx23.o: tpc23_base.h
