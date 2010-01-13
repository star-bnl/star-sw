-include ../rtsmakefile.def	# for RTS
-include ../rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS += libbsmd.a libbtow.a libemc.a libesmd.a libfpd.a libftp.a libl3.a libpmd.a libpp2pp.a \
	libric.a libsc.a libssd.a libsvt.a libtof.a libtpc.a libtpcfcf.a libtpx.a libtrg.a libetow.a \
	libdaqReader.a libsfs.a librtslog.a
VPATH += ../DAQ_BSMD ../DAQ_BTOW ../DAQ_EMC ../DAQ_ESMD ../DAQ_FPD ../DAQ_FTP ../DAQ_L3 ../DAQ_PMD \
	../DAQ_PP2PP ../DAQ_RIC ../DAQ_SC ../DAQ_SSD ../DAQ_SVT ../DAQ_TOF ../DAQ_TPC ../FCF ../DAQ_ETOW \
	../DAQ_TPX ../DAQ_TRG \
	../DAQ_READER  ../SFS ../LOG 


BINS = daqFileChopper rts_example tpc_rerun tpx_read_gains

all:   $(BINS)

//$(VOBJS):

rts_example: rts_example.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

tpc_rerun: tpc_rerun.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

tpx_read_gains: tpx_read_gains.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

sfs_stuff: sfs_stuff.o $(VOBJS)
	$(LINK.o) -o $@ $^ 

daqFileChopper: daqFileChopper.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive


# DO NOT DELETE

rts_example.o: ../../../../StRoot/RTS/include/rtsLog.h
rts_example.o: ../DAQ_READER/daqReader.h
rts_example.o: ../DAQ_READER/daqConfig.h
rts_example.o: ../DAQ_READER/daq_dta.h
rts_example.o: ../../../../StRoot/RTS/include/rts.h
rts_example.o: ../DAQ_READER/daq_dta_structs.h
rts_example.o: ../DAQ_BSMD/daq_bsmd.h
rts_example.o: ../DAQ_READER/daq_det.h
rts_example.o: ../DAQ_EMC/daq_emc.h
rts_example.o: ../DAQ_BTOW/daq_btow.h
rts_example.o: ../DAQ_ESMD/daq_esmd.h
rts_example.o: ../DAQ_ETOW/daq_etow.h
rts_example.o: ../DAQ_FPD/daq_fpd.h
rts_example.o: ../DAQ_FTP/daq_ftp.h
rts_example.o: ../DAQ_L3/daq_l3.h
rts_example.o: ../../../../StRoot/RTS/include/daqFormats.h
rts_example.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
rts_example.o: ../../../../StRoot/RTS/trg/include/trgStructures_32.h
rts_example.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
rts_example.o: ../DAQ_PMD/daq_pmd.h
rts_example.o: ../DAQ_PP2PP/daq_pp2pp.h
rts_example.o: ../DAQ_RIC/daq_ric.h
rts_example.o: ../DAQ_SC/daq_sc.h
rts_example.o: ../DAQ_SSD/daq_ssd.h
rts_example.o: ../DAQ_SVT/daq_svt.h
rts_example.o: ../DAQ_TOF/daq_tof.h
rts_example.o: ../DAQ_TPC/daq_tpc.h
rts_example.o: ../DAQ_TPX/daq_tpx.h
rts_example.o: ../DAQ_TRG/daq_trg.h
sfs_stuff.o: ../../../../StRoot/RTS/include/rtsLog.h
sfs_stuff.o: ../DAQ_READER/daqReader.h
sfs_stuff.o: ../DAQ_READER/daqConfig.h
tpc_rerun.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc_rerun.o: ../DAQ_READER/daqReader.h
tpc_rerun.o: ../DAQ_READER/daqConfig.h
tpc_rerun.o: ../DAQ_READER/daq_dta.h
tpc_rerun.o: ../../../../StRoot/RTS/include/rts.h
tpc_rerun.o: ../DAQ_READER/daq_dta_structs.h
tpc_rerun.o: ../DAQ_TPX/daq_tpx.h
tpc_rerun.o: ../DAQ_READER/daq_det.h
tpx_gains.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx_gains.o: ../DAQ_READER/daqReader.h
tpx_gains.o: ../DAQ_READER/daqConfig.h
tpx_gains.o: ../DAQ_READER/daq_dta.h
tpx_gains.o: ../../../../StRoot/RTS/include/rts.h
tpx_gains.o: ../DAQ_READER/daq_dta_structs.h
tpx_gains.o: ../DAQ_TPX/daq_tpx.h
tpx_gains.o: ../DAQ_READER/daq_det.h
tpx_gains.o: ../DAQ_TPX/tpxGain.h
tpx_gains.o: ../DAQ_TPX/tpxCore.h
tpx_gains.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
