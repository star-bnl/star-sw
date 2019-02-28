-include ../rtsmakefile.def	# for RTS
-include ../rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS += libbsmd.a libbtow.a libemc.a libesmd.a libfpd.a libftp.a libl3.a libpmd.a libpp2pp.a  \
	libric.a libsc.a libssd.a libsvt.a libtof.a libtpc.a libtpcfcf.a libtpx.a libtrg.a libfgt.a libmtd.a libhlt.a libl4.a libetow.a \
	libpxl.a libsst.a libfps.a librhicf.a libetof.a libitpc.a libfcs.a libstgc.a \
	libdaqReader.a libsfs.a librtslog.a

VPATH += ../DAQ_BSMD ../DAQ_BTOW ../DAQ_EMC ../DAQ_ESMD ../DAQ_FPD ../DAQ_FTP ../DAQ_L3 ../DAQ_PMD \
	../DAQ_PP2PP ../DAQ_RIC ../DAQ_SC ../DAQ_SSD ../DAQ_SVT ../DAQ_TOF ../DAQ_TPC ../FCF ../DAQ_HLT ../DAQ_ETOW \
	../DAQ_TPX ../DAQ_TRG ../DAQ_FGT ../DAQ_MTD ../DAQ_PXL ../DAQ_L4 ../DAQ_SST ../DAQ_FPS ../DAQ_RHICF ../DAQ_ETOF \
	../DAQ_ITPC ../DAQ_FCS ../DAQ_STGC \
	../DAQ_READER  ../SFS ../LOG 


BINS = daqFileMerger daqFileHacker daqFileChopper testReader testReader2 rts_example tpc_rerun tpx_read_gains

all:   $(BINS)

//$(VOBJS):

testMessaging: testMessaging.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

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

daqFileHacker: daqFileHacker.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

daqFileMerger: daqFileMerger.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

testReader: testReader.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

testReader2: testReader2.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive


# DO NOT DELETE

daqFileChopper.o: ../../../../StRoot/RTS/include/rtsLog.h
daqFileChopper.o: ../../../../StRoot/RTS/include/rtsSystems.h
daqFileChopper.o: ../DAQ_READER/daqReader.h
daqFileChopper.o: ../DAQ_READER/daqConfig.h
daqFileChopper.o: ../DAQ_READER/daq_dta.h
daqFileChopper.o: ../../../../StRoot/RTS/include/rts.h
daqFileChopper.o: ../DAQ_READER/daq_dta_structs.h
daqFileHacker.o: ../../../../StRoot/RTS/include/rtsLog.h
daqFileHacker.o: ../../../../StRoot/RTS/include/rtsSystems.h
daqFileHacker.o: ../../../../StRoot/RTS/include/daqFormats.h
daqFileHacker.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daqFileHacker.o: ../../trg/include/trgDataDefs_46.h
daqFileHacker.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daqFileHacker.o: ../SFS/sfs_index.h
daqFileHacker.o: ../SFS/fs_index.h
daqFileHacker.o: ../SFS/sfs_base.h
daqFileHacker.o: ../DAQ_READER/daqReader.h
daqFileHacker.o: ../DAQ_READER/daqConfig.h
daqFileHacker.o: ../DAQ_READER/daq_det.h
daqFileHacker.o: ../DAQ_READER/daq_dta.h
daqFileHacker.o: ../../../../StRoot/RTS/include/rts.h
daqFileHacker.o: ../DAQ_READER/daq_dta_structs.h
daqFileHacker.o: ../DAQ_BTOW/daq_btow.h
daqFileHacker.o: ../DAQ_EMC/daq_emc.h
daqFileMerger.o: ../../../../StRoot/RTS/include/rtsLog.h
daqFileMerger.o: ../../../../StRoot/RTS/include/rtsSystems.h
daqFileMerger.o: ../DAQ_READER/daqReader.h
daqFileMerger.o: ../DAQ_READER/daqConfig.h
daqFileMerger.o: ../DAQ_READER/daq_dta.h
daqFileMerger.o: ../../../../StRoot/RTS/include/rts.h
daqFileMerger.o: ../DAQ_READER/daq_dta_structs.h
rts_example.o: ../../../../StRoot/RTS/include/rtsLog.h
rts_example.o: ../../../../StRoot/RTS/include/rtsSystems.h
rts_example.o: ../DAQ_READER/daqReader.h
rts_example.o: ../DAQ_READER/daqConfig.h
rts_example.o: ../DAQ_READER/daq_dta.h
rts_example.o: ../../../../StRoot/RTS/include/rts.h
rts_example.o: ../DAQ_READER/daq_dta_structs.h
rts_example.o: ../../trg/include/trgConfNum.h
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
rts_example.o: ../../trg/include/trgDataDefs_46.h
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
rts_example.o: ../DAQ_TPX/tpxCore.h
rts_example.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
rts_example.o: ../DAQ_TRG/daq_trg.h
rts_example.o: ../DAQ_HLT/daq_hlt.h
rts_example.o: ../DAQ_L4/daq_l4.h
rts_example.o: ../DAQ_FGT/daq_fgt.h
rts_example.o: ../DAQ_MTD/daq_mtd.h
rts_example.o: ../DAQ_PXL/daq_pxl.h
rts_example.o: ../DAQ_SST/daq_sst.h
rts_example.o: ../DAQ_FPS/daq_fps.h
rts_example.o: ../DAQ_RHICF/daq_rhicf.h
rts_example.o: ../DAQ_ETOF/daq_etof.h
rts_example.o: ../DAQ_ITPC/daq_itpc.h
rts_example.o: ../DAQ_ITPC/itpcCore.h
rts_example.o: ../DAQ_READER/daq_dta_structs.h
rts_example.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
rts_example.o: ../DAQ_FCS/daq_fcs.h
rts_example.o: ../DAQ_STGC/daq_stgc.h
rts_example.o: ../DAQ_FGT/fgtPed.h
rts_example.o: ../DAQ_FGT/daq_fgt.h
rts_example.o: ../DAQ_SST/sstPed.h
rts_example.o: ../DAQ_SST/daq_sst.h
rts_example.trg.o: ../../../../StRoot/RTS/include/rtsLog.h
rts_example.trg.o: ../../../../StRoot/RTS/include/rtsSystems.h
rts_example.trg.o: ../DAQ_READER/daqReader.h
rts_example.trg.o: ../DAQ_READER/daqConfig.h
rts_example.trg.o: ../DAQ_READER/daq_dta.h
rts_example.trg.o: ../../../../StRoot/RTS/include/rts.h
rts_example.trg.o: ../DAQ_READER/daq_dta_structs.h
rts_example.trg.o: ../DAQ_BSMD/daq_bsmd.h
rts_example.trg.o: ../DAQ_READER/daq_det.h
rts_example.trg.o: ../DAQ_EMC/daq_emc.h
rts_example.trg.o: ../DAQ_BTOW/daq_btow.h
rts_example.trg.o: ../DAQ_ESMD/daq_esmd.h
rts_example.trg.o: ../DAQ_ETOW/daq_etow.h
rts_example.trg.o: ../DAQ_FPD/daq_fpd.h
rts_example.trg.o: ../DAQ_FTP/daq_ftp.h
rts_example.trg.o: ../DAQ_L3/daq_l3.h
rts_example.trg.o: ../../../../StRoot/RTS/include/daqFormats.h
rts_example.trg.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
rts_example.trg.o: ../../trg/include/trgDataDefs_46.h
rts_example.trg.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
rts_example.trg.o: ../DAQ_PMD/daq_pmd.h
rts_example.trg.o: ../DAQ_PP2PP/daq_pp2pp.h
rts_example.trg.o: ../DAQ_RIC/daq_ric.h
rts_example.trg.o: ../DAQ_SC/daq_sc.h
rts_example.trg.o: ../DAQ_SSD/daq_ssd.h
rts_example.trg.o: ../DAQ_SVT/daq_svt.h
rts_example.trg.o: ../DAQ_TOF/daq_tof.h
rts_example.trg.o: ../DAQ_TPC/daq_tpc.h
rts_example.trg.o: ../DAQ_TPX/daq_tpx.h
rts_example.trg.o: ../DAQ_TRG/daq_trg.h
rts_example.trg.o: ../DAQ_HLT/daq_hlt.h
rts_example.trg.o: ../DAQ_L4/daq_l4.h
rts_example.trg.o: ../DAQ_FGT/daq_fgt.h
rts_example.trg.o: ../DAQ_MTD/daq_mtd.h
rts_example.trg.o: ../DAQ_PXL/daq_pxl.h
sfs_stuff.o: ../../../../StRoot/RTS/include/rtsLog.h
sfs_stuff.o: ../DAQ_READER/daqReader.h
sfs_stuff.o: ../DAQ_READER/daqConfig.h
testReader2.o: ../../../../StRoot/RTS/include/rtsLog.h
testReader2.o: ../../../../StRoot/RTS/include/rtsSystems.h
testReader2.o: ../DAQ_READER/daqReader.h
testReader2.o: ../DAQ_READER/daqConfig.h
testReader2.o: ../../../../StRoot/RTS/include/SUNRT/clock.h
testReader.o: ../../../../StRoot/RTS/include/rtsLog.h
testReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
testReader.o: ../DAQ_READER/daqReader.h
testReader.o: ../DAQ_READER/daqConfig.h
testReader.o: ../../../../StRoot/RTS/include/SUNRT/clock.h
tpc_rerun.o: ../../../../StRoot/RTS/include/rtsLog.h
tpc_rerun.o: ../DAQ_READER/daqReader.h
tpc_rerun.o: ../DAQ_READER/daqConfig.h
tpc_rerun.o: ../DAQ_READER/daq_dta.h
tpc_rerun.o: ../../../../StRoot/RTS/include/rts.h
tpc_rerun.o: ../DAQ_READER/daq_dta_structs.h
tpc_rerun.o: ../DAQ_TPX/daq_tpx.h
tpc_rerun.o: ../DAQ_READER/daq_det.h
tpx_read_gains.o: ../DAQ_TPX/tpxGain.h
tpx_read_gains.o: ../DAQ_TPX/tpxCore.h
tpx_read_gains.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx_read_gains.o: ../../../../StRoot/RTS/include/TPX/tpx_rdo.h
