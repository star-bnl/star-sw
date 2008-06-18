-include ../rtsmakefile.def	# for RTS
-include ../rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS +=  libtpc.a libtpx.a libtof.a libpp2pp.a libesmd.a liblegacy.a libsfs.a librtslog.a
VPATH += ../DAQ_TPC ../DAQ_TPX ../DAQ_TOF ../DAQ_PP2PP ../DAQ_ESMD ../DAQ_LEGACY ../SFS ../LOG


BINS =  Reader tpx_simulation

LIBS = librts_reader.a

OBJS = rts_reader.o daq_det.o daq_dta.o daq_dta_dict.o




all:  $(LIBS) $(BINS)

libs: $(LIBS)

Reader: Reader.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a

tpx_simulation: tpx_simulation.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a


# used locally
dump_sfs: dump_sfs.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a

root_example: root_example.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a


$(LIBS): $(OBJS)
	@$(RTS_AR)



# DO NOT DELETE

daq_det.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_det.o: ../SFS/sfs_index.h
daq_det.o: ../SFS/fs_index.h
daq_det.o: ../SFS/sfs_base.h
daq_det.o: ../RTS_READER/rts_reader.h
daq_det.o: ../RTS_READER/daq_det.h
daq_det.o: ../RTS_READER/daq_dta.h
daq_det.o: ../../../../StRoot/RTS/include/rts.h
daq_det.o: daq_dta_structs.h
daq_dta.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_dta.o: daq_dta.h
daq_dta.o: ../../../../StRoot/RTS/include/rts.h
daq_dta.o: daq_dta_structs.h
daq_dta_dict.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_dta_dict.o: daq_dta_structs.h
rts_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
rts_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
rts_reader.o: ../SFS/sfs_index.h
rts_reader.o: ../SFS/fs_index.h
rts_reader.o: ../SFS/sfs_base.h
rts_reader.o: ../DAQ_TPX/daq_tpx.h
rts_reader.o: ../RTS_READER/daq_det.h
rts_reader.o: ../DAQ_TOF/daq_tof.h
rts_reader.o: ../DAQ_PP2PP/daq_pp2pp.h
rts_reader.o: ../DAQ_ESMD/daq_esmd.h
rts_reader.o: ../DAQ_TPC/daq_tpc.h
rts_reader.o: rts_reader.h
rts_reader.o: daq_det.h
dump_sfs.o: ../SFS/sfs_index.h
dump_sfs.o: ../SFS/fs_index.h
dump_sfs.o: ../SFS/sfs_base.h
dump_sfs.o: ../../../../StRoot/RTS/include/rtsLog.h
Reader.o: ../../../../StRoot/RTS/include/rtsLog.h
Reader.o: ../RTS_READER/rts_reader.h
Reader.o: ../RTS_READER/daq_dta.h
Reader.o: ../../../../StRoot/RTS/include/rts.h
Reader.o: daq_dta_structs.h
Reader.o: ../RTS_READER/daq_det.h
root_example.o: ../../../../StRoot/RTS/include/rtsLog.h
root_example.o: ../RTS_READER/rts_reader.h
root_example.o: ../RTS_READER/daq_dta.h
root_example.o: ../../../../StRoot/RTS/include/rts.h
root_example.o: daq_dta_structs.h
root_example.o: ../RTS_READER/daq_det.h
tpx_simulation.o: ../../../../StRoot/RTS/include/rtsLog.h
tpx_simulation.o: ../RTS_READER/rts_reader.h
tpx_simulation.o: ../RTS_READER/daq_dta.h
tpx_simulation.o: ../../../../StRoot/RTS/include/rts.h
tpx_simulation.o: daq_dta_structs.h
tpx_simulation.o: ../RTS_READER/daq_det.h
daq_dta.o: ../../../../StRoot/RTS/include/rts.h
daq_dta.o: daq_dta_structs.h
