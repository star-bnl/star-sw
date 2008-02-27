
# need to figure out if I'm inside StRoot or src

# silently attempt to include the rtsmakefile.def from
# the possible places:
-include ../rtsmakefile.def	# for RTS
-include ../../online/RTS/src/rtsmakefile.def	# if in StRoot tree

include $(RTS_DIR_PATH)/src/rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS += libtpx.a libtof.a libsfs.a
VPATH += ../SFS ../DAQ_TPX ../DAQ_TOF
ifdef IS_STROOT
	DEFINES += -DRTS_DISABLE_LOG
else
	VOBJS += librtslog.a
	VPATH += ../LOG
endif




BINS =  Reader root_example dump_sfs

LIBS = librts_reader.a

OBJS = rts_reader.o daq_det.o daq_dta.o




all:  $(LIBS) $(BINS)

libs: $(LIBS)

Reader: Reader.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a

dump_sfs: dump_sfs.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a

root_example: root_example.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ librts_reader.a


$(LIBS): $(OBJS)
	@$(RTS_AR)




	



# DO NOT DELETE

daq_det.o: ../RTS_READER/daq_det.h
daq_det.o: ../RTS_READER/rts_reader.h
daq_det.o: ../../include/rts.h
daq_det.o: ../../include/rtsLog.h
daq_det.o: ../../include/rtsSystems.h
daq_det.o: ../RTS_READER/daq_dta.h
daq_det.o: ../SFS/sfs_index.h
daq_det.o: ../SFS/fs_index.h
daq_dta.o: ../../include/rtsLog.h
daq_dta.o: ../RTS_READER/daq_dta.h
daq_dta.o: ../../include/rts.h
daq_dta.o: ../RTS_READER/rts_reader.h
daq_dta.o: ../../include/rtsSystems.h
daq_dta.o: ../RTS_READER/daq_det.h
rts_reader.o: rts_reader.h
rts_reader.o: ../../include/rts.h
rts_reader.o: ../../include/rtsLog.h
rts_reader.o: ../../include/rtsSystems.h
rts_reader.o: ../RTS_READER/daq_det.h
rts_reader.o: ../RTS_READER/rts_reader.h
rts_reader.o: ../RTS_READER/daq_dta.h
rts_reader.o: ../SFS/sfs_index.h
rts_reader.o: ../SFS/fs_index.h
rts_reader.o: ../DAQ_TPX/daq_tpx.h
rts_reader.o: ../DAQ_TOF/daq_tof.h
dump_sfs.o: ../SFS/sfs_index.h
dump_sfs.o: ../SFS/fs_index.h
dump_sfs.o: ../../include/rtsLog.h
legacy.o: ../../include/rtsLog.h
legacy.o: rts_reader.h
legacy.o: ../../include/rts.h
legacy.o: ../../include/rtsSystems.h
legacy.o: ../RTS_READER/daq_det.h
legacy.o: ../RTS_READER/rts_reader.h
legacy.o: ../RTS_READER/daq_dta.h
legacy.o: ../SFS/sfs_index.h
legacy.o: ../SFS/fs_index.h
Reader.o: ../../include/rtsLog.h
Reader.o: ../RTS_READER/rts_reader.h
Reader.o: ../../include/rts.h
Reader.o: ../../include/rtsSystems.h
Reader.o: ../RTS_READER/daq_det.h
Reader.o: ../RTS_READER/daq_dta.h
root_example.o: ../RTS_READER/rts_reader.h
root_example.o: ../../include/rts.h
root_example.o: ../../include/rtsLog.h
root_example.o: ../../include/rtsSystems.h
root_example.o: ../RTS_READER/daq_det.h
root_example.o: ../RTS_READER/daq_dta.h
daq_det.o: ../RTS_READER/rts_reader.h
daq_det.o: ../../include/rts.h
daq_det.o: ../../include/rtsLog.h
daq_det.o: ../../include/rtsSystems.h
daq_det.o: ../RTS_READER/daq_det.h
daq_det.o: ../RTS_READER/daq_dta.h
daq_dta.o: ../../include/rts.h
daq_dta.o: ../RTS_READER/rts_reader.h
daq_dta.o: ../../include/rtsLog.h
daq_dta.o: ../../include/rtsSystems.h
daq_dta.o: ../RTS_READER/daq_det.h
daq_dta.o: ../RTS_READER/daq_dta.h
rts_reader.o: ../../include/rts.h
rts_reader.o: ../../include/rtsLog.h
rts_reader.o: ../../include/rtsSystems.h
rts_reader.o: ../RTS_READER/daq_det.h
rts_reader.o: ../RTS_READER/rts_reader.h
rts_reader.o: ../RTS_READER/daq_dta.h
