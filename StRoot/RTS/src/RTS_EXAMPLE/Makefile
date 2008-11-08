-include ../rtsmakefile.def	# for RTS
-include ../rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS += libsc.a libdaqReader.a libsfs.a librtslog.a
VPATH += ../DAQ_READER ../DAQ_SC ../SFS ../LOG


BINS =  rts_example


all:   $(BINS)


rts_example: rts_example.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ 


# DO NOT DELETE

rts_example.o: ../../../../StRoot/RTS/include/rtsLog.h
rts_example.o: ../DAQ_READER/daqReader.h
rts_example.o: ../DAQ_READER/daq_dta.h
rts_example.o: ../../../../StRoot/RTS/include/rts.h
rts_example.o: ../DAQ_READER/daq_dta_structs.h
rts_example.o: ../DAQ_SC/daq_sc.h
rts_example.o: ../DAQ_READER/daq_det.h
