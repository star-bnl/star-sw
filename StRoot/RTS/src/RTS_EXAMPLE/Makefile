-include ../rtsmakefile.def	# for RTS
-include ../rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS += libsc.a libdaqReader.a libsfs.a librtslog.a
VPATH += ../DAQ_READER ../DAQ_SC ../DAQ_ESMD ../DAQ_BTOW ../DAQ_EMC ../SFS ../LOG


BINS =  rts_example


all:   $(BINS)


rts_example: rts_example.o $(LIBS) $(VOBJS)
	$(LINK.o) -o $@ $^ 


