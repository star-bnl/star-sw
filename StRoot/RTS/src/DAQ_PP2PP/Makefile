################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED

ifdef PP_MVME
	DEFINES += -DPP_MVME
endif

OBJS := daq_pp2pp.o

LIBS = libpp2pp.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libpp2pp.a : $(OBJS)
	@$(RTS_AR)


# DO NOT DELETE

daq_pp2pp.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_pp2pp.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_pp2pp.o: ../SFS/sfs_index.h
daq_pp2pp.o: ../SFS/fs_index.h
daq_pp2pp.o: ../SFS/sfs_base.h
daq_pp2pp.o: ../DAQ_READER/daq_dta.h
daq_pp2pp.o: ../../../../StRoot/RTS/include/rts.h
daq_pp2pp.o: ../DAQ_READER/daq_dta_structs.h
daq_pp2pp.o: ../DAQ_READER/daqReader.h
daq_pp2pp.o: ../DAQ_READER/daqConfig.h
daq_pp2pp.o: daq_pp2pp.h
daq_pp2pp.o: ../DAQ_READER/daq_det.h
pp2ppReader.o: ../DAQ_READER/daqReader.h
pp2ppReader.o: ../DAQ_READER/daqConfig.h
pp2ppReader.o: ../DAQ_READER/daq_dta.h
pp2ppReader.o: ../../../../StRoot/RTS/include/rts.h
pp2ppReader.o: ../DAQ_READER/daq_dta_structs.h
pp2ppReader.o: ../DAQ_READER/daq_det.h
pp2ppReader.o: pp2ppReader.h
pp2ppReader.o: daq_pp2pp.h
daq_pp2pp.o: ../DAQ_READER/daq_det.h
pp2pp.o: daq_pp2pp.h
pp2pp.o: ../DAQ_READER/daq_det.h
pp2ppReader.o: daq_pp2pp.h
pp2ppReader.o: ../DAQ_READER/daq_det.h
