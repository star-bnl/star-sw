################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def


# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -g -funroll-loops
DEFINES += -DRTS_LOG_COLORED

# If I'm in Zagreb...
ifeq ($(HOST_NAME),lvedaq.irb.lo)
	DEFINES +=  -DTEST_RDO
endif
ifeq ($(HOST_NAME),localhost.localdomain)
	DEFINES +=  -DTEST_RDO
endif
ifeq ($(HOST_NAME),ettie.star.bnl.gov)
	DEFINES +=  -DTEST_ETTIE
endif


#DEFINES += -DDO_DBG


OBJS := daq_fcs.o fcs_data_c.o

LIBS = libfcs.a


all: $(LIBS) $(BINS)

libs: $(LIBS)

libfcs.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_fcs.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_fcs.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_fcs.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_fcs.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_fcs.o: ../../trg/include/trgDataDefs_46.h
daq_fcs.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_fcs.o: ../SFS/sfs_index.h
daq_fcs.o: ../SFS/fs_index.h
daq_fcs.o: ../SFS/sfs_base.h
daq_fcs.o: ../DAQ_READER/daqReader.h
daq_fcs.o: ../DAQ_READER/daqConfig.h
daq_fcs.o: ../DAQ_READER/daq_dta.h
daq_fcs.o: ../../../../StRoot/RTS/include/rts.h
daq_fcs.o: ../DAQ_READER/daq_dta_structs.h
daq_fcs.o: daq_fcs.h
daq_fcs.o: ../DAQ_READER/daq_det.h
daq_fcs.o: fcs_data_c.h
fcs_data_c.o: ../../../../StRoot/RTS/include/rtsLog.h
fcs_data_c.o: fcs_data_c.h
daq_fcs.o: ../DAQ_READER/daq_det.h
