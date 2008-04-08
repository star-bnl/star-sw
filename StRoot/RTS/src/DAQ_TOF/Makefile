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




OBJS := daq_tof.o

LIBS = libtof.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libtof.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_tof.o: ../../include/rtsLog.h
daq_tof.o: ../../include/rtsSystems.h
daq_tof.o: ../SFS/sfs_index.h
daq_tof.o: ../SFS/fs_index.h
daq_tof.o: ../SFS/sfs_base.h
daq_tof.o: ../RTS_READER/rts_reader.h
daq_tof.o: ../RTS_READER/daq_dta.h
daq_tof.o: ../../include/rts.h
daq_tof.o: ../RTS_READER/daq_dta_structs.h
daq_tof.o: daq_tof.h
daq_tof.o: ../RTS_READER/daq_det.h
daq_tof.o: ../RTS_READER/daq_det.h
