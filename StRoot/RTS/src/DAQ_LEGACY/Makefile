################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_legacy.o

LIBS = liblegacy.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

liblegacy.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_legacy.o: ../../include/rtsLog.h
daq_legacy.o: ../../include/rtsSystems.h
daq_legacy.o: ../../include/rts.h
daq_legacy.o: ../../include/daqFormats.h
daq_legacy.o: ../../include/SECTOR/sector.h
daq_legacy.o: ../../trg/include/trgStructures.h
daq_legacy.o: ../../include/L3/L3Formats.h
daq_legacy.o: daq_legacy.h
