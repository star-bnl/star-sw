################ boilerplate #######################
-include ../rtsmakefile.def
-include ../rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_esmd.o emc_reader.o

LIBS = libesmd.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libesmd.a : $(OBJS)
	@$(RTS_AR)

