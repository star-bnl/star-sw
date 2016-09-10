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


OBJS := itpcCore.o

LIBS = libitpc.a


all: $(LIBS) $(BINS)

libs: $(LIBS)

libitpc.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

itpcCore.o: itpc_maps.h
itpcCore.o: itpc_rowlen.h
itpcCore.o: itpc_padplane.h
itpcCore.o: itpcCore.h
