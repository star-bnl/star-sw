-include ../rtsmakefile.def
-include ../rtsplus.def

LDLIBS = 

DEFINES += -DRTS_LOG_COLORED

OBJS := fcfClass.o fcfAfterburner.o

LIBS = libtpcfcf.a

all: $(LIBS)

libs: $(LIBS)

libtpcfcf.a: $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

fcfAfterburner.o: ../../../../StRoot/RTS/include/rtsLog.h
fcfAfterburner.o: ../../../../StRoot/RTS/include/fcfClass.hh
fcfAfterburner.o: ../../../../StRoot/RTS/include/rts.h
fcfAfterburner.o: ../../../../StRoot/RTS/include/TPC/padfinder.h
fcfClass.o: ../../../../StRoot/RTS/include/rts.h
fcfClass.o: ../../../../StRoot/RTS/include/rtsSystems.h
fcfClass.o: ../../../../StRoot/RTS/include/fcfClass.hh
fcfClass.o: ../../../../StRoot/RTS/include/rtsLog.h
