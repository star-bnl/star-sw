
include ../rtsmakefile.def
include ../rtsplus.def

# zap LDLIBS
LDLIBS = 


LIBS = librtslog.a
BINS =  rtsLogServer critServer logTest operDisplay rtsLog operDisplay.sh


all: $(LIBS) $(BINS)

libs: $(LIBS)

librtslog.a : rtsLogUnix.o
	ar -cruvs librtslog.a rtsLogUnix.o

logTest: logTest.o librtslog.a

rtsLog: rtsLog.o librtslog.a

operDisplay.sh:	


# DO NOT DELETE

logTest.o: ../../../../StRoot/RTS/include/rtsLog.h
rtsLog.o: ../../../../StRoot/RTS/include/rtsLog.h
rtsLogUnix.o: ../../../../StRoot/RTS/include/rtsLog.h
