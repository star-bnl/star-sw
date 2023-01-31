
include ../rtsmakefile.def
include ../rtsplus.def

# zap LDLIBS
LDLIBS = 


LIBS = librtslog.a

BINS =  logTest rtsLog

# NOTE: rtsLogServer is disabled!!! Moved to  online/RTS/src/LOG_SERVER
# I also disabled other servers
# critServer operDisplay

all: $(LIBS) $(BINS)

libs: $(LIBS)

librtslog.a : rtsLogUnix.o
	ar -cruvs librtslog.a rtsLogUnix.o

logTest: logTest.o librtslog.a

rtsLog.o : rtsLog.C
	$(CXX) $(CXXFLAGS) -DRTS_LOG_COLORED  -c  rtsLog.C

rtsLog: rtsLog.o rtsLogUnix.o
	$(CXX) $(CXXFLAGS) rtsLogUnix.o rtsLog.o -o rtsLog
	
#operDisplay.sh:	


# DO NOT DELETE

logTest.o: ../../../../StRoot/RTS/include/rtsLog.h
rtsLog.o: ../../../../StRoot/RTS/include/rtsLog.h
rtsLogUnix.o: ../../../../StRoot/RTS/include/rtsLog.h
