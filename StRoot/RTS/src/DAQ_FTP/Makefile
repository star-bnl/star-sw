################ boilerplate #######################
-include ../rtsmakefile.def
-include ../../online/RTS/src/rtsmakefile.def

include $(RTS_DIR_PATH)/src/rtsplus.def

# now fix and/or stuff to what out rtsmakefile.def did...

# make sure to kill all our -lyada which rtsmakefile did...
LDLIBS = 

HOSTFLAGS +=  -fomit-frame-pointer -funroll-loops
DEFINES += -DRTS_LOG_COLORED



OBJS := daq_ftp.o ftp_reader.o

LIBS = libftp.a
BINS = 

all: $(LIBS) $(BINS)

libs: $(LIBS)

libftp.a : $(OBJS)
	@$(RTS_AR)

# DO NOT DELETE

daq_ftp.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_ftp.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_ftp.o: ../DAQ_READER/daqReader.h
daq_ftp.o: ../DAQ_READER/daqConfig.h
daq_ftp.o: ../DAQ_READER/daq_dta.h
daq_ftp.o: ../../../../StRoot/RTS/include/rts.h
daq_ftp.o: ../DAQ_READER/daq_dta_structs.h
daq_ftp.o: daq_ftp.h
daq_ftp.o: ../DAQ_READER/daq_det.h
ftp_reader.o: ../../../../StRoot/RTS/include/daqFormats.h
ftp_reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ftp_reader.o: ../../trg/include/trgDataDefs_46.h
ftp_reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ftp_reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
ftp_reader.o: ../../../../StRoot/RTS/include/rtsLog.h
ftp_reader.o: ../../../../StRoot/RTS/include/rts.h
ftp_reader.o: daq_ftp.h
ftp_reader.o: ../DAQ_READER/daq_det.h
ftpReader.o: ../DAQ_READER/daqReader.h
ftpReader.o: ../DAQ_READER/daqConfig.h
ftpReader.o: ../DAQ_READER/daq_dta.h
ftpReader.o: ../../../../StRoot/RTS/include/rts.h
ftpReader.o: ../DAQ_READER/daq_dta_structs.h
ftpReader.o: ../DAQ_READER/daq_det.h
ftpReader.o: ftpReader.h
ftpReader.o: daq_ftp.h
daq_ftp.o: ../DAQ_READER/daq_det.h
ftpReader.o: daq_ftp.h
ftpReader.o: ../DAQ_READER/daq_det.h
