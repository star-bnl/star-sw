# A prototypical makefile


#potentially overrule TARGET_SYSTEM
#choose one of:
#TARGET_SYSTEM:=MVME
#TARGET_SYSTEM:=I960

# common intelligence
include ../rtsmakefile.def


#local flags etc.
VERSION := 2.0


#override or addition to predefined variables
CXXFLAGS +=  -fno-strict-aliasing -g -I../../include/TPC -DEVP_VERSION=\"EVP_VERSION=${VERSION}\" -I..

DEFINES += -DRTS_LOG_COLORED

ifeq ($(TARGET_SYSTEM),SUN)
LDLIBS := -lsocket -lnsl
else
LDLIBS :=
endif


# real stuff

all : libs

libs: libdaqReader.a


READER_FILES = msgNQLib.o daqReader.o cfgutil.o 

# Tonko: new style
VPATH_AUX = rtsLogUnix.o fs_index.o sfs_index.o sfs_index_daq.o rts_reader.o 

VPATH = ../RTS_READER ../LOG ../SFS ../FCF

libdaqReader.a : ${READER_FILES} $(VPATH_AUX)
	@$(RTS_AR) libdaqReader.a $?


#readEvents: readEvents.o libdaqReader.a



clean :
	rm -f *.o *.a *.so
	make -C ../SFS clean


# should not be called by non-DAQ personel...
install : 
	$(INSTALL) libdaqReader.a libdaqReaderSO.so  $(LIBDIR)




depend :
	@$(MAKEDEPEND) 


# DO NOT DELETE

cfgutil.o: ../../../../StRoot/RTS/include/RC_Config.h
cfgutil.o: ../../../../StRoot/RTS/include/rtsSystems.h
cfgutil.o: ../../../../StRoot/RTS/include/rts.h
cfgutil.o: ../../../../StRoot/RTS/include/rtsLog.h
cfgutil.o: cfgutil.h
daqReader.o: ./daqReader.hh
daqReader.o: ../../../../StRoot/RTS/include/evp.h
daqReader.o: ../../../../StRoot/RTS/include/rts.h
daqReader.o: cfgutil.h
daqReader.o: ../SFS/sfs_index.h
daqReader.o: ../SFS/fs_index.h
daqReader.o: ../SFS/sfs_base.h
daqReader.o: ../RTS_READER/rts_reader.h
daqReader.o: daqReaderClass.h
daqReader.o: ../../../../StRoot/RTS/include/iccp2k.h
daqReader.o: ../../../../StRoot/RTS/include/rtsLog.h
daqReader.o: ../../../../StRoot/RTS/include/daqFormats.h
daqReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daqReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
daqReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daqReader.o: ../../../../StRoot/RTS/include/iccp.h
daqReader.o: ../../../../StRoot/RTS/include/cmds.h
daqReader.o: ../../../../StRoot/RTS/include/rtsCmds.h
daqReader.o: ../../../../StRoot/RTS/include/tasks.h
daqReader.o: ../../../../StRoot/RTS/include/status.h
daqReader.o: ../../../../StRoot/RTS/include/iccpHeader.h
daqReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
daqReader.o: ./msgNQLib.h
msgNQLib.o: ../../../../StRoot/RTS/include/rtsLog.h
msgNQLib.o: ./msgNQLib.h
readEvents.o: ../../../../StRoot/RTS/include/rtsLog.h
readEvents.o: ./daqReader.hh
readEvents.o: ../../../../StRoot/RTS/include/evp.h
readEvents.o: ../../../../StRoot/RTS/include/rts.h
readEvents.o: cfgutil.h
readEvents.o: ../SFS/sfs_index.h
readEvents.o: ../SFS/fs_index.h
readEvents.o: ../SFS/sfs_base.h
readEvents.o: ../RTS_READER/rts_reader.h
readEvents.o: daqReaderClass.h
readEvents.o: ../../../../StRoot/RTS/include/iccp2k.h
daqReaderClass.o: ../../../../StRoot/RTS/include/evp.h
daqReaderClass.o: ../../../../StRoot/RTS/include/rts.h
daqReaderClass.o: cfgutil.h
daqReaderClass.o: ../../../../StRoot/RTS/include/iccp2k.h
daqReader.o: ../../../../StRoot/RTS/include/evp.h
daqReader.o: ../../../../StRoot/RTS/include/rts.h
daqReader.o: cfgutil.h
daqReader.o: ../SFS/sfs_index.h
daqReader.o: ../SFS/fs_index.h
daqReader.o: ../SFS/sfs_base.h
daqReader.o: ../RTS_READER/rts_reader.h
daqReader.o: daqReaderClass.h
daqReader.o: ../../../../StRoot/RTS/include/iccp2k.h
