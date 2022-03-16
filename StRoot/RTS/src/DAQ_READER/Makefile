# common intelligence
include ../rtsmakefile.def
include ../rtsplus.def


#override or addition to predefined variables

DEFINES += -DRTS_LOG_COLORED

ifeq ($(TARGET_SYSTEM),SUN)
LDLIBS := -lsocket -lnsl
else
LDLIBS :=
endif

ifdef PP_MVME
	DEFINES += -DPP_MVME
endif

# real stuff

all : libs

libs: libdaqReader.a


READER_FILES = msgNQLib.o daqReader.o cfgutil.o daq_det.o daq_dta.o


libdaqReader.a : ${READER_FILES}
	@$(RTS_AR)


# DO NOT DELETE

cfgutil.o: ../../../../StRoot/RTS/include/RC_Config.h
cfgutil.o: ../../../../StRoot/RTS/include/rtsSystems.h
cfgutil.o: ../../../../StRoot/RTS/include/rts.h
cfgutil.o: ../../../../StRoot/RTS/include/rtsLog.h
cfgutil.o: cfgutil.h
daq_det.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_det.o: ../../../../StRoot/RTS/include/rts.h
daq_det.o: ../../../../StRoot/RTS/include/rtsSystems.h
daq_det.o: ../../../../StRoot/RTS/include/daqFormats.h
daq_det.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daq_det.o: ../../trg/include/trgDataDefs_46.h
daq_det.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daq_det.o: ../SFS/sfs_index.h
daq_det.o: ../SFS/fs_index.h
daq_det.o: ../SFS/sfs_base.h
daq_det.o: daqReader.h
daq_det.o: daqConfig.h
daq_det.o: daq_det.h
daq_det.o: daq_dta.h
daq_det.o: daq_dta_structs.h
daq_dta.o: ../../../../StRoot/RTS/include/rtsLog.h
daq_dta.o: daq_dta.h
daq_dta.o: ../../../../StRoot/RTS/include/rts.h
daq_dta.o: daq_dta_structs.h
daqReader.o: ../../../../StRoot/RTS/include/iccp2k.h
daqReader.o: ../../../../StRoot/RTS/include/daqFormats.h
daqReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
daqReader.o: ../../trg/include/trgDataDefs_46.h
daqReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
daqReader.o: ../../../../StRoot/RTS/include/rtsLog.h
daqReader.o: ../../../../StRoot/RTS/include/iccp.h
daqReader.o: ../../../../StRoot/RTS/include/cmds.h
daqReader.o: ../../../../StRoot/RTS/include/rtsCmds.h
daqReader.o: ../../../../StRoot/RTS/include/tasks.h
daqReader.o: ../../../../StRoot/RTS/include/status.h
daqReader.o: ../../../../StRoot/RTS/include/iccpHeader.h
daqReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
daqReader.o: ../SFS/sfs_index.h
daqReader.o: ../SFS/fs_index.h
daqReader.o: ../SFS/sfs_base.h
daqReader.o: ../../../../StRoot/RTS/include/rts.h
daqReader.o: daqReader.h
daqReader.o: daqConfig.h
daqReader.o: msgNQLib.h
daqReader.o: cfgutil.h
daqReader.o: daq_det.h
msgNQLib.o: ../../../../StRoot/RTS/include/rtsLog.h
msgNQLib.o: msgNQLib.h
readEvents.o: ../../../../StRoot/RTS/include/rtsLog.h
readEvents.o: daqReader.h
readEvents.o: daqConfig.h
daq_dta.o: ../../../../StRoot/RTS/include/rts.h
daq_dta.o: daq_dta_structs.h
daqReader.o: daqConfig.h
