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

all : libs  special  # ppreader

libs: libevp.${VERSION}.a

svt: svt.o libevp.${VERSION}.a

READER_FILES = msgNQLib.o evpSupport.o evpReader.o tpcReader.o svtReader.o ftpReader.o \
		emcReader.o tofReader.o pmdReader.o fpdReader.o ricReader.o l3Reader.o \
		trgReader.o trgReader12.o trgReader20.o trgReader21.o trgReader22.o trgReader30.o trgReader10.o trgReader32.o \
		ssdReader.o pp2ppReader.o \
		sanityCheckers.o scReader.o tpcFCFReader.o fcfClass.o fcfAfterburner.o cfgutil.o 


# Tonko: new style
VPATH_AUX = rtsLogUnix.o fcfClass.o fcfAfterburner.o fs_index.o sfs_index.o sfs_index_daq.o get_line.o  \
	daq_tpx.o rts_reader.o daq_det.o daq_dta.o daq_tof.o tpxCore.o tpxFCF.o tpxGain.o tpxPed.o 
VPATH = ../RTS_READER ../DAQ_TOF ../DAQ_TPX ../LOG ../SFS ../FCF


libevp.${VERSION}.a : ${READER_FILES} $(VPATH_AUX)
	@$(RTS_AR) libevp.${VERSION}.a $?

#	ld -G -o libevpSO.${VERSION}.so $?

special: special.o libevp.${VERSION}.a

jtest: jtest.o libevp.${VERSION}.a

grpRates: grpRates.o libevp.${VERSION}.a

header: header.o libevp.${VERSION}.a

sfsSpecial:  sfsSpecial.o libevp.${VERSION}.a

cfgutil: cfgutil.o libevp.${VERSION}.a


#ppreader: ppreader.o msgNQLib.o evpSupport.o evpReader.o pp2ppReader.o rtsLogUnix.o

clean :
	@rm -f *.o *.a *.so
	@rm -f special ppreader 
	@rm -rf TAR *.tar
	make -C ../SFS clean
	make -C ../RTS_READER clean
	make -C ../DAQ_TPX clean
	make -C ../DAQ_TOF clean

# should not be called by non-DAQ personel...
install : 
	$(INSTALL) libevp.${VERSION}.a libevpSO.${VERSION}.so  $(LIBDIR)
	$(INSTALL) special ppreader $(BINDIR)

tarer: special
	tar cvf evp_reader.tar 

depend :
	@$(MAKEDEPEND) 


# DO NOT DELETE

cfgutil.o: ../../../../StRoot/RTS/include/RC_Config.h
cfgutil.o: ../../../../StRoot/RTS/include/rtsSystems.h
cfgutil.o: ../../../../StRoot/RTS/include/rts.h
cfgutil.o: ../../../../StRoot/RTS/include/rtsLog.h
cfgutil.o: cfgutil.h
emcReader.o: ../../../../StRoot/RTS/include/rtsLog.h
emcReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
emcReader.o: ../../../../StRoot/RTS/include/daqFormats.h
emcReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
emcReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
emcReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
emcReader.o: ./evpReader.hh
emcReader.o: ../../../../StRoot/RTS/include/evp.h
emcReader.o: ../../../../StRoot/RTS/include/rts.h
emcReader.o: ./evpSupport.h
emcReader.o: ./scReader.h
emcReader.o: ./trgReader.h
emcReader.o: ./tpcReader.h
emcReader.o: ./svtReader.h
emcReader.o: ./ftpReader.h
emcReader.o: ./ricReader.h
emcReader.o: ./tofReader.h
emcReader.o: ./fpdReader.h
emcReader.o: ./pmdReader.h
emcReader.o: ./emcReader.h
emcReader.o: ./l3Reader.h
emcReader.o: ./ssdReader.h
emcReader.o: cfgutil.h
emcReader.o: ../SFS/sfs_index.h
emcReader.o: ../SFS/fs_index.h
emcReader.o: ../SFS/sfs_base.h
emcReader.o: ../RTS_READER/rts_reader.h
emcReader.o: ./pp2ppReader.h
emcReader.o: evpReaderClass.h
emcReader.o: ../../../../StRoot/RTS/include/iccp2k.h
evpReader.o: ./evpReader.hh
evpReader.o: ../../../../StRoot/RTS/include/evp.h
evpReader.o: ../../../../StRoot/RTS/include/rts.h
evpReader.o: ./evpSupport.h
evpReader.o: ./scReader.h
evpReader.o: ./trgReader.h
evpReader.o: ./tpcReader.h
evpReader.o: ./svtReader.h
evpReader.o: ./ftpReader.h
evpReader.o: ./ricReader.h
evpReader.o: ./tofReader.h
evpReader.o: ./fpdReader.h
evpReader.o: ./pmdReader.h
evpReader.o: ../../../../StRoot/RTS/include/daqFormats.h
evpReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
evpReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
evpReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
evpReader.o: ./emcReader.h
evpReader.o: ./l3Reader.h
evpReader.o: ./ssdReader.h
evpReader.o: cfgutil.h
evpReader.o: ../SFS/sfs_index.h
evpReader.o: ../SFS/fs_index.h
evpReader.o: ../SFS/sfs_base.h
evpReader.o: ../RTS_READER/rts_reader.h
evpReader.o: ./pp2ppReader.h
evpReader.o: evpReaderClass.h
evpReader.o: ../../../../StRoot/RTS/include/iccp2k.h
evpReader.o: ../../../../StRoot/RTS/include/rtsLog.h
evpReader.o: ../../../../StRoot/RTS/include/iccp.h
evpReader.o: ../../../../StRoot/RTS/include/cmds.h
evpReader.o: ../../../../StRoot/RTS/include/rtsCmds.h
evpReader.o: ../../../../StRoot/RTS/include/tasks.h
evpReader.o: ../../../../StRoot/RTS/include/status.h
evpReader.o: ../../../../StRoot/RTS/include/iccpHeader.h
evpReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
evpReader.o: ./msgNQLib.h
evpSupport.o: ../../../../StRoot/RTS/include/rtsLog.h
evpSupport.o: ./evpSupport.h
evpSupport.o: ../../../../StRoot/RTS/include/rts.h
fpdReader.o: ../../../../StRoot/RTS/include/rtsLog.h
fpdReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
fpdReader.o: ../../../../StRoot/RTS/include/daqFormats.h
fpdReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
fpdReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
fpdReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
fpdReader.o: ./evpReader.hh
fpdReader.o: ../../../../StRoot/RTS/include/evp.h
fpdReader.o: ../../../../StRoot/RTS/include/rts.h
fpdReader.o: ./evpSupport.h
fpdReader.o: ./scReader.h
fpdReader.o: ./trgReader.h
fpdReader.o: ./tpcReader.h
fpdReader.o: ./svtReader.h
fpdReader.o: ./ftpReader.h
fpdReader.o: ./ricReader.h
fpdReader.o: ./tofReader.h
fpdReader.o: ./fpdReader.h
fpdReader.o: ./pmdReader.h
fpdReader.o: ./emcReader.h
fpdReader.o: ./l3Reader.h
fpdReader.o: ./ssdReader.h
fpdReader.o: cfgutil.h
fpdReader.o: ../SFS/sfs_index.h
fpdReader.o: ../SFS/fs_index.h
fpdReader.o: ../SFS/sfs_base.h
fpdReader.o: ../RTS_READER/rts_reader.h
fpdReader.o: ./pp2ppReader.h
fpdReader.o: evpReaderClass.h
fpdReader.o: ../../../../StRoot/RTS/include/iccp2k.h
ftpReader.o: ../../../../StRoot/RTS/include/daqFormats.h
ftpReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ftpReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
ftpReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ftpReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
ftpReader.o: ../../../../StRoot/RTS/include/rtsLog.h
ftpReader.o: ./evpReader.hh
ftpReader.o: ../../../../StRoot/RTS/include/evp.h
ftpReader.o: ../../../../StRoot/RTS/include/rts.h
ftpReader.o: ./evpSupport.h
ftpReader.o: ./scReader.h
ftpReader.o: ./trgReader.h
ftpReader.o: ./tpcReader.h
ftpReader.o: ./svtReader.h
ftpReader.o: ./ftpReader.h
ftpReader.o: ./ricReader.h
ftpReader.o: ./tofReader.h
ftpReader.o: ./fpdReader.h
ftpReader.o: ./pmdReader.h
ftpReader.o: ./emcReader.h
ftpReader.o: ./l3Reader.h
ftpReader.o: ./ssdReader.h
ftpReader.o: cfgutil.h
ftpReader.o: ../SFS/sfs_index.h
ftpReader.o: ../SFS/fs_index.h
ftpReader.o: ../SFS/sfs_base.h
ftpReader.o: ../RTS_READER/rts_reader.h
ftpReader.o: ./pp2ppReader.h
ftpReader.o: evpReaderClass.h
ftpReader.o: ../../../../StRoot/RTS/include/iccp2k.h
l3Reader.o: ../../../../StRoot/RTS/include/rtsLog.h
l3Reader.o: ../../../../StRoot/RTS/include/rtsSystems.h
l3Reader.o: ./evpReader.hh
l3Reader.o: ../../../../StRoot/RTS/include/evp.h
l3Reader.o: ../../../../StRoot/RTS/include/rts.h
l3Reader.o: ./evpSupport.h
l3Reader.o: ./scReader.h
l3Reader.o: ./trgReader.h
l3Reader.o: ./tpcReader.h
l3Reader.o: ./svtReader.h
l3Reader.o: ./ftpReader.h
l3Reader.o: ./ricReader.h
l3Reader.o: ./tofReader.h
l3Reader.o: ./fpdReader.h
l3Reader.o: ./pmdReader.h
l3Reader.o: ../../../../StRoot/RTS/include/daqFormats.h
l3Reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
l3Reader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
l3Reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
l3Reader.o: ./emcReader.h
l3Reader.o: ./l3Reader.h
l3Reader.o: ./ssdReader.h
l3Reader.o: cfgutil.h
l3Reader.o: ../SFS/sfs_index.h
l3Reader.o: ../SFS/fs_index.h
l3Reader.o: ../SFS/sfs_base.h
l3Reader.o: ../RTS_READER/rts_reader.h
l3Reader.o: ./pp2ppReader.h
l3Reader.o: evpReaderClass.h
l3Reader.o: ../../../../StRoot/RTS/include/iccp2k.h
msgNQLib.o: ../../../../StRoot/RTS/include/rtsLog.h
msgNQLib.o: ./msgNQLib.h
pmdReader.o: ../../../../StRoot/RTS/include/rtsLog.h
pmdReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
pmdReader.o: ../../../../StRoot/RTS/include/daqFormats.h
pmdReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pmdReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
pmdReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pmdReader.o: ./evpReader.hh
pmdReader.o: ../../../../StRoot/RTS/include/evp.h
pmdReader.o: ../../../../StRoot/RTS/include/rts.h
pmdReader.o: ./evpSupport.h
pmdReader.o: ./scReader.h
pmdReader.o: ./trgReader.h
pmdReader.o: ./tpcReader.h
pmdReader.o: ./svtReader.h
pmdReader.o: ./ftpReader.h
pmdReader.o: ./ricReader.h
pmdReader.o: ./tofReader.h
pmdReader.o: ./fpdReader.h
pmdReader.o: ./pmdReader.h
pmdReader.o: ./emcReader.h
pmdReader.o: ./l3Reader.h
pmdReader.o: ./ssdReader.h
pmdReader.o: cfgutil.h
pmdReader.o: ../SFS/sfs_index.h
pmdReader.o: ../SFS/fs_index.h
pmdReader.o: ../SFS/sfs_base.h
pmdReader.o: ../RTS_READER/rts_reader.h
pmdReader.o: ./pp2ppReader.h
pmdReader.o: evpReaderClass.h
pmdReader.o: ../../../../StRoot/RTS/include/iccp2k.h
pp2ppReader.o: ../../../../StRoot/RTS/include/rtsLog.h
pp2ppReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
pp2ppReader.o: ../../../../StRoot/RTS/include/daqFormats.h
pp2ppReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pp2ppReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
pp2ppReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pp2ppReader.o: ./evpReader.hh
pp2ppReader.o: ../../../../StRoot/RTS/include/evp.h
pp2ppReader.o: ../../../../StRoot/RTS/include/rts.h
pp2ppReader.o: ./evpSupport.h
pp2ppReader.o: ./scReader.h
pp2ppReader.o: ./trgReader.h
pp2ppReader.o: ./tpcReader.h
pp2ppReader.o: ./svtReader.h
pp2ppReader.o: ./ftpReader.h
pp2ppReader.o: ./ricReader.h
pp2ppReader.o: ./tofReader.h
pp2ppReader.o: ./fpdReader.h
pp2ppReader.o: ./pmdReader.h
pp2ppReader.o: ./emcReader.h
pp2ppReader.o: ./l3Reader.h
pp2ppReader.o: ./ssdReader.h
pp2ppReader.o: cfgutil.h
pp2ppReader.o: ../SFS/sfs_index.h
pp2ppReader.o: ../SFS/fs_index.h
pp2ppReader.o: ../SFS/sfs_base.h
pp2ppReader.o: ../RTS_READER/rts_reader.h
pp2ppReader.o: ./pp2ppReader.h
pp2ppReader.o: evpReaderClass.h
pp2ppReader.o: ../../../../StRoot/RTS/include/iccp2k.h
ricReader.o: ../../../../StRoot/RTS/include/rtsLog.h
ricReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
ricReader.o: ../../../../StRoot/RTS/include/daqFormats.h
ricReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ricReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
ricReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ricReader.o: ./evpReader.hh
ricReader.o: ../../../../StRoot/RTS/include/evp.h
ricReader.o: ../../../../StRoot/RTS/include/rts.h
ricReader.o: ./evpSupport.h
ricReader.o: ./scReader.h
ricReader.o: ./trgReader.h
ricReader.o: ./tpcReader.h
ricReader.o: ./svtReader.h
ricReader.o: ./ftpReader.h
ricReader.o: ./ricReader.h
ricReader.o: ./tofReader.h
ricReader.o: ./fpdReader.h
ricReader.o: ./pmdReader.h
ricReader.o: ./emcReader.h
ricReader.o: ./l3Reader.h
ricReader.o: ./ssdReader.h
ricReader.o: cfgutil.h
ricReader.o: ../SFS/sfs_index.h
ricReader.o: ../SFS/fs_index.h
ricReader.o: ../SFS/sfs_base.h
ricReader.o: ../RTS_READER/rts_reader.h
ricReader.o: ./pp2ppReader.h
ricReader.o: evpReaderClass.h
ricReader.o: ../../../../StRoot/RTS/include/iccp2k.h
sanityCheckers.o: ./evpReader.hh
sanityCheckers.o: ../../../../StRoot/RTS/include/evp.h
sanityCheckers.o: ../../../../StRoot/RTS/include/rts.h
sanityCheckers.o: ./evpSupport.h
sanityCheckers.o: ./scReader.h
sanityCheckers.o: ./trgReader.h
sanityCheckers.o: ./tpcReader.h
sanityCheckers.o: ./svtReader.h
sanityCheckers.o: ./ftpReader.h
sanityCheckers.o: ./ricReader.h
sanityCheckers.o: ./tofReader.h
sanityCheckers.o: ./fpdReader.h
sanityCheckers.o: ./pmdReader.h
sanityCheckers.o: ../../../../StRoot/RTS/include/daqFormats.h
sanityCheckers.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
sanityCheckers.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
sanityCheckers.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
sanityCheckers.o: ./emcReader.h
sanityCheckers.o: ./l3Reader.h
sanityCheckers.o: ./ssdReader.h
sanityCheckers.o: cfgutil.h
sanityCheckers.o: ../SFS/sfs_index.h
sanityCheckers.o: ../SFS/fs_index.h
sanityCheckers.o: ../SFS/sfs_base.h
sanityCheckers.o: ../RTS_READER/rts_reader.h
sanityCheckers.o: ./pp2ppReader.h
sanityCheckers.o: evpReaderClass.h
sanityCheckers.o: ../../../../StRoot/RTS/include/iccp2k.h
sanityCheckers.o: ../../../../StRoot/RTS/include/rtsLog.h
sanityCheckers.o: ../../../../StRoot/RTS/include/rtsSystems.h
scReader.o: ../../../../StRoot/RTS/include/rtsLog.h
scReader.o: ../../../../StRoot/RTS/include/daqFormats.h
scReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
scReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
scReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
scReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
scReader.o: ./evpReader.hh
scReader.o: ../../../../StRoot/RTS/include/evp.h
scReader.o: ../../../../StRoot/RTS/include/rts.h
scReader.o: ./evpSupport.h
scReader.o: ./scReader.h
scReader.o: ./trgReader.h
scReader.o: ./tpcReader.h
scReader.o: ./svtReader.h
scReader.o: ./ftpReader.h
scReader.o: ./ricReader.h
scReader.o: ./tofReader.h
scReader.o: ./fpdReader.h
scReader.o: ./pmdReader.h
scReader.o: ./emcReader.h
scReader.o: ./l3Reader.h
scReader.o: ./ssdReader.h
scReader.o: cfgutil.h
scReader.o: ../SFS/sfs_index.h
scReader.o: ../SFS/fs_index.h
scReader.o: ../SFS/sfs_base.h
scReader.o: ../RTS_READER/rts_reader.h
scReader.o: ./pp2ppReader.h
scReader.o: evpReaderClass.h
scReader.o: ../../../../StRoot/RTS/include/iccp2k.h
ssdReader.o: ../../../../StRoot/RTS/include/daqFormats.h
ssdReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ssdReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
ssdReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ssdReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
ssdReader.o: ../../../../StRoot/RTS/include/rtsLog.h
ssdReader.o: ./evpReader.hh
ssdReader.o: ../../../../StRoot/RTS/include/evp.h
ssdReader.o: ../../../../StRoot/RTS/include/rts.h
ssdReader.o: ./evpSupport.h
ssdReader.o: ./scReader.h
ssdReader.o: ./trgReader.h
ssdReader.o: ./tpcReader.h
ssdReader.o: ./svtReader.h
ssdReader.o: ./ftpReader.h
ssdReader.o: ./ricReader.h
ssdReader.o: ./tofReader.h
ssdReader.o: ./fpdReader.h
ssdReader.o: ./pmdReader.h
ssdReader.o: ./emcReader.h
ssdReader.o: ./l3Reader.h
ssdReader.o: ./ssdReader.h
ssdReader.o: cfgutil.h
ssdReader.o: ../SFS/sfs_index.h
ssdReader.o: ../SFS/fs_index.h
ssdReader.o: ../SFS/sfs_base.h
ssdReader.o: ../RTS_READER/rts_reader.h
ssdReader.o: ./pp2ppReader.h
ssdReader.o: evpReaderClass.h
ssdReader.o: ../../../../StRoot/RTS/include/iccp2k.h
svtReader.o: ../../../../StRoot/RTS/include/daqFormats.h
svtReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
svtReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
svtReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
svtReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
svtReader.o: ../../../../StRoot/RTS/include/rtsLog.h
svtReader.o: ../../../../StRoot/RTS/include/SVT/key_map.h
svtReader.o: ./evpReader.hh
svtReader.o: ../../../../StRoot/RTS/include/evp.h
svtReader.o: ../../../../StRoot/RTS/include/rts.h
svtReader.o: ./evpSupport.h
svtReader.o: ./scReader.h
svtReader.o: ./trgReader.h
svtReader.o: ./tpcReader.h
svtReader.o: ./svtReader.h
svtReader.o: ./ftpReader.h
svtReader.o: ./ricReader.h
svtReader.o: ./tofReader.h
svtReader.o: ./fpdReader.h
svtReader.o: ./pmdReader.h
svtReader.o: ./emcReader.h
svtReader.o: ./l3Reader.h
svtReader.o: ./ssdReader.h
svtReader.o: cfgutil.h
svtReader.o: ../SFS/sfs_index.h
svtReader.o: ../SFS/fs_index.h
svtReader.o: ../SFS/sfs_base.h
svtReader.o: ../RTS_READER/rts_reader.h
svtReader.o: ./pp2ppReader.h
svtReader.o: evpReaderClass.h
svtReader.o: ../../../../StRoot/RTS/include/iccp2k.h
tofReader.o: ../../../../StRoot/RTS/include/rtsLog.h
tofReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tofReader.o: ../../../../StRoot/RTS/include/daqFormats.h
tofReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tofReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
tofReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tofReader.o: ./evpReader.hh
tofReader.o: ../../../../StRoot/RTS/include/evp.h
tofReader.o: ../../../../StRoot/RTS/include/rts.h
tofReader.o: ./evpSupport.h
tofReader.o: ./scReader.h
tofReader.o: ./trgReader.h
tofReader.o: ./tpcReader.h
tofReader.o: ./svtReader.h
tofReader.o: ./ftpReader.h
tofReader.o: ./ricReader.h
tofReader.o: ./tofReader.h
tofReader.o: ./fpdReader.h
tofReader.o: ./pmdReader.h
tofReader.o: ./emcReader.h
tofReader.o: ./l3Reader.h
tofReader.o: ./ssdReader.h
tofReader.o: cfgutil.h
tofReader.o: ../SFS/sfs_index.h
tofReader.o: ../SFS/fs_index.h
tofReader.o: ../SFS/sfs_base.h
tofReader.o: ../RTS_READER/rts_reader.h
tofReader.o: ./pp2ppReader.h
tofReader.o: evpReaderClass.h
tofReader.o: ../../../../StRoot/RTS/include/iccp2k.h
tofReader.o: ../RTS_READER/daq_dta.h
tofReader.o: ../RTS_READER/daq_dta_structs.h
tofReader.o: ../RTS_READER/daq_det.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/daqFormats.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tpcFCFReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/rtsLog.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/fcfClass.hh
tpcFCFReader.o: ./evpSupport.h
tpcFCFReader.o: ./tpcReader.h
tpcFCFReader.o: ../../include/TPC/padfinder.h
tpcFCFReader.o: ../../include/TPC/rowlen.h
tpcFCFReader.o: ../../../../StRoot/RTS/include/adcLogTable.h
tpcReader.o: ../../../../StRoot/RTS/include/daqFormats.h
tpcReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
tpcReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
tpcReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
tpcReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
tpcReader.o: ../../../../StRoot/RTS/include/rtsLog.h
tpcReader.o: ../../../../StRoot/RTS/include/fcfClass.hh
tpcReader.o: ./evpReader.hh
tpcReader.o: ../../../../StRoot/RTS/include/evp.h
tpcReader.o: ../../../../StRoot/RTS/include/rts.h
tpcReader.o: ./evpSupport.h
tpcReader.o: ./scReader.h
tpcReader.o: ./trgReader.h
tpcReader.o: ./tpcReader.h
tpcReader.o: ./svtReader.h
tpcReader.o: ./ftpReader.h
tpcReader.o: ./ricReader.h
tpcReader.o: ./tofReader.h
tpcReader.o: ./fpdReader.h
tpcReader.o: ./pmdReader.h
tpcReader.o: ./emcReader.h
tpcReader.o: ./l3Reader.h
tpcReader.o: ./ssdReader.h
tpcReader.o: cfgutil.h
tpcReader.o: ../SFS/sfs_index.h
tpcReader.o: ../SFS/fs_index.h
tpcReader.o: ../SFS/sfs_base.h
tpcReader.o: ../RTS_READER/rts_reader.h
tpcReader.o: ./pp2ppReader.h
tpcReader.o: evpReaderClass.h
tpcReader.o: ../../../../StRoot/RTS/include/iccp2k.h
tpcReader.o: ../../../../StRoot/RTS/include/TPC/offsets.h
tpcReader.o: ../../../../StRoot/RTS/include/TPC/trans_table.hh
tpcReader.o: ../RTS_READER/daq_dta.h
tpcReader.o: ../RTS_READER/daq_dta_structs.h
tpcReader.o: ../RTS_READER/daq_det.h
tpcReader.o: ../DAQ_TPX/daq_tpx.h
trgReader10.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader10.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader10.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader10.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader10.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader10.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader10.o: ./evpSupport.h
trgReader10.o: ../../../../StRoot/RTS/include/rts.h
trgReader12.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader12.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader12.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader12.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader12.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader12.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader12.o: ./evpSupport.h
trgReader12.o: ./trgReader.h
trgReader20.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader20.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader20.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader20.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader20.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader20.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader20.o: ./evpSupport.h
trgReader20.o: ./trgReader.h
trgReader20.o: ../../../../StRoot/RTS/include/rts.h
trgReader21.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader21.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader21.o: ../../../../StRoot/RTS/include/rts.h
trgReader21.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader21.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader21.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader21.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader21.o: ./evpSupport.h
trgReader21.o: ./trgReader.h
trgReader22.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader22.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader22.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader22.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader22.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader22.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader22.o: ../../../../StRoot/RTS/include/rts.h
trgReader22.o: ./evpSupport.h
trgReader22.o: ./trgReader.h
trgReader30.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader30.o: ../../../../StRoot/RTS/include/rts.h
trgReader30.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader30.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader30.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader30.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader30.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader30.o: ./evpSupport.h
trgReader30.o: ./trgReader.h
trgReader32.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader32.o: ../../../../StRoot/RTS/include/rts.h
trgReader32.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader32.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader32.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader32.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader32.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader32.o: ./evpSupport.h
trgReader32.o: ./trgReader.h
trgReader.o: ../../../../StRoot/RTS/include/rtsLog.h
trgReader.o: ../../../../StRoot/RTS/include/daqFormats.h
trgReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
trgReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
trgReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
trgReader.o: ../../../../StRoot/RTS/include/rtsSystems.h
trgReader.o: ./evpReader.hh
trgReader.o: ../../../../StRoot/RTS/include/evp.h
trgReader.o: ../../../../StRoot/RTS/include/rts.h
trgReader.o: ./evpSupport.h
trgReader.o: ./scReader.h
trgReader.o: ./trgReader.h
trgReader.o: ./tpcReader.h
trgReader.o: ./svtReader.h
trgReader.o: ./ftpReader.h
trgReader.o: ./ricReader.h
trgReader.o: ./tofReader.h
trgReader.o: ./fpdReader.h
trgReader.o: ./pmdReader.h
trgReader.o: ./emcReader.h
trgReader.o: ./l3Reader.h
trgReader.o: ./ssdReader.h
trgReader.o: cfgutil.h
trgReader.o: ../SFS/sfs_index.h
trgReader.o: ../SFS/fs_index.h
trgReader.o: ../SFS/sfs_base.h
trgReader.o: ../RTS_READER/rts_reader.h
trgReader.o: ./pp2ppReader.h
trgReader.o: evpReaderClass.h
trgReader.o: ../../../../StRoot/RTS/include/iccp2k.h
grpRates.o: ../../../../StRoot/RTS/include/SVT/key_map.h
grpRates.o: ../../../../StRoot/RTS/include/rtsLog.h
grpRates.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
grpRates.o: ../../../../StRoot/RTS/include/TPC/trans_table.hh
grpRates.o: ./evpReader.hh
grpRates.o: ../../../../StRoot/RTS/include/evp.h
grpRates.o: ../../../../StRoot/RTS/include/rts.h
grpRates.o: ./evpSupport.h
grpRates.o: ./scReader.h
grpRates.o: ./trgReader.h
grpRates.o: ./tpcReader.h
grpRates.o: ./svtReader.h
grpRates.o: ./ftpReader.h
grpRates.o: ./ricReader.h
grpRates.o: ./tofReader.h
grpRates.o: ./fpdReader.h
grpRates.o: ./pmdReader.h
grpRates.o: ../../../../StRoot/RTS/include/daqFormats.h
grpRates.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
grpRates.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
grpRates.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
grpRates.o: ./emcReader.h
grpRates.o: ./l3Reader.h
grpRates.o: ./ssdReader.h
grpRates.o: cfgutil.h
grpRates.o: ../SFS/sfs_index.h
grpRates.o: ../SFS/fs_index.h
grpRates.o: ../SFS/sfs_base.h
grpRates.o: ../RTS_READER/rts_reader.h
grpRates.o: ./pp2ppReader.h
grpRates.o: evpReaderClass.h
grpRates.o: ../../../../StRoot/RTS/include/iccp2k.h
grpRates.o: ../../../../StRoot/RTS/include/rtsSystems.h
ppreader.o: ../../../../StRoot/RTS/include/rtsLog.h
ppreader.o: ./evpReader.hh
ppreader.o: ../../../../StRoot/RTS/include/evp.h
ppreader.o: ../../../../StRoot/RTS/include/rts.h
ppreader.o: ./evpSupport.h
ppreader.o: ./scReader.h
ppreader.o: ./trgReader.h
ppreader.o: ./tpcReader.h
ppreader.o: ./svtReader.h
ppreader.o: ./ftpReader.h
ppreader.o: ./ricReader.h
ppreader.o: ./tofReader.h
ppreader.o: ./fpdReader.h
ppreader.o: ./pmdReader.h
ppreader.o: ../../../../StRoot/RTS/include/daqFormats.h
ppreader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
ppreader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
ppreader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
ppreader.o: ./emcReader.h
ppreader.o: ./l3Reader.h
ppreader.o: ./ssdReader.h
ppreader.o: cfgutil.h
ppreader.o: ../SFS/sfs_index.h
ppreader.o: ../SFS/fs_index.h
ppreader.o: ../SFS/sfs_base.h
ppreader.o: ../RTS_READER/rts_reader.h
ppreader.o: ./pp2ppReader.h
ppreader.o: evpReaderClass.h
ppreader.o: ../../../../StRoot/RTS/include/iccp2k.h
ppreader.o: ../../../../StRoot/RTS/include/rtsSystems.h
special.o: ../../../../StRoot/RTS/include/rtsLog.h
special.o: ../../../../StRoot/RTS/include/TPC/rowlen.h
special.o: ../../../../StRoot/RTS/include/TPC/trans_table.hh
special.o: ./evpReader.hh
special.o: ../../../../StRoot/RTS/include/evp.h
special.o: ../../../../StRoot/RTS/include/rts.h
special.o: ./evpSupport.h
special.o: ./scReader.h
special.o: ./trgReader.h
special.o: ./tpcReader.h
special.o: ./svtReader.h
special.o: ./ftpReader.h
special.o: ./ricReader.h
special.o: ./tofReader.h
special.o: ./fpdReader.h
special.o: ./pmdReader.h
special.o: ../../../../StRoot/RTS/include/daqFormats.h
special.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
special.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
special.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
special.o: ./emcReader.h
special.o: ./l3Reader.h
special.o: ./ssdReader.h
special.o: cfgutil.h
special.o: ../SFS/sfs_index.h
special.o: ../SFS/fs_index.h
special.o: ../SFS/sfs_base.h
special.o: ../RTS_READER/rts_reader.h
special.o: ./pp2ppReader.h
special.o: evpReaderClass.h
special.o: ../../../../StRoot/RTS/include/iccp2k.h
special.o: ../../../../StRoot/RTS/include/rtsSystems.h
evpReaderClass.o: ../../../../StRoot/RTS/include/evp.h
evpReaderClass.o: ../../../../StRoot/RTS/include/rts.h
evpReaderClass.o: cfgutil.h
evpReaderClass.o: ../../../../StRoot/RTS/include/iccp2k.h
l3Reader.o: ./evpSupport.h
l3Reader.o: ../../../../StRoot/RTS/include/daqFormats.h
l3Reader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
l3Reader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
l3Reader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pmdReader.o: ../../../../StRoot/RTS/include/daqFormats.h
pmdReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pmdReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
pmdReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
pp2ppReader.o: ../../../../StRoot/RTS/include/daqFormats.h
pp2ppReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
pp2ppReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
pp2ppReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
evpReader.o: ../../../../StRoot/RTS/include/evp.h
evpReader.o: ../../../../StRoot/RTS/include/rts.h
evpReader.o: ./evpSupport.h
evpReader.o: ./scReader.h
evpReader.o: ./trgReader.h
evpReader.o: ./tpcReader.h
evpReader.o: ./svtReader.h
evpReader.o: ./ftpReader.h
evpReader.o: ./ricReader.h
evpReader.o: ./tofReader.h
evpReader.o: ./fpdReader.h
evpReader.o: ./pmdReader.h
evpReader.o: ../../../../StRoot/RTS/include/daqFormats.h
evpReader.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
evpReader.o: ../../../../StRoot/RTS/trg/include/trgStructures.h
evpReader.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
evpReader.o: ./emcReader.h
evpReader.o: ./l3Reader.h
evpReader.o: ./ssdReader.h
evpReader.o: cfgutil.h
evpReader.o: ../SFS/sfs_index.h
evpReader.o: ../SFS/fs_index.h
evpReader.o: ../SFS/sfs_base.h
evpReader.o: ../RTS_READER/rts_reader.h
evpReader.o: ./pp2ppReader.h
evpReader.o: evpReaderClass.h
evpReader.o: ../../../../StRoot/RTS/include/iccp2k.h
