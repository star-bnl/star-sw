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
CXXFLAGS +=  -g -I../../include/TPC -DEVP_VERSION=\"EVP_VERSION=${VERSION}\" -I..


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
	daq_tpx.o rts_reader.o daq_det.o daq_dta.o daq_tof.o tpxCore.o tpxFCF.o tpxGain.o tpxPed.o \
	daq_pp2pp.o  daq_esmd.o emc_reader.o daq_tpc.o tpc_reader.o daq_legacy.o
VPATH = ../RTS_READER ../DAQ_TOF ../DAQ_TPX ../DAQ_PP2PP ../DAQ_ESMD ../DAQ_TPC ../DAQ_LEGACY ../LOG ../SFS ../FCF

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

cfgutil.o: ../../include/RC_Config.h
cfgutil.o: ../../include/rtsSystems.h
cfgutil.o: ../../include/rts.h
cfgutil.o: ../../include/rtsLog.h
cfgutil.o: cfgutil.h
emcReader.o: ../../include/rtsLog.h
emcReader.o: ../../include/rtsSystems.h
emcReader.o: ../../include/daqFormats.h
emcReader.o: ../../include/SECTOR/sector.h
emcReader.o: ../../trg/include/trgStructures.h
emcReader.o: ../../include/L3/L3Formats.h
emcReader.o: ./evpReader.hh
emcReader.o: ../../include/evp.h
emcReader.o: ../../include/rts.h
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
emcReader.o: ./../SFS/sfs_index.h
emcReader.o: ./../SFS/fs_index.h
emcReader.o: ./../RTS_READER/rts_reader.h
emcReader.o: ../RTS_READER/daq_det.h
emcReader.o: ../RTS_READER/rts_reader.h
emcReader.o: ../RTS_READER/daq_dta.h
emcReader.o: ./pp2ppReader.h
evpReader.o: ./evpReader.hh
evpReader.o: ../../include/evp.h
evpReader.o: ../../include/rts.h
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
evpReader.o: ../../include/daqFormats.h
evpReader.o: ../../include/SECTOR/sector.h
evpReader.o: ../../trg/include/trgStructures.h
evpReader.o: ../../include/L3/L3Formats.h
evpReader.o: ./emcReader.h
evpReader.o: ./l3Reader.h
evpReader.o: ./ssdReader.h
evpReader.o: cfgutil.h
evpReader.o: ./../SFS/sfs_index.h
evpReader.o: ./../SFS/fs_index.h
evpReader.o: ./../RTS_READER/rts_reader.h
evpReader.o: ../../include/rtsLog.h
evpReader.o: ../../include/rtsSystems.h
evpReader.o: ../RTS_READER/daq_det.h
evpReader.o: ../RTS_READER/rts_reader.h
evpReader.o: ../RTS_READER/daq_dta.h
evpReader.o: ./pp2ppReader.h
evpReader.o: ../../include/iccp.h
evpReader.o: ../../include/cmds.h
evpReader.o: ../../include/rtsCmds.h
evpReader.o: ../../include/tasks.h
evpReader.o: ../../include/status.h
evpReader.o: ../../include/iccpHeader.h
evpReader.o: ./msgNQLib.h
evpSupport.o: ../../include/rtsLog.h
evpSupport.o: ./evpSupport.h
evpSupport.o: ../../include/rts.h
fpdReader.o: ../../include/rtsLog.h
fpdReader.o: ../../include/rtsSystems.h
fpdReader.o: ../../include/daqFormats.h
fpdReader.o: ../../include/SECTOR/sector.h
fpdReader.o: ../../trg/include/trgStructures.h
fpdReader.o: ../../include/L3/L3Formats.h
fpdReader.o: ./evpReader.hh
fpdReader.o: ../../include/evp.h
fpdReader.o: ../../include/rts.h
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
fpdReader.o: ./../SFS/sfs_index.h
fpdReader.o: ./../SFS/fs_index.h
fpdReader.o: ./../RTS_READER/rts_reader.h
fpdReader.o: ../RTS_READER/daq_det.h
fpdReader.o: ../RTS_READER/rts_reader.h
fpdReader.o: ../RTS_READER/daq_dta.h
fpdReader.o: ./pp2ppReader.h
ftpReader.o: ../../include/daqFormats.h
ftpReader.o: ../../include/SECTOR/sector.h
ftpReader.o: ../../trg/include/trgStructures.h
ftpReader.o: ../../include/L3/L3Formats.h
ftpReader.o: ../../include/rtsSystems.h
ftpReader.o: ../../include/rtsLog.h
ftpReader.o: ./evpReader.hh
ftpReader.o: ../../include/evp.h
ftpReader.o: ../../include/rts.h
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
ftpReader.o: ./../SFS/sfs_index.h
ftpReader.o: ./../SFS/fs_index.h
ftpReader.o: ./../RTS_READER/rts_reader.h
ftpReader.o: ../RTS_READER/daq_det.h
ftpReader.o: ../RTS_READER/rts_reader.h
ftpReader.o: ../RTS_READER/daq_dta.h
ftpReader.o: ./pp2ppReader.h
grpRates.o: ../../include/SVT/key_map.h
grpRates.o: ../../include/rtsLog.h
grpRates.o: ../../include/TPC/rowlen.h
grpRates.o: ../../include/TPC/trans_table.hh
grpRates.o: ./evpReader.hh
grpRates.o: ../../include/evp.h
grpRates.o: ../../include/rts.h
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
grpRates.o: ../../include/daqFormats.h
grpRates.o: ../../include/SECTOR/sector.h
grpRates.o: ../../trg/include/trgStructures.h
grpRates.o: ../../include/L3/L3Formats.h
grpRates.o: ./emcReader.h
grpRates.o: ./l3Reader.h
grpRates.o: ./ssdReader.h
grpRates.o: cfgutil.h
grpRates.o: ./../SFS/sfs_index.h
grpRates.o: ./../SFS/fs_index.h
grpRates.o: ./../RTS_READER/rts_reader.h
grpRates.o: ../../include/rtsSystems.h
grpRates.o: ../RTS_READER/daq_det.h
grpRates.o: ../RTS_READER/rts_reader.h
grpRates.o: ../RTS_READER/daq_dta.h
grpRates.o: ./pp2ppReader.h
l3Reader.o: ../../include/rtsLog.h
l3Reader.o: ../../include/rtsSystems.h
l3Reader.o: ./evpReader.hh
l3Reader.o: ../../include/evp.h
l3Reader.o: ../../include/rts.h
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
l3Reader.o: ../../include/daqFormats.h
l3Reader.o: ../../include/SECTOR/sector.h
l3Reader.o: ../../trg/include/trgStructures.h
l3Reader.o: ../../include/L3/L3Formats.h
l3Reader.o: ./emcReader.h
l3Reader.o: ./l3Reader.h
l3Reader.o: ./ssdReader.h
l3Reader.o: cfgutil.h
l3Reader.o: ./../SFS/sfs_index.h
l3Reader.o: ./../SFS/fs_index.h
l3Reader.o: ./../RTS_READER/rts_reader.h
l3Reader.o: ../RTS_READER/daq_det.h
l3Reader.o: ../RTS_READER/rts_reader.h
l3Reader.o: ../RTS_READER/daq_dta.h
l3Reader.o: ./pp2ppReader.h
msgNQLib.o: ../../include/rtsLog.h
msgNQLib.o: ./msgNQLib.h
pmdReader.o: ../../include/rtsLog.h
pmdReader.o: ../../include/rtsSystems.h
pmdReader.o: ../../include/daqFormats.h
pmdReader.o: ../../include/SECTOR/sector.h
pmdReader.o: ../../trg/include/trgStructures.h
pmdReader.o: ../../include/L3/L3Formats.h
pmdReader.o: ./evpReader.hh
pmdReader.o: ../../include/evp.h
pmdReader.o: ../../include/rts.h
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
pmdReader.o: ./../SFS/sfs_index.h
pmdReader.o: ./../SFS/fs_index.h
pmdReader.o: ./../RTS_READER/rts_reader.h
pmdReader.o: ../RTS_READER/daq_det.h
pmdReader.o: ../RTS_READER/rts_reader.h
pmdReader.o: ../RTS_READER/daq_dta.h
pmdReader.o: ./pp2ppReader.h
pp2ppReader.o: ../../include/rtsLog.h
pp2ppReader.o: ../../include/rtsSystems.h
pp2ppReader.o: ../../include/daqFormats.h
pp2ppReader.o: ../../include/SECTOR/sector.h
pp2ppReader.o: ../../trg/include/trgStructures.h
pp2ppReader.o: ../../include/L3/L3Formats.h
pp2ppReader.o: ./evpReader.hh
pp2ppReader.o: ../../include/evp.h
pp2ppReader.o: ../../include/rts.h
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
pp2ppReader.o: ./../SFS/sfs_index.h
pp2ppReader.o: ./../SFS/fs_index.h
pp2ppReader.o: ./../RTS_READER/rts_reader.h
pp2ppReader.o: ../RTS_READER/daq_det.h
pp2ppReader.o: ../RTS_READER/rts_reader.h
pp2ppReader.o: ../RTS_READER/daq_dta.h
pp2ppReader.o: ./pp2ppReader.h
ppreader.o: ../../include/rtsLog.h
ppreader.o: ./evpReader.hh
ppreader.o: ../../include/evp.h
ppreader.o: ../../include/rts.h
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
ppreader.o: ../../include/daqFormats.h
ppreader.o: ../../include/SECTOR/sector.h
ppreader.o: ../../trg/include/trgStructures.h
ppreader.o: ../../include/L3/L3Formats.h
ppreader.o: ./emcReader.h
ppreader.o: ./l3Reader.h
ppreader.o: ./ssdReader.h
ppreader.o: cfgutil.h
ppreader.o: ./../SFS/sfs_index.h
ppreader.o: ./../SFS/fs_index.h
ppreader.o: ./../RTS_READER/rts_reader.h
ppreader.o: ../../include/rtsSystems.h
ppreader.o: ../RTS_READER/daq_det.h
ppreader.o: ../RTS_READER/rts_reader.h
ppreader.o: ../RTS_READER/daq_dta.h
ppreader.o: ./pp2ppReader.h
ricReader.o: ../../include/rtsLog.h
ricReader.o: ../../include/rtsSystems.h
ricReader.o: ../../include/daqFormats.h
ricReader.o: ../../include/SECTOR/sector.h
ricReader.o: ../../trg/include/trgStructures.h
ricReader.o: ../../include/L3/L3Formats.h
ricReader.o: ./evpReader.hh
ricReader.o: ../../include/evp.h
ricReader.o: ../../include/rts.h
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
ricReader.o: ./../SFS/sfs_index.h
ricReader.o: ./../SFS/fs_index.h
ricReader.o: ./../RTS_READER/rts_reader.h
ricReader.o: ../RTS_READER/daq_det.h
ricReader.o: ../RTS_READER/rts_reader.h
ricReader.o: ../RTS_READER/daq_dta.h
ricReader.o: ./pp2ppReader.h
sanityCheckers.o: ./evpReader.hh
sanityCheckers.o: ../../include/evp.h
sanityCheckers.o: ../../include/rts.h
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
sanityCheckers.o: ../../include/daqFormats.h
sanityCheckers.o: ../../include/SECTOR/sector.h
sanityCheckers.o: ../../trg/include/trgStructures.h
sanityCheckers.o: ../../include/L3/L3Formats.h
sanityCheckers.o: ./emcReader.h
sanityCheckers.o: ./l3Reader.h
sanityCheckers.o: ./ssdReader.h
sanityCheckers.o: cfgutil.h
sanityCheckers.o: ./../SFS/sfs_index.h
sanityCheckers.o: ./../SFS/fs_index.h
sanityCheckers.o: ./../RTS_READER/rts_reader.h
sanityCheckers.o: ../../include/rtsLog.h
sanityCheckers.o: ../../include/rtsSystems.h
sanityCheckers.o: ../RTS_READER/daq_det.h
sanityCheckers.o: ../RTS_READER/rts_reader.h
sanityCheckers.o: ../RTS_READER/daq_dta.h
sanityCheckers.o: ./pp2ppReader.h
scReader.o: ../../include/rtsLog.h
scReader.o: ../../include/daqFormats.h
scReader.o: ../../include/SECTOR/sector.h
scReader.o: ../../trg/include/trgStructures.h
scReader.o: ../../include/L3/L3Formats.h
scReader.o: ../../include/rtsSystems.h
scReader.o: ./evpReader.hh
scReader.o: ../../include/evp.h
scReader.o: ../../include/rts.h
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
scReader.o: ./../SFS/sfs_index.h
scReader.o: ./../SFS/fs_index.h
scReader.o: ./../RTS_READER/rts_reader.h
scReader.o: ../RTS_READER/daq_det.h
scReader.o: ../RTS_READER/rts_reader.h
scReader.o: ../RTS_READER/daq_dta.h
scReader.o: ./pp2ppReader.h
special.o: ../../include/SVT/key_map.h
special.o: ../../include/rtsLog.h
special.o: ../../include/TPC/rowlen.h
special.o: ../../include/TPC/trans_table.hh
special.o: ./evpReader.hh
special.o: ../../include/evp.h
special.o: ../../include/rts.h
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
special.o: ../../include/daqFormats.h
special.o: ../../include/SECTOR/sector.h
special.o: ../../trg/include/trgStructures.h
special.o: ../../include/L3/L3Formats.h
special.o: ./emcReader.h
special.o: ./l3Reader.h
special.o: ./ssdReader.h
special.o: cfgutil.h
special.o: ./../SFS/sfs_index.h
special.o: ./../SFS/fs_index.h
special.o: ./../RTS_READER/rts_reader.h
special.o: ../../include/rtsSystems.h
special.o: ../RTS_READER/daq_det.h
special.o: ../RTS_READER/rts_reader.h
special.o: ../RTS_READER/daq_dta.h
special.o: ./pp2ppReader.h
ssdReader.o: ../../include/daqFormats.h
ssdReader.o: ../../include/SECTOR/sector.h
ssdReader.o: ../../trg/include/trgStructures.h
ssdReader.o: ../../include/L3/L3Formats.h
ssdReader.o: ../../include/rtsSystems.h
ssdReader.o: ../../include/rtsLog.h
ssdReader.o: ./evpReader.hh
ssdReader.o: ../../include/evp.h
ssdReader.o: ../../include/rts.h
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
ssdReader.o: ./../SFS/sfs_index.h
ssdReader.o: ./../SFS/fs_index.h
ssdReader.o: ./../RTS_READER/rts_reader.h
ssdReader.o: ../RTS_READER/daq_det.h
ssdReader.o: ../RTS_READER/rts_reader.h
ssdReader.o: ../RTS_READER/daq_dta.h
ssdReader.o: ./pp2ppReader.h
svtReader.o: ../../include/daqFormats.h
svtReader.o: ../../include/SECTOR/sector.h
svtReader.o: ../../trg/include/trgStructures.h
svtReader.o: ../../include/L3/L3Formats.h
svtReader.o: ../../include/rtsSystems.h
svtReader.o: ../../include/rtsLog.h
svtReader.o: ../../include/SVT/key_map.h
svtReader.o: ./evpReader.hh
svtReader.o: ../../include/evp.h
svtReader.o: ../../include/rts.h
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
svtReader.o: ./../SFS/sfs_index.h
svtReader.o: ./../SFS/fs_index.h
svtReader.o: ./../RTS_READER/rts_reader.h
svtReader.o: ../RTS_READER/daq_det.h
svtReader.o: ../RTS_READER/rts_reader.h
svtReader.o: ../RTS_READER/daq_dta.h
svtReader.o: ./pp2ppReader.h
tofReader.o: ../../include/rtsLog.h
tofReader.o: ../../include/rtsSystems.h
tofReader.o: ../../include/daqFormats.h
tofReader.o: ../../include/SECTOR/sector.h
tofReader.o: ../../trg/include/trgStructures.h
tofReader.o: ../../include/L3/L3Formats.h
tofReader.o: ./evpReader.hh
tofReader.o: ../../include/evp.h
tofReader.o: ../../include/rts.h
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
tofReader.o: ./../SFS/sfs_index.h
tofReader.o: ./../SFS/fs_index.h
tofReader.o: ./../RTS_READER/rts_reader.h
tofReader.o: ../RTS_READER/daq_det.h
tofReader.o: ../RTS_READER/rts_reader.h
tofReader.o: ../RTS_READER/daq_dta.h
tofReader.o: ./pp2ppReader.h
tpcFCFReader.o: ../../include/daqFormats.h
tpcFCFReader.o: ../../include/SECTOR/sector.h
tpcFCFReader.o: ../../trg/include/trgStructures.h
tpcFCFReader.o: ../../include/L3/L3Formats.h
tpcFCFReader.o: ../../include/rtsSystems.h
tpcFCFReader.o: ../../include/rtsLog.h
tpcFCFReader.o: ../../include/fcfClass.hh
tpcFCFReader.o: ./evpSupport.h
tpcFCFReader.o: ../../include/rts.h
tpcFCFReader.o: ./tpcReader.h
tpcFCFReader.o: ../../include/TPC/padfinder.h
tpcFCFReader.o: ../../include/TPC/rowlen.h
tpcFCFReader.o: ../../include/adcLogTable.h
tpcReader.o: ../../include/daqFormats.h
tpcReader.o: ../../include/SECTOR/sector.h
tpcReader.o: ../../trg/include/trgStructures.h
tpcReader.o: ../../include/L3/L3Formats.h
tpcReader.o: ../../include/rtsSystems.h
tpcReader.o: ../../include/rtsLog.h
tpcReader.o: ../../include/fcfClass.hh
tpcReader.o: ./evpReader.hh
tpcReader.o: ../../include/evp.h
tpcReader.o: ../../include/rts.h
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
tpcReader.o: ./../SFS/sfs_index.h
tpcReader.o: ./../SFS/fs_index.h
tpcReader.o: ./../RTS_READER/rts_reader.h
tpcReader.o: ../RTS_READER/daq_det.h
tpcReader.o: ../RTS_READER/rts_reader.h
tpcReader.o: ../RTS_READER/daq_dta.h
tpcReader.o: ./pp2ppReader.h
tpcReader.o: ../../include/TPC/offsets.h
tpcReader.o: ../../include/TPC/trans_table.hh
tpcReader.o: ./../DAQ_TPX/daq_tpx.h
trgReader10.o: ../../include/rtsLog.h
trgReader10.o: ../../include/rtsSystems.h
trgReader10.o: ../../include/daqFormats.h
trgReader10.o: ../../include/SECTOR/sector.h
trgReader10.o: ../../trg/include/trgStructures.h
trgReader10.o: ../../include/L3/L3Formats.h
trgReader10.o: ./evpSupport.h
trgReader10.o: ../../include/rts.h
trgReader12.o: ../../include/rtsLog.h
trgReader12.o: ../../include/daqFormats.h
trgReader12.o: ../../include/SECTOR/sector.h
trgReader12.o: ../../trg/include/trgStructures.h
trgReader12.o: ../../include/L3/L3Formats.h
trgReader12.o: ../../include/rtsSystems.h
trgReader12.o: ./evpSupport.h
trgReader12.o: ../../include/rts.h
trgReader12.o: ./trgReader.h
trgReader20.o: ../../include/rtsLog.h
trgReader20.o: ../../include/rtsSystems.h
trgReader20.o: ../../include/daqFormats.h
trgReader20.o: ../../include/SECTOR/sector.h
trgReader20.o: ../../trg/include/trgStructures.h
trgReader20.o: ../../include/L3/L3Formats.h
trgReader20.o: ./evpSupport.h
trgReader20.o: ../../include/rts.h
trgReader20.o: ./trgReader.h
trgReader21.o: ../../include/rtsLog.h
trgReader21.o: ../../include/rtsSystems.h
trgReader21.o: ../../include/daqFormats.h
trgReader21.o: ../../include/SECTOR/sector.h
trgReader21.o: ../../trg/include/trgStructures.h
trgReader21.o: ../../include/L3/L3Formats.h
trgReader21.o: ./evpSupport.h
trgReader21.o: ../../include/rts.h
trgReader21.o: ./trgReader.h
trgReader22.o: ../../include/rtsLog.h
trgReader22.o: ../../include/rtsSystems.h
trgReader22.o: ../../include/daqFormats.h
trgReader22.o: ../../include/SECTOR/sector.h
trgReader22.o: ../../trg/include/trgStructures.h
trgReader22.o: ../../include/L3/L3Formats.h
trgReader22.o: ./evpSupport.h
trgReader22.o: ../../include/rts.h
trgReader22.o: ./trgReader.h
trgReader30.o: ../../include/rtsLog.h
trgReader30.o: ../../include/rtsSystems.h
trgReader30.o: ../../include/daqFormats.h
trgReader30.o: ../../include/SECTOR/sector.h
trgReader30.o: ../../trg/include/trgStructures.h
trgReader30.o: ../../include/L3/L3Formats.h
trgReader30.o: ./evpSupport.h
trgReader30.o: ../../include/rts.h
trgReader30.o: ./trgReader.h
trgReader32.o: ../../include/rtsLog.h
trgReader32.o: ../../include/rtsSystems.h
trgReader32.o: ../../include/daqFormats.h
trgReader32.o: ../../include/SECTOR/sector.h
trgReader32.o: ../../trg/include/trgStructures.h
trgReader32.o: ../../include/L3/L3Formats.h
trgReader32.o: ./evpSupport.h
trgReader32.o: ../../include/rts.h
trgReader32.o: ./trgReader.h
trgReader.o: ../../include/rtsLog.h
trgReader.o: ../../include/daqFormats.h
trgReader.o: ../../include/SECTOR/sector.h
trgReader.o: ../../trg/include/trgStructures.h
trgReader.o: ../../include/L3/L3Formats.h
trgReader.o: ../../include/rtsSystems.h
trgReader.o: ./evpReader.hh
trgReader.o: ../../include/evp.h
trgReader.o: ../../include/rts.h
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
trgReader.o: ./../SFS/sfs_index.h
trgReader.o: ./../SFS/fs_index.h
trgReader.o: ./../RTS_READER/rts_reader.h
trgReader.o: ../RTS_READER/daq_det.h
trgReader.o: ../RTS_READER/rts_reader.h
trgReader.o: ../RTS_READER/daq_dta.h
trgReader.o: ./pp2ppReader.h
evpSupport.o: ../../include/rts.h
l3Reader.o: ./evpSupport.h
l3Reader.o: ../../include/rts.h
l3Reader.o: ../../include/daqFormats.h
l3Reader.o: ../../include/SECTOR/sector.h
l3Reader.o: ../../trg/include/trgStructures.h
l3Reader.o: ../../include/L3/L3Formats.h
pmdReader.o: ../../include/daqFormats.h
pmdReader.o: ../../include/SECTOR/sector.h
pmdReader.o: ../../trg/include/trgStructures.h
pmdReader.o: ../../include/L3/L3Formats.h
pp2ppReader.o: ../../include/daqFormats.h
pp2ppReader.o: ../../include/SECTOR/sector.h
pp2ppReader.o: ../../trg/include/trgStructures.h
pp2ppReader.o: ../../include/L3/L3Formats.h
evpReader.o: ../../include/evp.h
evpReader.o: ../../include/rts.h
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
evpReader.o: ../../include/daqFormats.h
evpReader.o: ../../include/SECTOR/sector.h
evpReader.o: ../../trg/include/trgStructures.h
evpReader.o: ../../include/L3/L3Formats.h
evpReader.o: ./emcReader.h
evpReader.o: ./l3Reader.h
evpReader.o: ./ssdReader.h
evpReader.o: cfgutil.h
evpReader.o: ./../SFS/sfs_index.h
evpReader.o: ./../SFS/fs_index.h
evpReader.o: ./../RTS_READER/rts_reader.h
evpReader.o: ../../include/rtsLog.h
evpReader.o: ../../include/rtsSystems.h
evpReader.o: ../RTS_READER/daq_det.h
evpReader.o: ../RTS_READER/rts_reader.h
evpReader.o: ../RTS_READER/daq_dta.h
evpReader.o: ./pp2ppReader.h
