
# need to figure out if I'm inside StRoot or src

STROOT_STR:=StRoot
IS_STROOT=$(findstring $(STROOT_STR), $(PWD))


ifeq "$(STROOT_STR)" "$(IS_STROOT)" 
	SAY="Working under StRoot"
	DEFINES += -DRTS_DISABLE_LOG
	RTS_DIR_PATH = ../../online/RTS
else
	SAY="Working in RTS"
	RTS_DIR_PATH = ../..
	VPATH = ../LOG
	VOBJS = librtslog.a
endif

include $(RTS_DIR_PATH)/src/rtsmakefile.def

INCLUDES = -g -I. -I../ -I$(RTS_DIR_PATH)/include -I$(RTS_DIR_PATH)/trg/include
DEFINES += -DRTS_LOG_COLORED

BINS =  fs fs_ex  sfsTest sfs_single
LIBS = libsfs.a

all:  libs $(BINS) 

libs:	$(LIBS)

blastSFS: libsfs.a blastSFS.o

fs: libsfs.a fs.o

fs_ex: libsfs.a fs_ex.o 

readPed: libsfs.a readPed.o

sfsTest: libsfs.a sfsTest.o

sfs_experiment: libsfs.a sfs_experiment.o

sfs_single: libsfs.a sfs_single.o

libsfs.a: fs_index.o sfs_index.o daqr_index.o get_line.o fsBankReader.o
	ar -rucvs libsfs.a $?

$(LIBDIR)/libsfs.a: libsfs.a
	@$(INSTALL) libsfs.a $(LIBDIR)

install:
	@$(INSTALL) fs $(BINDIR)
	@$(INSTALL) libsfs.a $(LIBDIR)

clean:
	@$(CLEAN) $(BINS) $(LIBS)

depend:
	@$(MAKEDEPEND)


# DO NOT DELETE

daqr_index.o: ../../include/rts.h
daqr_index.o: daqr_index.h
daqr_index.o: ../../include/daqFormats.h
daqr_index.o: ../../include/SECTOR/sector.h
daqr_index.o: ../../trg/include/trgStructures.h
daqr_index.o: ../../include/L3/L3Formats.h
daqr_index.o: fs_index.h
fsBankReader.o: ../../include/SUNRT/clock.h
fsBankReader.o: ../../include/daqFormats.h
fsBankReader.o: ../../include/SECTOR/sector.h
fsBankReader.o: ../../trg/include/trgStructures.h
fsBankReader.o: ../../include/L3/L3Formats.h
fsBankReader.o: ../../include/rts.h
fsBankReader.o: fs_index.h
fsBankReader.o: fsBankReader.h
fs_index.o: ../../include/rts.h
fs_index.o: fs_index.h
sfs_index.o: sfs_index.h
sfs_index.o: fs_index.h
blastSFS.o: ../../include/SUNRT/clock.h
fs.o: ../../include/SUNRT/clock.h
fs.o: daqr_index.h
fs.o: ../../include/daqFormats.h
fs.o: ../../include/SECTOR/sector.h
fs.o: ../../trg/include/trgStructures.h
fs.o: ../../include/L3/L3Formats.h
fs.o: fs_index.h
fs.o: sfs_index.h
fs.o: get_line.h
fs.o: fsBankReader.h
fs_ex.o: ../../include/rts.h
fs_ex.o: daqr_index.h
fs_ex.o: ../../include/daqFormats.h
fs_ex.o: ../../include/SECTOR/sector.h
fs_ex.o: ../../trg/include/trgStructures.h
fs_ex.o: ../../include/L3/L3Formats.h
fs_ex.o: fs_index.h
fs_ex.o: sfs_index.h
fs_ex.o: ../../include/SUNRT/clock.h
sfsTest.o: sfs_index.h
sfsTest.o: fs_index.h
daqr_index.o: ../../include/daqFormats.h
daqr_index.o: ../../include/SECTOR/sector.h
daqr_index.o: ../../trg/include/trgStructures.h
daqr_index.o: ../../include/L3/L3Formats.h
daqr_index.o: fs_index.h
sfs_index.o: fs_index.h
