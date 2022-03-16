include ../rtsmakefile.def
LDLIBS := 

VPATH = ../LOG
VOBJS = librtslog.a

INCLUDES = -g -I. -I../ -I../../include -I../../trg/include
DEFINES += -DRTS_LOG_COLORED

BINS =  fs fs_ex  sfsTest sfs_single example sfs_header
LIBS = libsfs.a

LDFLAGS += ${HOSTFLAGS}

all:  libs $(BINS) 

libs:	$(LIBS)

blastSFS: libsfs.a blastSFS.o

fs: libsfs.a fs.o $(VOBJS)

fs_ex: libsfs.a fs_ex.o $(VOBJS)

readPed: libsfs.a readPed.o $(VOBJS)

sfsTest: libsfs.a sfsTest.o $(VOBJS)

sfs_experiment: libsfs.a sfs_experiment.o $(VOBJS)

example: libsfs.a example.o $(VOBJS)

sfs_single: libsfs.a sfs_single.o $(VOBJS)

daq_test: libsfs.a daq_test.o $(VOBJS)

sfs_header: libsfs.a sfs_header.o $(VOBJS)

libsfs.a: fs_index.o sfs_index.o sfs_index_daq.o get_line.o
	ar -rucvs libsfs.a $?

$(LIBDIR)/libsfs.a: libsfs.a
	@$(INSTALL) libsfs.a $(LIBDIR)

install:
	$(INSTALL) fs $(BINDIR)
	cp fs daq_fs
	$(INSTALL) daq_fs $(BINDIR)
	$(INSTALL) sfs_header $(BINDIR)
	$(INSTALL) libsfs.a $(LIBDIR)

clean:
	@$(CLEAN) $(BINS) $(LIBS)

depend:
	@$(MAKEDEPEND)


# DO NOT DELETE

fs_index.o: ../../include/rts.h
fs_index.o: ../../include/rtsLog.h
fs_index.o: fs_index.h
sfs_index.o: sfs_index.h
sfs_index.o: fs_index.h
sfs_index.o: sfs_base.h
sfs_index.o: ../../include/rtsLog.h
sfs_index.o: ../../include/rts.h
sfs_index_daq.o: sfs_index.h
sfs_index_daq.o: fs_index.h
sfs_index_daq.o: sfs_base.h
sfs_index_daq.o: ../../include/rtsLog.h
sfs_index_daq.o: ../../include/rts.h
sfs_index_daq.o: ../../include/daqFormats.h
sfs_index_daq.o: ../../include/SECTOR/sector.h
sfs_index_daq.o: ../../trg/include/trgDataDefs_46.h
sfs_index_daq.o: ../../include/L3/L3Formats.h
blastSFS.o: ../../include/SUNRT/clock.h
daq_test.o: sfs_index.h
daq_test.o: fs_index.h
daq_test.o: sfs_base.h
daq_test.o: ../../include/rtsLog.h
example.o: sfs_index.h
example.o: fs_index.h
example.o: sfs_base.h
fs.o: ../../include/SUNRT/clock.h
fs.o: sfs_index.h
fs.o: fs_index.h
fs.o: sfs_base.h
fs.o: get_line.h
fs.o: ../../include/rtsLog.h
fs_ex.o: ../../include/rts.h
fs_ex.o: sfs_index.h
fs_ex.o: fs_index.h
fs_ex.o: sfs_base.h
fs_ex.o: ../../include/SUNRT/clock.h
sfs_header.o: sfs_index.h
sfs_header.o: fs_index.h
sfs_header.o: sfs_base.h
sfs_single.o: sfs_index.h
sfs_single.o: fs_index.h
sfs_single.o: sfs_base.h
sfs_single.o: ../../include/rtsLog.h
sfsTest.o: sfs_index.h
sfsTest.o: fs_index.h
sfsTest.o: sfs_base.h
sfs_index.o: fs_index.h
sfs_index.o: sfs_base.h
