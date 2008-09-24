include ../rtsmakefile.def

VPATH = ../LOG
VOBJS = librtslog.a

INCLUDES = -g -I. -I../ -I../../include -I../../trg/include
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

libsfs.a: fs_index.o sfs_index.o sfs_index_daq.o get_line.o
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

fs_index.o: ../../include/rts.h
fs_index.o: ../../include/rtsLog.h
fs_index.o: fs_index.h
sfs_index.o: sfs_index.h
sfs_index.o: fs_index.h
sfs_index.o: sfs_base.h
sfs_index.o: ../../include/rtsLog.h
blastSFS.o: ../../include/SUNRT/clock.h
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
sfs_single.o: sfs_index.h
sfs_single.o: fs_index.h
sfs_single.o: sfs_base.h
sfsTest.o: sfs_index.h
sfsTest.o: fs_index.h
sfsTest.o: sfs_base.h
sfs_index.o: fs_index.h
sfs_index.o: sfs_base.h
