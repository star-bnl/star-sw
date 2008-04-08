include ../rtsmakefile.def

VPATH = ../LOG
VOBJS = librtslog.a

INCLUDES = -g -I. -I../ -I../../include -I$../../trg/include
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

libsfs.a: fs_index.o sfs_index.o get_line.o
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


