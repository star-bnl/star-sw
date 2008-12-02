-include ../rtsmakefile.def	# for RTS
-include ../rtsplus.def



LDLIBS = 
DEFINES += -DRTS_LOG_COLORED


VOBJS += libbsmd.a libbtow.a libemc.a libesmd.a libfpd.a libftp.a libl3.a libpmd.a libpp2pp.a \
	libric.a libsc.a libssd.a libsvt.a libtof.a libtpc.a libtpcfcf.a libtpx.a libtrg.a libetow.a \
	libdaqReader.a libsfs.a librtslog.a
VPATH += ../DAQ_BSMD ../DAQ_BTOW ../DAQ_EMC ../DAQ_ESMD ../DAQ_FPD ../DAQ_FTP ../DAQ_L3 ../DAQ_PMD \
	../DAQ_PP2PP ../DAQ_RIC ../DAQ_SC ../DAQ_SSD ../DAQ_SVT ../DAQ_TOF ../DAQ_TPC ../FCF ../DAQ_ETOW \
	../DAQ_TPX ../DAQ_TRG \
	../DAQ_READER  ../SFS ../LOG 


BINS =  rts_example


all:   $(BINS)


rts_example: rts_example.o $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

sfs_stuff: sfs_stuff.o $(VOBJS)
	$(LINK.o) -o $@ $^ 


