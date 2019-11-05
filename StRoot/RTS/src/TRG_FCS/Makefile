RTS_ST_ROOT = ../../../../StRoot/RTS

include $(RTS_ST_ROOT)/src/rtsmakefile.def
include $(RTS_ST_ROOT)/src/rtsplus.def

DEFINES += -DRTS_LOG_COLORED

LDLIBS := -lm -lrt 

VPATH = $(RTS_ST_ROOT)/src/LOG $(RTS_ST_ROOT)/src/DAQ_FCS $(RTS_ST_ROOT)/src/DAQ_READER  $(RTS_ST_ROOT)/src/SFS \
	$(RTS_ST_ROOT)/src/DAQ_TRG

VOBJS = librtslog.a libfcs.a libdaqReader.a libsfs.a libtrg.a

LIBS = fcs_trg_lib.a
BINS = 


all : $(LIBS) fcs_trg_run

libs: $(LIBS)


fcs_trg_run: fcs_trg_run.o fcs_trg_lib.a $(VOBJS)
	$(LINK.o) -o $@ $(LDLIBS) -Wl,--whole-archive $^ -Wl,--no-whole-archive

# ALL versions of stages need to be entered here!
fcs_trg_lib.a : fcs_trg_base.o \
	stage_0_201900.o \
	stage_1_201900.o \
	stage_2_201900.o \
	stage_3_201900.o
	$(RTS_AR)

# DO NOT DELETE

fcs_trg_base.o: ../../../../StRoot/RTS/include/rtsLog.h
fcs_trg_base.o: ../DAQ_FCS/fcs_data_c.h
fcs_trg_base.o: fcs_trg_base.h
stage_0_201900.o: fcs_trg_base.h
stage_1_201900.o: fcs_trg_base.h
stage_2_201900.o: fcs_trg_base.h
stage_3_201900.o: fcs_trg_base.h
fcs_trg_run.o: ../../../../StRoot/RTS/include/rtsLog.h
fcs_trg_run.o: ../../../../StRoot/RTS/include/rtsSystems.h
fcs_trg_run.o: ../../../../StRoot/RTS/include/daqFormats.h
fcs_trg_run.o: ../../../../StRoot/RTS/include/SECTOR/sector.h
fcs_trg_run.o: ../../trg/include/trgDataDefs_46.h
fcs_trg_run.o: ../../../../StRoot/RTS/include/L3/L3Formats.h
fcs_trg_run.o: ../DAQ_READER/daqReader.h
fcs_trg_run.o: ../DAQ_READER/daqConfig.h
fcs_trg_run.o: ../DAQ_READER/daq_dta.h
fcs_trg_run.o: ../../../../StRoot/RTS/include/rts.h
fcs_trg_run.o: ../DAQ_READER/daq_dta_structs.h
fcs_trg_run.o: ../../trg/include/trgConfNum.h
fcs_trg_run.o: ../DAQ_TRG/daq_trg.h
fcs_trg_run.o: ../DAQ_READER/daq_det.h
fcs_trg_run.o: ../DAQ_FCS/daq_fcs.h
fcs_trg_run.o: ../DAQ_FCS/fcs_data_c.h
fcs_trg_run.o: fcs_trg_base.h
