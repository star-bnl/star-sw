#  Make STAF (STAR) executable
STAF_ARCHIVE_LIBS :=
ifndef STAF_MAKE_HOME
  STAF_MAKE_HOME := $(STAR)/mgr
endif
include $(STAF_MAKE_HOME)/MakeEnv.mk
include $(STAF_MAKE_HOME)/MakeArch.mk

#
#	INP_DIR & OUT_DIR could be declared in invoking
#
ifndef INP_DIR
  override INP_DIR := $(CWD)
endif
ifeq (,$(strip $(filter /%,$(INP_DIR))))
  override INP_DIR := $(CWD)/$(INP_DIR)
endif
GST_DIR := $(INP_DIR)/gst

#
ifndef OUT_DIR
  override OUT_DIR := $(CWD)/EXE
endif

ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif

STAF :=YESS

CPPFLAGS += -DSTAF -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
GEA := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
FOR := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
GST := $(FOR72) $(FFLAGS)   $(EXEFLAGS)
GSC := $(CXX)   $(CXXFLAGS) $(EXEFLAGS)
CPP := $(CXX)   $(CXXFLAGS) $(CPPFLAGS)  
CC  := $(CC)    $(CFLAGS)   $(CPPFLAGS)   
FSL := $(FOR72) $(FFLAGS)   $(CPPFLAGS)   $(SOFLAGS)

CMDS  := atlsim starsim gstar staf staf+ staf++ Staf

#	OUT dirs
OBJ_DIR := $(OUT_DIR)/.$(STAF_ARCH)/obj/exe
DEP_DIR := $(OUT_DIR)/.$(STAF_ARCH)/dep/exe
SRG_DIR := $(OUT_DIR)/srg/exe
EXE_DIR := $(OUT_DIR)/.$(STAF_ARCH)/bin

ifAFS_OUT := $(strip $(filter /afs/%, $(OUT_DIR)))
ifdef ifAFS_OUT
  AFS_OBJ_DIR := $(OUT_DIR)/obj
  AFS_EXE_DIR := $(OUT_DIR)/bin
endif

SRC_DIRS := $(addprefix $(GST_DIR)/,main dummy ntuple comis kuip agsim deccc dzdoc geant zebra)  

VPATH := $(SRC_DIRS) $(OUT_DIR) $(OBJ_DIR) $(SRG_DIR) $(EXE_DIR) 

FILES_O := $(addsuffix /*.[fFgc]*,$(SRC_DIRS))
FILES_O := $(wildcard $(FILES_O))
FILES_O := $(filter %.f %.F %.c %.cc %.g %.cdf,$(FILES_O))
FILES_O := $(filter-out %afmain.F,$(FILES_O))
FILES_D := $(filter-out %.f %.g %.cdf,$(FILES_O))))))

FILES_O := $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(basename $(notdir $(FILES_O)))))
FILES_D := $(addprefix $(DEP_DIR)/,$(addsuffix .d,$(basename $(notdir $(FILES_D)))))


INCL  := $(INP_DIR) $(GST_DIR) $(STAF_SYS_INCS) $(CERN_ROOT)/include $(CERN_ROOT)/src/pawlib/paw/ntuple
INCL  := $(addprefix -I,$(INCL))


#DOEXE  = $(GSC) $(GST_DIR)/main/acmain.c -o $(EXE_DIR)/$(TARGET) $(FILES_O)
DOEXE  = $(GSC)  $(FILES_O)


DOEXE  += -DSTAF $(INCL)

ALL_EXE_LIBS :=
ifdef GCALOR
ALL_EXE_LIBS +=     -L/afs/cern.ch/atlas/offline/@sys/pro/lib -lgcalor 
endif

ifdef STAF
  ASPs := msg tdm spx  asu top tnt ami dio dui dsl dsu tls soc
  ASPsLib := $(addprefix -l,$(ASPs))

  ifdef ASPS_STATIC
    ASPsLib := $(addprefix $(STAF_SYS_LIB)/lib,$(addsuffix .a,$(ASPs)))
  endif
  ALL_EXE_LIBS +=    -L$(STAF_SYS_LIB) $(ASPsLib)
endif

ALL_EXE_LIBS +=	`cernlib geant321 pawlib

ifdef  MOTIF
  ALL_EXE_LIBS += graflib/Motif
else
  ALL_EXE_LIBS += graflib/X11
endif
ALL_EXE_LIBS += packlib mathlib kernlib`

ALL_EXE_LIBS += $(FLIBS) $(CLIBS)	

#.SILENT:
.SUFFIXES:
.SUFFIXES:  .o .g .f .c .cc .cdf  .F
#

ifDEP_DIR := $(wildcard $(DEP_DIR))
ifdef ifDEP_DIR
 include $(FILES_D)
endif
#
#
$(OBJ_DIR)/%.o :%.cdf
	kuipc     $(1ST_DEPS) $(SRG_DIR)/$(STEM).f
	$(FOR) -c $(SRG_DIR)/$(STEM).f -o  $(OBJ_DIR)/$(STEM).o
	$(RM)    $(SRG_DIR)/$(STEM).f

.PRECIOUS : $(SRG_DIR)/%.f
$(SRG_DIR)/%.f : %.g 
	cd $(GST_DIR); \
	$(EXE_DIR)/geant3    $(1ST_DEPS) -o $(SRG_DIR)/$(STEM).f

$(OBJ_DIR)/%.o : %.F
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : $(SRG_DIR)/%.f
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.f
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.c
	$(CC)  -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.cc
	$(CPP) -c $(INCL) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
#
#
geant3: $(GST_DIR)/geant3/geant3.f 
	$(GEA) -o $(EXE_DIR)/geant3 $(GST_DIR)/geant3/geant3.f `cernlib kernlib`
#
$(CMDS): $(FILES_O)
	$(DOEXE)   $(ALL_EXE_LIBS) -o $(EXE_DIR)/$(notdir $(TARGET))  
#
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
	echo '#!'$(PWD)'/$@'  > import.map
	nm $@|egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq>>import.map
	xlf $@ -o $@  -bE:import.map -lX11 -lXm -lXt
endif

$(DEP_DIR)/%.d: %.c 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w  $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cc 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w  $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) > $(ALL_TAGS)


$(DEP_DIR)/%.d:  %.F
	$(RM)  $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w -traditional $(CPPFLAGS) $(INCLUDES) -x c $(ALL_DEPS) | sed -e 's/\.F\.o/.o/' > $(ALL_TAGS)



 
detm:
	$(RM) detm.rz; echo "createdoc dzdoc.bank detm.rz; quit;" | dzedit


cleanall :
	$(RMDIR) $(OUT_DIR)
clean:
	$(RMDIR) $(OBJ_DIR) $(EXE_DIR)
	
atlas:	geant3 atlsim detm clean
star:	geant3 gstar  detm clean


setup: $(SRG_DIR) $(OBJ_DIR) $(DEP_DIR) $(EXE_DIR) $(AFS_OBJ_DIR) $(AFS_EXE_DIR)

$(SRG_DIR) $(OBJ_DIR) $(EXE_DIR) $(DEP_DIR): 
	@$(MKDIR) $(TARGET)

$(AFS_OBJ_DIR) :
	@$(LN) $(OUT_DIR)/.@sys/obj $(AFS_OBJ_DIR) 
$(AFS_EXE_DIR) :
	@$(LN) $(OUT_DIR)/.@sys/bin $(AFS_EXE_DIR) 



show:
	@echo VPATH=$(VPATH)
	@echo INP_DIR=$(INP_DIR)
	@echo OUT_DIR=$(OUT_DIR)
	@echo CC=$(CC)
	@echo STAF_ARCH=$(STAF_ARCH)
	@echo FILES_O=$(FILES_O)
	@echo SRC_DIRS=$(SRC_DIRS)
	@echo INCL=$(INCL)
	
