#  Make STAF (STAR) executable

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

#
ifndef OUT_DIR
  override OUT_DIR := $(CWD)/EXE
endif

ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif


CPPFLAGS += -DSTAF -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
GEA := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
FOR := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
GST := $(FOR72) $(FFLAGS)   $(EXEFLAGS)
GSC := $(CXX)   $(CXXFLAGS) $(EXEFLAGS)
CXX := $(CXX)   $(CXXFLAGS) $(CPPFLAGS)  
CC  := $(CC)    $(CFLAGS)   $(CPPFLAGS)   
FSL := $(FOR72) $(FFLAGS)   $(CPPFLAGS)   $(SOFLAGS)


#	OUT dirs
OBJ_DIR := $(OUT_DIR)/.$(STAF_ARCH)/obj/rex
EXE_DIR := $(ROOTSYS)/bin

ifAFS_OUT := $(strip $(filter /afs/%, $(OUT_DIR)))
ifdef ifAFS_OUT
  AFS_OBJ_DIR := $(OUT_DIR)/obj
  AFS_EXE_DIR := $(OUT_DIR)/bin
endif

SRC_DIRS := $(INP_DIR)  

VPATH := $(SRC_DIRS) $(OUT_DIR) $(OBJ_DIR)  $(EXE_DIR) 

FILES_O := $(addsuffix /*.[fFgc]*,$(SRC_DIRS))
FILES_O := $(wildcard $(FILES_O))
FILES_O := $(filter %.f %.F %.c %.cc %.cxx,$(FILES_O))
FILES_O := $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(basename $(notdir $(FILES_O)))))


INCL  := $(ROOTSYS)/include $(ROOTSYS)/src $(INP_DIR) $(GST_DIR) $(STAF_SYS_INCS) $(CERN_ROOT)/include $(CERN_ROOT)/src/pawlib/paw/ntuple
INCL  := $(addprefix -I,$(INCL))

STAF  = YES
CCload = YES



#DOEXE  = $(GSC) $(GST_DIR)/main/acmain.c -o $(EXE_DIR)/$(TARGET) $(FILES_O)
DOEXE  = $(GSC)  $(FILES_O)


ifdef STAF
DOEXE  += -DSTAF $(INCL)
endif

ALL_EXE_LIBS :=
ifdef GCALOR
ALL_EXE_LIBS +=     -L/afs/cern.ch/atlas/offline/@sys/pro/lib -lgcalor 
endif

ifdef STAF
# ALL_EXE_LIBS +=    -L$(STAF_SYS_LIB) -lmsg -ltdm -lspx -lsoc -lasu -ltop -ltnt -lami -ldio -ldui -ldsl -ldsu -ltls 
endif

ALL_EXE_LIBS +=	`cernlib geant321 

ifdef  MOTIF
  ALL_EXE_LIBS += graflib/Motif
else
  ALL_EXE_LIBS += graflib/X11
endif
ALL_EXE_LIBS += packlib mathlib kernlib`
 
ROOTLIBS      = -L$(ROOTSYS)/lib -lRint -lNew -lBase -lCint -lClib -lCont -lFunc \
                -lGraf -lGraf3d -lHist -lHtml -lMatrix -lMeta -lMinuit -lNet \
                -lPostscript -lProof -lTree -lUnix -lZip
ROOTGLIBS     = -lGpad -lGui -lGX11 -lX3d





ALL_EXE_LIBS += $(ROOTLIBS) $(ROOTGLIBS)
ALL_EXE_LIBS += -lXpm $(FLIBS) $(CLIBS)	

#.SILENT:
.SUFFIXES:
.SUFFIXES:  .o .g .f .c .cc .cxx   .F


Root.exe: $(FILES_O)
	$(DOEXE)  $(ALL_EXE_LIBS) -o $(EXE_DIR)/$(notdir $(TARGET))  
#
#
#
$(OBJ_DIR)/%.o : %.f
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.F
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.c
	$(CC)  -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.cc
	$(CXX) -c $(INCL) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.cxx
	$(CXX) -c $(INCL) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
#
#
#



cleanall :
	$(RMDIR) $(OUT_DIR)
clean:
	$(RMDIR) $(OBJ_DIR) $(EXE_DIR)
	

setup:  $(OBJ_DIR) $(EXE_DIR) $(AFS_OBJ_DIR) $(AFS_EXE_DIR)

$(OBJ_DIR) $(EXE_DIR): 
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
	
