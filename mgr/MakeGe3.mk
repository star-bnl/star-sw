#  Make GEANT3 (acually Mortran) executable

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
SRC_DIR := $(GST_DIR)/geant3
#
ifndef OUT_DIR
  override OUT_DIR := $(CWD)/EXE
endif

ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif


CPPFLAGS += -DSTAF -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
GEA := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
#	OUT dirs
EXE_DIR := $(OUT_DIR)/.$(STAF_ARCH)/bin

ifAFS_OUT := $(strip $(filter /afs/%, $(OUT_DIR)))
ifdef ifAFS_OUT
  AFS_EXE_DIR := $(OUT_DIR)/bin
endif

VPATH := $(SRC_DIR) $(EXE_DIR)
#
#
#
geant3: geant3.f 
	$(GEA) -o $(EXE_DIR)/geant3 $(1ST_DEPS) `cernlib kernlib`


setup:  $(EXE_DIR)  $(AFS_EXE_DIR)

$(EXE_DIR): 
	@$(MKDIR) $(TARGET)

$(AFS_EXE_DIR) :
	@$(LN) $(OUT_DIR)/.@sys/bin $(AFS_EXE_DIR) 



show:
	@echo INP_DIR=$(INP_DIR)
	@echo OUT_DIR=$(OUT_DIR)
	@echo STAF_ARCH=$(STAF_ARCH)
	
