#
ifndef STAF_HOME
  STAF_HOME := $(shell pwd)
endif


###	Suppress all imlicit rules
.SUFFIXES:

include $(STAF_HOME)/MakeArch.mk

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
# override OUT_DIR := $(INP_DIR)/arch/$(STAF_ARCH)/$(STAF_SYS_LEVEL)/sys
  override OUT_DIR := ../../.$(STAF_ARCH)
endif


#ifeq (,$(strip $(filter /%,$(OUT_DIR))))
#  override OUT_DIR := $(CWD)/$(OUT_DIR)
#endif
#	Generic .inc dir  , copy of all includes
#INC_GEN_DIR := $(OUT_DIR)/inc
INC_GEN_DIR :=  ./inc
check_out   := $(shell test -d $(INC_GEN_DIR) || mkdir $(INC_GEN_DIR)) 
#	list of all /inc dirs
ALL_INC_DIRS := $(wildcard $(INP_DIR)/???/inc)
ALL_INC_DIRS := $(filter-out $(INC_GEN_DIR), $(ALL_INC_DIRS))
#	List of all include files
ALL_INC_INST := $(wildcard $(addsuffix /*.[hi]*,$(ALL_INC_DIRS)))
ALL_INC_INST := $(filter %.inc %.idl %.h %.hh, $(ALL_INC_INST))
ALL_INC_INST := $(sort $(addprefix $(INC_GEN_DIR)/,$(notdir $(ALL_INC_INST))))

VPATH := $(ALL_INC_DIRS)

ASPS  := $(notdir $(subst / , ,$(dir $(wildcard $(INP_DIR)/???/src)) ))
ASPSLIB := $(filter-out sdd pam tbr c4t tst,$(ASPS))
ASPSLIB := $(filter tls,$(ASPSLIB))  $(filter-out tls, $(ASPSLIB))
ASPSLIB := $(filter msg,$(ASPSLIB))  $(filter-out msg, $(ASPSLIB))
ifdef PGI
ASPSLIB := $(filter-out str,$(ASPSLIB))
endif 
#ASPSLIB := $(filter-out dio,$(ASPSLIB))
ASPSLIB := $(addsuffix _LIB,$(ASPSLIB))

ASPSEXE := $(notdir $(subst / , ,$(dir $(subst / , ,$(dir $(shell $(GREP) -l -e '.*main *(.*)' $(INP_DIR)/???/srcm/*.c*)) )) )) 
ASPSEXE += $(findstring sdd,$(ASPS))
ASPSEXE := $(sort $(addsuffix _EXE,$(ASPSEXE)))


.PHONY : all libs exes $(ASPS) $(ASPSLIB) $(ASPSEXE)

all : libs exes incs

libs : $(ASPSLIB)
$(ASPSLIB): %_LIB:
	$(MAKE) -f $(STAF_HOME)/MakeAsp.mk Libraries INP_DIR=$(INP_DIR)/$(STEM) OUT_DIR=$(OUT_DIR)


exes : $(ASPSEXE)

$(ASPSEXE): %_EXE : 
	$(MAKE) -f $(STAF_HOME)/MakeAsp.mk Executables INP_DIR=$(INP_DIR)/$(STEM) OUT_DIR=$(OUT_DIR)

incs : $(ALL_INC_INST)

$(ALL_INC_INST): $(INC_GEN_DIR)/%  : %
	ln -s $(ALL_DEPS) $(INC_GEN_DIR)/$(STEM)

show: 
	@echo MAKE        := $(MAKE)
	@echo STAF_HOME   := $(STAF_HOME)
	@echo INP_DIR     := $(INP_DIR) 
	@echo OUT_DIR     := $(OUT_DIR) 
	@echo ASPS        := $(ASPS)
	@echo ASPSEXE     := $(ASPSEXE)
	@echo ALL_INC_DIRS:= $(ALL_INC_DIRS)
	@echo ALL_INC_INST:= $(ALL_INC_INST)
	@echo INC_GEN_DIR := $(INC_GEN_DIR)
	@echo ASPSLIB     := $(ASPSLIB)
	@echo INC_GEN_DIR := $(INC_GEN_DIR)
	@echo GREP        := $(GREP)
	@echo STAF_ARCH   := $(STAF_ARCH)