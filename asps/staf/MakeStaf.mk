CWD := $(shell pwd)
MAKESTAFLOGON :=$(CWD)/makestaflogon.mk
ifndef INPUT_DIR
ifMAKESTAFLOGON := $(strip $(wildcard $(MAKESTAFLOGON)))
  ifdef ifMAKESTAFLOGON
    include $(MAKESTAFLOGON)
  endif
endif

#
ifndef STAF_MAKE_HOME
  STAF_MAKE_HOME := $(shell pwd)
endif


###	Suppress all imlicit rules
.SUFFIXES:

include $(STAF_MAKE_HOME)/MakeEnv.mk

#
#	INP_DIR & OUT_DIR could be declared in invoking
#
ifndef INP_DIR
  override INP_DIR := $(CWD)
endif
ifeq (,$(strip $(filter /%,$(INP_DIR))))
  override INP_DIR := $(CWD)/$(INP_DIR)
endif



#	Name of domain
DOMAIN := $(notdir $(INP_DIR))

#	Is it system part?
SYSTEM_DOMAIN := $(strip $(filter sys staf asps,$(DOMAIN)))
DOM := ana
ifdef SYSTEM_DOMAIN
 DOM :=sys
endif

#
ifndef OUT_DIR
  override OUT_DIR := $(CWD)/GEN$(DOM)
endif


ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif

#	Check if OUT directory on AFS ?
ifOUT_AFS := $(strip $(filter /afs/%,$(OUT_DIR)))


ifndef SYSTEM_DOMAIN
# 	if ANA case, STAF_SYS pointed out to STAF system area
  ifndef  STAF_SYS
    STAF_SYS := $(shell echo "*** STAF_SYS is not defined ***"; exit 1)
  endif
endif


#	Generic .inc dir  , copy of all includes
INC_GEN_DIR := $(OUT_DIR)/inc
#	list of all /inc dirs
ALL_INC_DIRS := $(wildcard $(INP_DIR)/???/inc)
ALL_INC_DIRS := $(filter-out $(INC_GEN_DIR), $(ALL_INC_DIRS))



#	List of all include files
ALL_INC_INST := $(wildcard $(addsuffix /*.[hi]*,$(ALL_INC_DIRS)))
ALL_INC_INST := $(filter %.inc %.h %.hh %.idl, $(ALL_INC_INST))
ALL_INC_INST := $(sort $(addprefix $(INC_GEN_DIR)/,$(notdir $(ALL_INC_INST))))

VPATH := $(ALL_INC_DIRS)

ASPS  := $(notdir $(subst / , ,$(dir $(wildcard $(INP_DIR)/???/src)) ))
#
# 	if PKG=pkg  use unly this package(pam or asp)
#	... example  PKG=sdd   only sdd will be maked
#	... example  PKG=a% only started from a (like asu amu) will be maked
ifdef pkg
  PKG := $(pkg)
endif  
ifdef PKG
 ASPS := $(filter $(PKG),$(ASPS))
endif

ASPSLIB := $(filter-out sdd pam tbr c4t tst,$(ASPS))
ASPSLIB := $(addsuffix _LIB,$(ASPSLIB))

# 	list of exe dirs
ASPSEXE := $(ASPS)
ifdef SYSTEM_DOMAIN
  ASPSEXE := $(strip $(wildcard $(INP_DIR)/???/srcm))
  ifdef ASPSEXE
    ASPSEXE := $(subst / , ,$(dir $(ASPSEXE)) )
    ASPSEXE := $(notdir $(basename $(ASPSEXE)))
  endif
ifdef PKG
 ASPSEXE := $(filter $(PKG),$(ASPSEXE))
endif


  ASPSEXE += $(findstring sdd,$(ASPS))
endif
ASPSEXE := $(sort $(addsuffix _EXE,$(ASPSEXE)))


.PHONY : all setup libs exes incs setup $(ASPS) $(ASPSLIB) $(ASPSEXE)

all : incs libs exes

libs : $(ASPSLIB)
$(ASPSLIB): %_LIB:
	$(MAKE) -f $(STAF_HOME)/MakeAsp.mk lib INP_DIR=$(INP_DIR)/$(STEM) OUT_DIR=$(OUT_DIR)


exes : $(ASPSEXE)

$(ASPSEXE): %_EXE : 
	$(MAKE) -f $(STAF_HOME)/MakeAsp.mk exe INP_DIR=$(INP_DIR)/$(STEM) OUT_DIR=$(OUT_DIR)

incs : $(ALL_INC_INST)

$(ALL_INC_INST): $(OUT_DIR)/inc/%  : %
######	cp $(ALL_DEPS) $(OUT_DIR)/inc/$(STEM)
	ln -s $(ALL_DEPS) $(OUT_DIR)/inc/$(STEM)


setup :
	@echo MakeStaf.setup create $(MAKESTAFLOGON) file
	@$(RM) $(MAKESTAFLOGON);
	@echo STAF_MAKE_HOME:=$(STAF_MAKE_HOME)   > $(MAKESTAFLOGON);
	@echo INP_DIR:=$(INP_DIR)                >> $(MAKESTAFLOGON);
	@echo OUT_DIR:=$(OUT_DIR)                >> $(MAKESTAFLOGON);
	@echo  STAF_SYS:=$(STAF_SYS)             >> $(MAKESTAFLOGON);
	@echo  STAF_SYS_INCS:=$(STAF_SYS_INCS)   >> $(MAKESTAFLOGON);
	@echo  STAF_ANA:=$(STAF_ANA)             >> $(MAKESTAFLOGON);
	@echo  STAF_ANA_INCS:=$(STAF_ANA_INCS)   >> $(MAKESTAFLOGON);
	@mkdir -p $(OUT_DIR)/inc                 
	@mkdir -p $(OUT_DIR)/srg
	@mkdir -p $(OUT_DIR)/.$(STAF_ARCH)/lib
	@mkdir -p $(OUT_DIR)/.$(STAF_ARCH)/bin
	@mkdir -p $(OUT_DIR)/.$(STAF_ARCH)/obj
	@mkdir -p $(OUT_DIR)/.$(STAF_ARCH)/dep
ifdef ifOUT_AFS
	@if [ ! -h $(OUT_DIR)/lib ]; then \
	  ln -f -s $(OUT_DIR)/.@sys/lib $(OUT_DIR)/lib ; \
	fi
	@if [ ! -h $(OUT_DIR)/bin ]; then \
	  ln -f -s $(OUT_DIR)/.@sys/bin $(OUT_DIR)/bin ; \
	fi
	@if [ ! -h $(OUT_DIR)/obj ]; then \
	  ln -f -s $(OUT_DIR)/.@sys/obj $(OUT_DIR)/obj ; \
	fi
	@if [ ! -h $(OUT_DIR)/dep ]; then \
	  ln -f -s $(OUT_DIR)/.@sys/dep $(OUT_DIR)/dep ; \
	fi
endif

show: 
	@echo EXPERIMENT	:= $(EXPERIMENT)
	@echo STAF_SYS		:= $(STAF_SYS)
	@echo STAF_ANA		:= $(STAF_ANA)
	@echo STAF_SYS_INCS	:= $(STAF_SYS_INCS)
	@echo STAF_SYS_LIBS	:= $(STAF_SYS_LIBS)
	@echo MAKE        	:= $(MAKE)
	@echo STAF_HOME   	:= $(STAF_HOME)
	@echo INP_DIR     	:= $(INP_DIR) 
	@echo OUT_DIR     	:= $(OUT_DIR) 
	@echo ASPS        	:= $(ASPS)
	@echo ASPSEXE       	:=$(ASPSEXE)=
	@echo ALL_INC_DIRS	:= $(ALL_INC_DIRS)
	@echo ALL_INC_INST      := $(ALL_INC_INST)
	@echo INC_GEN_DIR       := $(INC_GEN_DIR)
	@echo LN       		:= $(LN)



