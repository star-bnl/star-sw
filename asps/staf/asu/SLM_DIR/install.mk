#
include $(BASE_DIR)/$(notdir $(BASE_DIR)).config
#
install_all: install_idl install_inc install_lib install_solib install_bin
#
MODULES := $(CVSROOT)/CVSROOT/modules
#BRANCH_DIR := $(shell awk '$$1=="$(BASE_NAME)" {print $$2}' $(MODULES))
#BRANCH_DIR := $(addprefix $(STAR_LIB)/,$(BRANCH_DIR))
#BRANCH_DIR := $(dir $(BRANCH_DIR))
SYS := sys
ifeq( $(BRANCH_NAME),$(SYS) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_SYS_LEVEL)/$(BRANCH_NAME)/
endif
SIM := sim
ifeq( $(BRANCH_NAME),$(SIM) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_SIM_LEVEL)/$(BRANCH_NAME)/
endif
ANA := ana
ifeq( $(BRANCH_NAME),$(ANA) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_ANA_LEVEL)/$(BRANCH_NAME)/
endif
PHY := phy
ifeq( $(BRANCH_NAME),$(PHY) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_PHY_LEVEL)/$(BRANCH_NAME)/
endif
OFL := ofl
ifeq( $(BRANCH_NAME),$(OFL) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_OFL_LEVEL)/$(BRANCH_NAME)/
endif
ONL := onl
ifeq( $(BRANCH_NAME),$(ONL) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_ONL_LEVEL)/$(BRANCH_NAME)/
endif
TRG := trg
ifeq( $(BRANCH_NAME),$(TRG) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_TRG_LEVEL)/$(BRANCH_NAME)/
endif
DAQ := daq
ifeq( $(BRANCH_NAME),$(DAQ) )
	BRANCH_DIR := $(STAR_LIB)/$(STAR_DAQ_LEVEL)/$(BRANCH_NAME)/
endif
#
# Real install command should be "cp".
INSTALL := cp
INSTALL := -ln -s
#
INSTALL_IDL_FILES := $(wildcard $(IDLDIR)/*.idl)
install_idl:
	@echo " "
	@echo "Installing IDL files."
ifneq ($(INSTALL_IDL_FILES),$(EMPTY))
	$(INSTALL) $(INSTALL_IDL_FILES) $(BRANCH_DIR)/inc/
else
	@echo "No IDL files installed."
endif
#
INSTALL_INC_FILES := $(wildcard $(INCDIR)/*.h) \
		$(wildcard $(INCDIR)/*.hh) \
		$(wildcard $(INCDIR)/*.inc) \
		$($(wildcard $(INCDIR)/*.cc)
install_inc:
	@echo " "
	@echo "Installing INC files."
ifneq ($(INSTALL_INC_FILES),$(EMPTY))
	$(INSTALL) $(INSTALL_INC_FILES) $(BRANCH_DIR)/inc/
else
	@echo "No INC files installed."
endif
#
INSTALL_LIB_FILES := $(wildcard $(LIBDIR)/*.a)
install_lib:
	@echo " "
	@echo "Installing LIB files."
ifneq ($(INSTALL_LIB_FILES),$(EMPTY))
	$(INSTALL) $(INSTALL_LIB_FILES) \
		$(BRANCH_DIR)/lib/
else
	@echo "No LIB files installed."
endif
#
INSTALL_SOLIB_FILES := $(wildcard $(LIBDIR)/*.a) \
		$(wildcard $(SOLIBDIR)/lib*.*)
install_solib:
	@echo " "
	@echo "Installing SOLIB files."
ifneq ($(INSTALL_SOLIB_FILES),$(EMPTY))
	$(INSTALL) $(INSTALL_SOLIB_FILES) \
		$(BRANCH_DIR)/solib/
else
	@echo "No SOLIB files installed."
endif
#
BINROOT := $(dir $(BINDIR))
#INSTALL_BIN_FILES := $(shell cat $(BINROOT)/install_bin_files)
install_bin:
	@echo " "
	@echo "Installing BIN files."
ifneq ($(INSTALL_BIN_FILES),$(EMPTY))
	cd $(BINDIR); \
	$(INSTALL) $(INSTALL_BIN_FILES) \
		$(BRANCH_DIR)/bin/
else
	@echo "No BIN files installed."
endif
#
