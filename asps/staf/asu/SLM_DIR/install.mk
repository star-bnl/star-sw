#
include $(BASE_DIR)/$(notdir $(BASE_DIR)).config
#
install_all: install_idl install_inc install_lib install_solib install_bin
#
MODULES := $(CVSROOT)/CVSROOT/modules
#BRANCH_DIR := $(shell awk '$$1=="$(BASE_NAME)" {print $$2}' $(MODULES))
#BRANCH_DIR := $(addprefix $(STAR_LIB)/,$(BRANCH_DIR))
#BRANCH_DIR := $(dir $(BRANCH_DIR))
BRANCH_DIR := $(STAR_LIB)/dev/$(BRANCH_NAME)/
#
INSTALL_IDL_FILES := $(wildcard $(IDLDIR)/*.idl)
install_idl:
	@echo " "
	@echo "Installing IDL files."
ifneq ($(INSTALL_IDL_FILES),$(EMPTY))
	cp $(INSTALL_IDL_FILES) $(BRANCH_DIR)/inc/
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
	cp $(INSTALL_INC_FILES) $(BRANCH_DIR)/inc/
else
	@echo "No INC files installed."
endif
#
INSTALL_LIB_FILES := $(wildcard $(LIBDIR)/*.a)
install_lib:
	@echo " "
	@echo "Installing LIB files."
ifneq ($(INSTALL_LIB_FILES),$(EMPTY))
	cp $(INSTALL_LIB_FILES) \
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
	cp $(INSTALL_SOLIB_FILES) \
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
	cp $(INSTALL_BIN_FILES) \
		$(BRANCH_DIR)/bin/
else
	@echo "No BIN files installed."
endif
#
