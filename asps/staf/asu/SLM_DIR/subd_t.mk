# subd_t.mk
########################################################################
.PHONY: setup
#
include $(SLM_DIR)/env.mk
#include $(SLM_DIR)/defaults.mk
#
setup: $(SUBDIRS) $(SUBMAKES)
#
%.config:
	@echo "************************************"
	@echo "***" $@ "not found"
	@echo "*** Please run 'make config' in base
	@echo "*** directory before proceeding.
	@echo "************************************"
	@cat a_non_existant_file
#
