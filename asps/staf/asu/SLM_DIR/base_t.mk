# base_t.mk
########################################################################
.PHONY: setup config
#
include $(SLM_DIR)/env.mk
#
setup: $(SUBDIRS) $(SUBMAKES)
#
config:
#
