# subd_v.mk
########################################################################
#
ifndef BASE_DIR
export BASE_DIR := $(shell cd ..;pwd)
endif
#
ifndef SUBD
export SUBD := $(notdir $(shell pwd))
endif
#
include $(BASE_DIR)/$(notdir $(BASE_DIR)).config
#
