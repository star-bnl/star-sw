# base_v.mk
########################################################################
#
include $(SLM_DIR)/globals.mk
#
include $(notdir $(shell pwd)).config
#
SUBDIRS += bin cdf doc idl inc lib solib src srcm wrk
SUBMAKES := $(addsuffix /Makefile,$(SUBDIRS))
#
