# srcm_v.mk - make variables for source files
########################################################################
#
ifndef SRCM_V_MK
export SRCM_V_MK += DONE
#
SRCMTYPES += %.F %.c %.cc
export SRCMFILES = $(sort $(filter $(SRCMTYPES), $(shell ls \
		$(SRCMDIR))))
#
SRCMOBJS = $(foreach d, $(SRCMTYPES), $(patsubst $(d),%.o, \
		$(SRCMFILES)))
export OBJS += $(filter %.o, $(SRCMOBJS))
#
endif #SRCM_V_MK
#
