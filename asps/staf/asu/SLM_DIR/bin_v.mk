# bin_v.mk - make variables for building shared & object bins
########################################################################
include $(SLM_DIR)/cern.mk
#
ifndef BIN_V_MK
export BIN_V_MK += DONE
#
SUBDIRS := $(STAR_ARCH)
#
SRCMTYPES += %.F %.c %.cc
SRCMFILES := $(sort $(filter $(SRCMTYPES), $(shell ls $(SRCMDIR))))
SRCMOBJS := $(filter %.o, $(foreach d, $(SRCMTYPES), \
		$(patsubst $(d),%.o, $(SRCMFILES))))
#
export PROGS := $(basename $(SRCMOBJS))
#
export OBJS := $(filter %.o, $(SRCMOBJS))
#
export VPATH += $(SRCMDIR)
#
export LOAD_LIBS := $(LOADLIBS:%=-l%)
export PAM_LIBS := $(PAMS:%=-l%)
export ASP_LIBS := $(ASPS:%=-l%)
export LIB_DIRS := $(LIBDIRS:%=-L%)
#
endif #BIN_V_MK
#
