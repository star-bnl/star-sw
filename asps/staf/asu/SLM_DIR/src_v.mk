# src_v.mk - make variables for source files
########################################################################
#
ifndef SRC_V_MK
export SRC_V_MK += DONE
#
SRCTYPES += %.F %.c %.cc
export SRCFILES = $(sort $(filter $(SRCTYPES), $(shell ls $(SRCDIR))))
#--use only _i.cc files - Use S.cc & C.cc only with CORBA.
CDFTYPES += %_def.c
CDFFILES := $(sort $(foreach d, $(CDFTYPES), $(patsubst %_def.cdf,$(d),\
		$(wildcard $(CDFDIR)/*_def.cdf))))
IDLTYPES += %_i.cc
IDLFILES := $(sort $(foreach d, $(IDLTYPES), $(patsubst %.idl,$(d), \
		$(wildcard $(IDLDIR)/*.idl))))
GENFILES += .sources_generated
GENFILES := $(CDFFILES) $(IDLFILES)
#
SRCOBJS = $(foreach d, $(SRCTYPES), $(patsubst $(d),%.o, \
		$(SRCFILES)))
export OBJS += $(filter %.o, $(SRCOBJS))
#
endif #SRC_V_MK
#
