# cdf_v.mk - make variables for asp/cdf
########################################################################
#
ifndef CDF_V_MK
export CDF_V_MK += DONE
#
CDFFILES := $(shell ls *.cdf)
CDFDRVS := $(CDFFILES:%.cdf=%.c)
#
CDFOBJS := $(notdir $(CDFFILES:%.cdf=%.o))
export OBJS += $(CDFOBJS)
#
KUIPC := $(shell which kuipc)
KUIPC_FLAGS := $(EMPTY)
#
endif #CDF_V_MK
#
