# inc_v.mk - make variables for include and header files
########################################################################
#
ifndef INC_V_MK
export INC_V_MK += DONE
#
INCTYPES += %.inc %.h %.hh
export INCFILES = $(sort $(filter $(INCTYPES), $(shell ls $(INCDIR))))
#--Remove .hh files - Use only with CORBA.
GENTYPES += %.inc %.h
GENFILES += .includes_generated
GENFILES := $(sort $(foreach d, $(GENTYPES), $(patsubst %.idl,$(d), \
		$(shell ls $(IDLDIR)/*.idl))))
ifneq ($(PAM),$(EMPTY))
LOADFILES := $(PAM)_load.cc $(PAM)_load.h
endif
#
endif #INC_V_MK
#
