# idl_v.mk - make variables for compiling IDL files
########################################################################
#
ifndef IDL_V_MK
export IDL_V_MK += DONE
#
IDLFILES := $(shell ls *.idl)
#
ifneq ($(ASP),$(EMPTY))
GENTYPES := %C.cc %S.cc %.hh
endif
ifneq ($(PAM),$(EMPTY))
GENTYPES := %_i.cc %.h %.inc
endif
GENFILES := $(foreach t, $(GENTYPES),$(patsubst %.idl,$(t), \
		$(IDLFILES)))
IDLSRCS := $(filter %.c %.F %.cc,$(GENFILES))
IDLINCS := $(filter %.h %.inc %.hh,$(GENFILES))
IDLJUNK := *.ih *.ic *.template
#
### - Use these for distribution via CORBA
### IDLOBJS := $(IDLSRCS:.cc=.o)
### export OBJS += $(IDLOBJS)
#
ORBIX_BINDIR	:= $(shell ls -d /usr/local/Orbix/bin)
ORBIX_LIBDIR	:= $(shell ls -d /usr/local/Orbix/lib)
ORBIX_INCDIR	:= $(shell ls -d /usr/local/Orbix/include)
#
ifneq ($(ASP),$(EMPTY))
#IDL             := $(shell ls -d $(ORBIX_BINDIR)/idl)
IDL		:= $(shell which idl)
IDLFLAGS        += -S -C -c C.cc -s S.cc
IDLFLAGS	+= $(addprefix -I, $(IDLDIRS))
IDLFLAGS	+= $(filter -D%,$(CPPFLAGS))
endif
ifneq ($(PAM),$(EMPTY))
IDL := $(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/bin/stic
IDLFLAGS 	:=
endif
##ifeq ($(shell ls $(IDL)),$(EMPTY))
##IDL             := $(STAF_DIR)/bin/idl.csh
##endif
#
### - Use these for distribution via CORBA
### export IDIRS	+= $(ORBIX_INCDIR)
#
endif #IDL_V_MK
#
