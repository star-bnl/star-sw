# lib_v.mk - make variables for building shared & object libs
########################################################################
#
ifndef LIB_V_MK
export LIB_V_MK += DONE
#
SUBDIRS := $(STAR_ARCH)
#
SRCTYPES += %.F %.c %.cc
SRCFILES := $(sort $(filter $(SRCTYPES), $(shell ls $(SRCDIR))))
SRCOBJS = $(foreach d, $(SRCTYPES), $(patsubst $(d),%.o, \
		$(SRCFILES)))
#
export OBJS := $(filter %.o, $(SRCOBJS))
export ALIB := lib$(BASE_NAME).a
export SOLIB := lib$(BASE_NAME).so
#
#export SOLIBDIR := $(LIBDIR lib=solib)
#
ifneq ($(OBJS),$(EMPTY))
LIBS := $(ALIB)
ifneq ($(SHARE),$(FALSE))
LIBS += $(SOLIB)
endif
else
LIBS := 
endif
#
LIB_ALL_TARGETS := $(OBJS) $(LIBS)
#
export VPATH := $(SRCDIR)
#
endif #LIB_V_MK
#
