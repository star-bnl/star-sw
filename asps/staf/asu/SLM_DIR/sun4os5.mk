# sun4os5.mk - Solaris flags for compilers, loaders, etc. and libraries
########################################################################
#
ifndef SUN4OS5_MK
export SUN4OS5_MK += DONE
#
# C PreProcessor
#export CPP := gcc -E -x c
export CPPFLAGS += $(INCDIRS:%=-I%)
export CPPFLAGS += -D$(shell uname) -D$(STAR_ARCH)
ifeq ($(DEBUG),$(TRUE))
export CPPFLAGS += -DDEBUG
endif
#
# FORTRAN 77
#
export FC := f77
#17sep96---export FFLAGS += -e -xl -sb -PIC
export FFLAGS += -e -xl -PIC
ifeq ($(DEBUG),$(TRUE))
export FFLAGS += -g
endif
#
# C
export CC := gcc
export CFLAGS += -w -fPIC
ifeq ($(DEBUG),$(TRUE))
export CFLAGS += -g
endif
#
# C++
###export CXX := CC
###export CXXFLAGS += -w -fPIC
export CXX := g++
export CXXFLAGS :=
ifeq ($(DEBUG),$(TRUE))
export CXXFLAGS += -g
endif
#
# Loader
export LD := $(CXX)
ifeq ($(STATIC),$(TRUE))
export LDFLAGS += -non_shared
else
export LDFLAGS += $(EMPTY)
endif
#
# Shared Libraries				???UNTESTED???
#export SO := $(FC) -G -PIC -h ??? -R??? -o
#export SO := $(FC) -G -PIC -o
export SO := touch
export SOFLAGS += $(EMPTY)
#
# Object Libraries
export AR := ar
export ARFLAGS := srv
export RANLIB := /bin/true
#
# OS Libraries
export OS_LIBS := -lm -ldl
#
# Fortran Libraries
ifndef SUNW_ROOT
SUNW_LDIRS := /vol/lic/SUNWspro-3.0/libs /vol/lic/SUNWspro-3.0/SC3.0/lib
SUNW_LDIRS := /opt/SUNWspro/libs /opt/SUNWspro/SC4.0/lib
endif
ifndef SUNW_CRTN
SUNW_CRTN := /vol/lic/SUNWspro-3.0/SC3.0/lib/crtn.o
SUNW_CRTN := /opt/SUNWspro/SC4.0/lib/crtn.o
endif
export FOR_LIBS += $(addprefix -L,$(SUNW_LDIRS)) \
	-lM77 -lF77 -lsunmath -lm -lc -L/usr/ucblib -lucb \
	$(SUNW_CRTN)
#
# X-Motif Libraries
export XM_LIBS += -L/usr/openwin/lib -L/vol/pub/SunSDK/SUNWmotif/lib \
	-L/vol/packages/X11R5/lib -lXm -lXt -lX11 -lm
#
ifeq ($(MOTIF),$(TRUE))
export LOAD_LIBS += $(FOR_LIBS) $(XM_LIBS) $(OS_LIBS)
else
export LOAD_LIBS += $(FOR_LIBS) $(OS_LIBS)
endif
#
endif #SUN4OS5_MK
#
%.o: %.f
	$(FC) -c $(FFLAGS) -o $@ $<
%.o: %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) -o $@ $<
