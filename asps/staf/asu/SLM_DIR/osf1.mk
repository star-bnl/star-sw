# osf1.mk - OSF1 flags for compilers, loaders, etc. and libraries
########################################################################
#
ifndef OSF1_MK
export OSF1_MK += DONE
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
export FFLAGS += -static -extend_source
ifeq ($(DEBUG),$(TRUE))
export FFLAGS += -g
endif
#
# C
export CC := gcc
export CFLAGS += -w 
ifeq ($(DEBUG),$(TRUE))
export CFLAGS += -g
endif
#
# C++
export CXX := c++
export CXXFLAGS += -w 
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
# Shared Libraries
export SO := ld -shared -o
export SOFLAGS += $(EMPTY)
#
# Object Libraries
export AR := ar
export ARFLAGS := slrv
export RANLIB := /bin/true
#
# OS Libraries
export OS_LIBS += $(EMPTY)
#
# Fortran Libraries
export FOR_LIBS += -lfor -lUfor -lots
#
# X-Motif Libraries
export XM_LIBS += 
#
# Load PAM Libraries
ifdef PAMS
export LLIBS += $(PAMS:%=-l%)
endif #PAMS
#
# Load ASP Libraries
ifdef ASPS
export LLIBS += $(ASPS:%=-l%)
endif #ASPS
#
export LLIBS += $(FOR_LIBS) $(XM_LIBS) $(OS_LIBS)
#
endif #OSF1_MK
#
%.o: %.f
	$(FC) -c $(FFLAGS) -o $@ $<
%.o: %.F
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) -o $@ $<
%.i: %.cc
	$(CPP) -c $(CPPFLAGS) -o $@ $<
