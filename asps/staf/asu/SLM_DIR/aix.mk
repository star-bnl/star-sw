# aix.mk - AIX flags for compilers, loaders, etc. and libraries
########################################################################
#
ifndef AIX_MK
export AIX_MK += DONE
#
# C PreProcessor
#export CPP := gcc -E -x c
export CPP := /usr/ccs/lib/cpp
export CPPFLAGS += $(INCDIRS:%=-I%)
export CPPFLAGS += -D$(shell uname) -D$(STAR_ARCH)
export CPPFLAGS += -Dextname
ifeq ($(DEBUG),$(TRUE))
export CPPFLAGS += -DDEBUG
endif
#
# FORTRAN 77
#
export FC := xlf23
export FFLAGS += -w -qextname -qsave -qfixed=132 -qctyplss -qintlog \
		-bD:600000000
ifeq ($(DEBUG),$(TRUE))
export FFLAGS += -g
endif
ifneq ($(PKOFF),$(TRUE))
ifneq ($(DEBUG),$(TRUE))
FFLAGS += -Pk -Wp,-tr=NV,-INTL,-SV=A,-CTYPLSS
else
FFLAGS += -Pk -Wp,-tr=NV,-INTL,-SV=A,-CTYPLSS,-F
endif
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
export LD := c++
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
export FOR_LIBS += -lxlf -lxlf90 -lm
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
endif #AIX_MK
#
#%.f: %.F
#	$(CPP) $(CPPFLAGS) $< | egrep -v '^#' > $*.f
%.o: %.F
	$(CPP) -P $(CPPFLAGS) $< > $*.f
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $*.f -o $*.o
#%.o: %.f
#	$(FC) -c $(FFLAGS) -o $@ $<
%.o: %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) -o $@ $<
%.i: %.cc
	$(CPP) -c $(CPPFLAGS) -o $@ $<
