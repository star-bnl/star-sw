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
export FFLAGS += -e -xl -sb -PIC
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
export CXX := CC
export CXXFLAGS += -w -fPIC
ifeq ($(DEBUG),$(TRUE))
export CXXFLAGS += -g
endif
#
# Loader
export LD := CC
ifeq ($(STATIC),$(TRUE))
export LDFLAGS += -non_shared
else
export LDFLAGS += $(EMPTY)
endif
#
# Shared Libraries				???UNTESTED???
#export SO := $(FC) -G -PIC -h ??? -R??? -o
export SO := $(FC) -G -PIC -o
export SOFLAGS += $(EMPTY)
#
# Object Libraries
export AR := ar
export ARFLAGS := srv
export RANLIB := /bin/true
#
# OS Libraries
export OS_LIBS += $(EMPTY)
#
# Fortran Libraries
SUNLIBDIR1 := /opt/SUNWspro/lib 
SUNLIBDIR2 := /opt/SUNWspro/SC3.0.1/lib
SUNLIBDIR1 := /vol/lic/SUNWspro-3.0/lib
SUNLIBDIR2 := /vol/lic/SUNWspro-3.0/SC3.0/lib
export FOR_LIBS += -L$(SUNLIBDIR1) -L$(SUNLIBDIR2) \
	-lM77 -lF77 -lsunmath -lm -lc \
	$(SUNLIBDIR2)/crtn.o
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
