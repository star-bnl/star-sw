# hpux.mk - HPUX flags for compilers, loaders, etc. and libraries
########################################################################
#
ifndef HPUX_MK
export HPUX_MK += DONE
#
# C PreProcessor
#export CPP := gcc -E -x c
export CPPFLAGS += -Dextname
export CPPFLAGS += $(INCDIRS:%=-I%)
export CPPFLAGS += -D$(shell uname | sed -e 's/-//g') -D$(STAR_ARCH)
ifeq ($(DEBUG),$(TRUE))
export CPPFLAGS += -DDEBUG
endif
#
# FORTRAN 77
#
export FC := f77
export FFLAGS += +es +e -K +ppu
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
export CXX := g++
export CXXFLAGS += -w 
ifeq ($(DEBUG),$(TRUE))
export CXXFLAGS += -g
endif
#
# Loader
export LD := CC
ifeq ($(STATIC),$(TRUE))
export LDFLAGS += -non_shared
else
export LDFLAGS += -L/usr/lib -L/lib
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
#export OS_LIBS += /lib/crt0.o
export OS_LIBS += -lcl -lisamstub -lnlsstubs -lc
#
# Fortran Libraries
export FOR_LIBS += -lf
#
# X-Motif Libraries
export XM_LIBS += 
#
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
endif #HPUX_MK
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
