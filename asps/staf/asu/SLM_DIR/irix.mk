# irix.mk - IRIX flags for compilers, loaders, etc. and libraries
########################################################################
#
ifndef IRIX_MK
export IRIX_MK += DONE
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
export FFLAGS += -static -extend_source -KPIC
ifeq ($(DEBUG),$(TRUE))
export FFLAGS += -g -check_bounds -trapuv -u -d_lines
endif
#
# C
export CC := gcc
export CFLAGS += -ansi -fpic -fPIC
ifeq ($(DEBUG),$(TRUE))
export CFLAGS += -g
endif
#
# C++
export CXX := CC
#export CXX := g++
export CXXFLAGS += -xansi -use_cfront
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
export FOR_LIBS += -lftn -lm -lc -lPW
#
# X-Motif Libraries
export XM_LIBS += -lXm -lXt -lX11
#
ifeq ($(MOTIF),$(TRUE))
export LOAD_LIBS += $(FOR_LIBS) $(XM_LIBS) $(OS_LIBS)
else
export LOAD_LIBS += $(FOR_LIBS) $(OS_LIBS)
endif
#
endif #IRIX_MK
#
%.o: %.f
	rm -f $@
	$(FC) -c $(FFLAGS) -o $@ $<
%.o: %.F
	rm -f $@
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.c
	rm -f $@
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<
%.o: %.cc
	rm -f $@
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) -o $@ $<
%.i: %.cc
	rm -f $@
	$(CPP) -c $(CPPFLAGS) -o $@ $<
%: %.o
	rm -f $@
	$(LD) $(LDFLAGS) -o $@ $<
