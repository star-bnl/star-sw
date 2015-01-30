# #################################################################
# Makefile for MillePede II (Fortran90) with possible input from C
# 
# Tested on - 64-bit SL5          with gcc version 4.4.4.
#           - 64 bit Ubuntu 11.10 with gcc version 4.6.1.
#
# See comments about different gcc versions inline to get a
# hint about the necessary adjustments.
# #################################################################
#
# ### Define gcc to be used ###
#
GCC=gcc
GCCVERS=$(shell $(GCC) -dumpversion)
# On SL5 replace default (gcc41) by gcc44
ifeq ($(findstring 4.1., $(GCCVERS)), 4.1.)
  GCC=gcc44
  GCCVERS=$(shell $(GCC) -dumpversion)  
endif
$(info Using gcc version $(GCCVERS))
#
# #################################################################
#
# ### Options ###
#
# All but 'yes' disables support of reading C-binaries:
SUPPORT_READ_C = yes
# If yes (and if SUPPORT_READ_C is yes), uses rfio, i.e. shift library, for IO,
# requires shift library and header to be installed at places defined here:
#
SUPPORT_C_RFIO =
# default path (by '=') to RFIO - overwrite if needed...
RFIO_INCLUDES_DIR = =
RFIO_LIBS_DIR = =
# yes
#
# If yes (and if SUPPORT_READ_C is yes and SUPPORT_C_RFIO is not yes),
# use zlib to read gzipped binary files:
SUPPORT_ZLIB = yes
# default path (by '=') to ZLIB - overwrite if needed
# requires z library and header to be installed at places defined here:
ZLIB_INCLUDES_DIR = =
ZLIB_LIBS_DIR = =
#
# If yes use multithreading with OpenMP (TM)
#SUPPORT_OPENMP = yes
# ompP profiler (http://www.ompp-tool.com, needs Opari for source-to-source instrumentation)
OMPP =
# kinst-ompp
#
# make install copies the binary to $(PREFIX)/bin
PREFIX = .
#
# #################################################################
#
FCOMP = $(OMPP) $(GCC)
#F_FLAGS = -Wall -fautomatic -fno-backslash -O3 -cpp
F_FLAGS = -Wall -fautomatic -fno-backslash -g -cpp
#
CCOMP = $(OMPP) $(GCC) 
#C_FLAGS = -Wall -O3 -Df2cFortran
C_FLAGS = -Wall -g -Df2cFortran
C_INCLUDEDIRS =  # e.g. -I
#.
ifeq ($(findstring 4.4., $(GCCVERS)), 4.4.)
# gcc44: 
  C_LIBS = -lgfortran -lgfortranbegin
else  
# gcc45, gcc46:
#  C_LIBS = -lgfortran -lm
   C_LIBS = /afs/rhic.bnl.gov/rcassoft/x8664_sl6/gcc492/lib/gcc/x86_64-unknown-linux-gnu/4.9.2/../../../../lib64/libgfortran.so -lm
# math library -lm or -lquadmath may be required  
endif
DEBUG =          # e.g. -g
#
ifeq ($(SUPPORT_OPENMP),yes)
# Multithreading with OpenMP (TM)
  C_LIBS  += -lgomp
  F_FLAGS += -fopenmp
endif
#
LOADER = $(OMPP) $(GCC)
#L_FLAGS = -Wall -O3
L_FLAGS = -Wall -g
#
# objects for this project
#
USER_OBJ_PEDE = mpdef.o mpdalc.o mpmod.o mpbits.o mptest1.o mptest2.o mille.o mpnum.o mptext.o mphistab.o \
	minresDataModule.o minresModule.o minresqlpDataModule.o minresqlpBlasModule.o minresqlpModule.o \
        randoms.o vertpr.o linesrch.o Dbandmatrix.o pede.o
#
# Chose flags/object files for C-binary support:
#
ifeq ($(SUPPORT_READ_C),yes)
  F_FLAGS += -DREAD_C_FILES
  USER_OBJ_PEDE += readc.o
  ifeq ($(SUPPORT_C_RFIO),yes)
    C_FLAGS += -DUSE_SHIFT_RFIO -I$(RFIO_INCLUDES_DIR)
    C_LIBS += -L$(RFIO_LIBS_DIR) -lshift
  else
    ifeq ($(SUPPORT_ZLIB),yes)
      C_FLAGS += -DUSE_ZLIB -I$(ZLIB_INCLUDES_DIR)
      C_LIBS += -L$(ZLIB_LIBS_DIR) -lz
    endif
  endif
endif
#  
#
# Make the executables
EXECUTABLES = pede 
#

all:	$(EXECUTABLES)

pede : 	${USER_OBJ_PEDE} Makefile
	$(LOADER) $(L_FLAGS) \
		-o $@ ${USER_OBJ_PEDE} $(C_LIBS) 
#
clean:
	rm -f *.o *~ */*.o */*~ *.mod */*.mod
#
clobber: clean 
	rm -f $(EXECUTABLES)

install: $(EXECUTABLES) #clean
	mkdir -p $(PREFIX)/bin
	mv $(EXECUTABLES) $(PREFIX)/bin

# Make the object files - depend on source and include file 
#
%.o : %.f90 Makefile
	${FCOMP} ${F_FLAGS} -c $< -o $@ 
%.o: %.c Makefile
	$(CCOMP) -c $(C_FLAGS) $(DEFINES) $(C_INCLUDEDIRS) $(DEBUG) -o $@ $<
#
# ##################################################################
# Module dependencies
mpbits.o:  mpdef.o mpdalc.o
mpdalc.o:  mpdef.o
mpmod.o:   mpdef.o
mpnum.o:   mpdef.o
pede.o:    mpdef.o mpmod.o mpdalc.o mptest1.o mptest2.o mptext.o
# ##################################################################
# END
# ##################################################################
