# #################################################################
# Makefile for info_mkl (fortran90)
# #################################################################
#
# ### Define gcc to be used ###
#
GCC=gcc

LAPACK64 = MKL
LAPACK64_LIBS_DIR = <path to mkl_rt>
LAPACK64_LIB = mkl_rt
#
FCOMP = $(GCC)
F_FLAGS = -Wall -fautomatic -fno-backslash -O3 -g -cpp
#
CCOMP = $(GCC) 
C_FLAGS = -Wall -O3
C_INCLUDEDIRS =  # e.g. -I
#.
C_LIBS = -lgfortran -lm -L$(LAPACK64_LIBS_DIR) -l$(LAPACK64_LIB)
LOADER = $(GCC)
L_FLAGS = -Wall -O3
#
# objects for this project
#
USER_OBJ_INFO = info_mkl.o
#
# Make the executables
EXECUTABLES = info_mkl 
#

all:	$(EXECUTABLES)

info_mkl : ${USER_OBJ_INFO} Makefile
	$(LOADER) $(L_FLAGS) \
		-o $@ ${USER_OBJ_INFO} $(C_LIBS) 
#
clean:
	rm -f *.o *~ */*.o */*~ *.mod */*.mod
#
clobber: clean 
	rm -f $(EXECUTABLES)

# Make the object files - depend on source and include file 
#
%.o : %.f90 Makefile
	${FCOMP} ${F_FLAGS} -c $< -o $@ 
#	
# ##################################################################	
# END
# ##################################################################
