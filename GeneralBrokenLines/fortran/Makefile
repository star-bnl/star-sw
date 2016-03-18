# #################################################################
# Makefile for GBLTST (test general broken lines)
#
# Author Claus Kleinwort, DESY, 2011
# #################################################################
# for gprof use -pg
#
FCOMP = gcc
F_FLAGS = -Wall -fno-automatic -fno-backslash -O3
# profiling: -pg
# gcc3: 
#F_LIBS = -lg2c -lfrtbegin
# gcc4: 
F_LIBS = -lgfortran -lgfortranbegin
DEBUG =          # e.g. -g
#
LOADER = gcc
L_FLAGS = -Wall -O3
#
# objects for this project
#
USER_OBJ_GBL = gbltraj.o gbltst.o gblnum.o Dbandmatrix.o mille.o
#
EXECUTABLES = gbltst
#

all:	$(EXECUTABLES)

# The single special one:
gbltst: ${USER_OBJ_GBL} Makefile
	$(LOADER) $(L_FLAGS) -o $@ ${USER_OBJ_GBL} $(F_LIBS)
#  
clean:
	rm -f *.o *~ */*.o */*~
#
clobber: clean 
	rm -f $(EXECUTABLES)

install: $(EXECUTABLES) #clean
	mkdir -p bin
	mv $(EXECUTABLES) bin

# Make the object files - depend on source and include file 
#
%.o : %.F Makefile
	${FCOMP} ${F_FLAGS} -c $< -o $@ 
%.o : %.f90 Makefile
	${FCOMP} ${F_FLAGS} -c $< -o $@ 
#
# ##################################################################
# END
# ##################################################################
