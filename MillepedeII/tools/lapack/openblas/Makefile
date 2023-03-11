# #################################################################
# Makefile for info_openblas (c++)
# #################################################################
#
LAPACK64 = OPENBLAS
LAPACK64_LIBS_DIR = /usr/lib64
LAPACK64_LIB = openblasp64
#
CCOMP = g++
C_FLAGS = -Wall -O3 
# profiling: -pg
C_LIBS = -L$(LAPACK64_LIBS_DIR) -l$(LAPACK64_LIB)
DEBUG =          # e.g. -g
#
LOADER = g++
L_FLAGS = -Wall -O3
#
# objects for this project
#
USER_OBJ_INFO = info_openblas.o 
#
EXECUTABLES = info_openblas
#

all:	$(EXECUTABLES)

# The single special one:
info_openblas: ${USER_OBJ_INFO} Makefile
	$(LOADER) $(L_FLAGS) -o $@ ${USER_OBJ_INFO} $(C_LIBS)
#  
clean:
	rm -f *.o *~ */*.o */*~
#
clobber: clean 
	rm -f $(EXECUTABLES)

# Make the object files - depend on source and include file 
#
%.o : %.cpp %.h Makefile 
	$(CCOMP) -c $(C_FLAGS) $(DEFINES) $(C_INCLUDEDIRS) $(DEBUG) -o $@ $<
#
# ##################################################################
# END
# ##################################################################
