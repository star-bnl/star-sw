ifndef STAF_MAKE_HOME
  STAF_MAKE_HOME := $(STAR)/mgr
endif
include $(STAF_MAKE_HOME)/MakeEnv.mk
include $(STAF_MAKE_HOME)/MakeArch.mk

CPPFLAGS += -DSTAF -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
GEA := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
FOR := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
GST := $(FOR72) $(FFLAGS)   $(EXEFLAGS)
GSC := $(CXX)   $(CXXFLAGS) $(EXEFLAGS)
CPP := $(CXX)   $(CXXFLAGS) $(CPPFLAGS)  
CC  := $(CC)    $(CFLAGS)   $(CPPFLAGS)   
FSL := $(FOR72) $(FFLAGS)   $(CPPFLAGS)   $(SOFLAGS)
LDS := $(SO)    $(SOFLAGS)

CMDS  = atlsim starsim gstar staf staf+ staf++ staf+++
SIZE  = 1 2 4 6 8 10 12 14 16 18 20 22 24 28 32 34 36
INCL  = -Iinc  -I$(STAR)/asps/staf/inc
SHELL := /bin/ksh
LIBSL :=
RMF   := /bin/rm     -f
LIB   = -L/usr/dt/lib -L/usr/SUNWspro/lib -ldl  -lM77 -lF77 -lsunmath -lmalloc
.SILENT:
.SUFFIXES:
.SUFFIXES: .sl .o .g .f .F .c .cc .cdf .rz
#
.g.sl:; geant3    $*   -o  $*.F
	$(FSL) -c $*.F -o  $*.o     $(INCL)
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)    $*.F     $*.o
#
.F.sl:; $(FSL) -c $*.F -o  $*.o     $(INCL)
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)             $*.o
#
.f.sl:; $(FSL) -c $*.f -o  $*.o
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)             $*.o
#
.c.sl:; $(CC)  -c $*.c -o  $*.o     $(INCL)
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)    $*.o
#
.cc.sl:;$(CPP) -c $*.cc -o $*.o     $(INCL)
	$(LDS)    $*.o -o  $*.sl    $(LIBCC)
	$(RMF)    $*.o
#
.o.sl:;	$(LDS)    $*.o -o  $*.sl    $(LIBCC)
#
.cdf.o:;kuipc     $*
	$(FOR) -c $*.f -o  $*.o
	$(RMF)    $*.f
.g.o:;  geant3    $*   -o  $*.F
	$(FOR) -c $*.F -o  $*.o     $(INCL)
	$(RMF)    $*.F
#
.F.o:;  $(FOR) -c $*.F -o  $*.o     $(INCL)
.f.o:;  $(FOR) -c $*.f -o  $*.o
.c.o:;  $(CC)  -c $*.c -o  $*.o
.cc.o:; $(CPP) -c $(INCL)  $*.cc -o $*.o
#
#
geant3: geant3.f; $(GEA) -o geant3 geant3.f `cernlib kernlib`
#
$(CMDS):ccsim.o fgsim.o agsim.o kgsim.o ggsim.o comisf.o comisc.o agdummy.o
	$(GSC) -o $@                   \
                      -DSTAF $(INCL)   \
                      ccsim.cc fgsim.o \
	agsim.o kgsim.o ggsim.o comisf.o comisc.o agdummy.o \
     -L./ -ltdm -lspx -lsoc -lasu -ltop -ltnt -lami -ldio -ldui -ldsl -ldsu \
	`cernlib geant321 pawlib graflib/Motif \
         packlib mathlib kernlib`  \
        $(LIB)
#
	echo '#!'$(PWD)'/$@'  > import.map
#	mv  /tmp/$@  ./$@
 
$(SIZE):
	sed -e '5,15s/8 000/$@ 000/; 5,15s/2 000/500/' fgsim.f > simsize.f
	$(FOR) -c simsize.f -o  fgsim.o
normal:
	$(FOR) -c fgsim.f   -o  fgsim.o
detm:
	$(RMF) detm.rz; echo "createdoc dzdoc.bank detm.rz; quit;" | dzedit
cleanup:
	/bin/rm -f *.o *.f last* *~ *.lst *.exp
	/bin/rm -f agsim.g agdummy.g kgsim.cdf comisc.* ccsim.cc
	/bin/rm -f cmzsave.dat paw.metafile dzedit.las
	ls -ltr
atlas:	geant3 atlsim detm cleanup
star:	geant3 gstar  detm cleanup
setup:
show:
	@echo LDS = $(LDS) 
	@echo FSL = $(FSL) 
	@echo RMF = $(RMF) 


 
 
 
