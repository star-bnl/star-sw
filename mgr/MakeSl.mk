ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif
include $(STAR_MAKE_HOME)/MakeEnv.mk
include $(STAR_MAKE_HOME)/MakeArch.mk

CPPFLAGS += -DSTAF -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
INCL  =  -I$(STAR)/inc -I$(STAR)/.share/tables
CPPFLAGS += $(INCL)

FOR := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
F77 := $(FOR)   $(FEXTEND)
CPP := $(CXX)   $(CXXFLAGS) $(CPPFLAGS)  
CC  := $(CC)    $(CFLAGS)   $(CPPFLAGS)   
LDS := $(SO)    $(SOFLAGS)

INCL  =  -I$(STAR)/inc -I$(STAR)/.share/tables
SHELL := /bin/ksh
RMF   := /bin/rm     -f
.SILENT:
.SUFFIXES:
.SUFFIXES: .sl .o .g .f .F .c .cc .cdf .rz
#
.g.sl:; geant3    $*   -o  $*.F
	$(FOR) -c $*.F -o  $*.o     
	$(LDS)    $*.o -o  $*.sl    
	$(RMF)    $*.F     $*.o
#
.F.sl:; $(F77) -c $*.F -o  $*.o    
	$(LDS)    $*.o -o  $*.sl    
	$(RMF)             $*.o
#
.f.sl:; $(F77) -c $*.f -o  $*.o
	$(LDS)    $*.o -o  $*.sl    
	$(RMF)             $*.o
#
.c.sl:; $(CC)  -c $*.c -o  $*.o    
	$(LDS)    $*.o -o  $*.sl    
	$(RMF)    $*.o
#
.cc.sl:;$(CPP) -c $*.cc -o $*.o     
	$(LDS)    $*.o -o  $*.sl    
	$(RMF)    $*.o
#
.o.sl:;	$(LDS)    $*.o -o  $*.sl    
#
.cdf.o:;kuipc     $*
	$(FOR) -c $*.f -o  $*.o
	$(RMF)    $*.f
.g.o:;  geant3    $*   -o  $*.F
	$(FOR) -c $*.F -o  $*.o     $(INCL)
	$(RMF)    $*.F
#
.F.o:;  $(F77) -c $*.F -o  $*.o     $(INCL)
.f.o:;  $(F77) -c $*.f -o  $*.o
.c.o:;  $(CC)  -c $*.c -o  $*.o
.cc.o:; $(CPP) -c $(INCL)  $*.cc -o $*.o
#
#
#
detm:
	$(RMF) detm.rz; echo "createdoc dzdoc.bank detm.rz; quit;" | dzedit
cleanup:
	/bin/rm -f *.o *.f last* *~ *.lst *.exp
	/bin/rm -f agsim.g agdummy.g kgsim.cdf comisc.* ccsim.cc
	/bin/rm -f cmzsave.dat paw.metafile dzedit.las
	ls -ltr
show:
	@echo LDS = $(LDS) 
	@echo FSL = $(FSL) 
	@echo RMF = $(RMF) 


 
 
 
