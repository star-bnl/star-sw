include ../make.inc

LT_HOME = ./LoopTools-2.1
LT_INCLUDE = $(LT_HOME)/include
LT_LIBRARY = $(LT_HOME)
LT_LIB = ooptools

SANC_HOME = .
SANC_LIBRARY_VERSION = 1_02
SANC_INCLUDE = .
SANC_LIBRARY = $(SANC_HOME)/SancLib_v$(SANC_LIBRARY_VERSION)
SANC_LIB     = SancLib_v$(SANC_LIBRARY_VERSION)

MODULES = ./modules

#####################################################################

.f.o:
	$(F77) -I$(LT_INCLUDE) -I$(SANC_INCLUDE) $(F77FLAGS) -c $< -o $*.o

.F.o:
	$(F77) -I$(LT_INCLUDE) -I$(SANC_INCLUDE) $(F77FLAGS) -c $< -o $*.o

LIBFILES = $(SANC_LIBRARY)/B0D_gen.o \
	   $(SANC_LIBRARY)/B0F_gen.o \
           $(SANC_LIBRARY)/B0P_gen.o \
           $(SANC_LIBRARY)/C0IR.o \
           $(SANC_LIBRARY)/C01_gen.o \
           $(SANC_LIBRARY)/D040.o \
           $(SANC_LIBRARY)/D040WZ.o \
           $(SANC_LIBRARY)/eett_aux.o \
           $(SANC_LIBRARY)/unibosconsts_bos.o \
           $(SANC_LIBRARY)/unibosconsts_fer.o \
           $(SANC_LIBRARY)/uniproconsts_fer.o \
           $(SANC_LIBRARY)/qedphotrenprop.o \
           $(SANC_LIBRARY)/xspenz.o \
           $(SANC_LIBRARY)/Delta_r.o \
           $(SANC_LIBRARY)/FermSE.o \
           $(SANC_LIBRARY)/BFF_gen.o \
           $(SANC_LIBRARY)/C0IRF.o \
           $(SANC_LIBRARY)/Jint.o \
           $(SANC_LIBRARY)/jaa_av.o \
           $(SANC_LIBRARY)/BoostRotationLib.o \
           $(SANC_LIBRARY)/GLK.o \
           $(SANC_LIBRARY)/InPrMassLib.o \
           $(SANC_LIBRARY)/InPrZeroLib.o \
           $(SANC_LIBRARY)/PhaseSpaceLib.o \
           $(SANC_LIBRARY)/PhysParamLib.o \
           $(SANC_LIBRARY)/TrMxMassLib.o \
           $(SANC_LIBRARY)/TrMxZeroLib.o \
           $(SANC_LIBRARY)/RandomLib.o \
           $(SANC_LIBRARY)/SANCLogo.o \
           $(SANC_LIBRARY)/vegasa.o 

FILES_1loop = \
	$(MODULES)/nc_ff_1414_2020.o \
	$(MODULES)/DownDown_TauTau_HA_NLO.o \
	$(MODULES)/nc_ff_1313_2020.o \
	$(MODULES)/UpUp_TauTau_HA_NLO.o

#####################################################################

all: tools lib main

help:
	@echo "make help|lib|main|tables|clean|clobber"

tools:
	make -C $(LT_HOME)

lib:	$(LIBFILES)
	$(AR) rv $(SANC_LIBRARY)/lib$(SANC_LIB).a $(LIBFILES)

main: main_2d_2tau_NLO.F SANCinterfaceF.o s2n_init.o $(FILES_1loop)
	$(F77) -o main_2d_2tau_NLO.exe main_2d_2tau_NLO.F \
	 SANCinterfaceF.o s2n_init.o $(FILES_1loop) \
	-L$(LT_LIBRARY) -l$(LT_LIB) -L$(SANC_LIBRARY) -l$(SANC_LIB)

tables:
	$(CC) -fPIC -o SANCtable.o -c SANCtable.cxx
	$(CC) -fPIC -o SANCinterface.o -c SANCinterface.cxx
	gfortran -lstdc++ $(F77FLAGS) -I$(LT_INCLUDE) -I$(SANC_INCLUDE) -o SANCinterface.exe SANCinterface.o SANCinterfaceF.o SANCtable.o s2n_init.o $(FILES_1loop) -L$(LT_LIBRARY) -l$(LT_LIB) -L$(SANC_LIBRARY) -l$(SANC_LIB)
	./SANCinterface.exe

../make.inc:
	@echo ""
	@echo "Please execute ./configure in TAUOLA directory first!"
	@echo ""
	@false

clean:
	rm -f *.out *.exe *.o $(MODULES)/*.o  $(MODULES)/*~

clobber: clean
	rm -f $(SANC_LIBRARY)/*.o $(SANC_LIBRARY)/*~ $(SANC_LIBRARY)/*.a
