#######################################################################
#
#   Makefile for DPMJET-III
#
#######################################################################
#choose gnu, intel or g95
CVendor = "GNU"
Config?="Release"

#######################################################################
#
#   compiler
#
#######################################################################

ifeq ($(CVendor),"GNU")
	#  GNU
	FC := $(or $(FC), gfortran)
	F2PY_C = gnu95
	ifeq ($(OS),Windows_NT)
		F2PY_CCONF = --compiler=mingw32 --fcompiler=$(F2PY_C)
	else
		F2PY_CCONF = --compiler=unix --fcompiler=$(F2PY_C)
	endif
else
	#  Intel
	FC = ifort
	F2PY_C = intelem
endif

#######################################################################
#
#   compiler options for different platforms
#
#######################################################################
ifeq ($(CVendor),"GNU")
	ifeq ($(Config),"Debug")
		# GNU Debug
		OPT = -fPIC -Wall -fbounds-check -O0 -g \
			  -ffpe-trap=invalid,zero,overflow -Wuninitialized
		OPTF90 = -fPIC -Wall -fbounds-check -O0 -g \
			  -ffpe-trap=invalid,zero,overflow -Wuninitialized \
			  -fno-second-underscore
		#OPT = -fPIC -Wall -Wno-uninitialized -Wno-unused-variable -O3 -g -ffpe-trap=invalid,zero,overflow
	else
		# GNU Release
		OPT = -O3 -Wno-uninitialized -fPIC
		OPTF90 = -O3 -Wno-uninitialized -fPIC -fno-second-underscore 
	endif
else
	ifeq ($(Config),"Debug")
	# Intel Debug (-gen-interfaces -warn interfaces)
		OPT = -check bounds -O0 -g -check pointer -fpe0 -traceback
		OPTF90 = -check bounds -O0 -g -check pointer -fpe0 -traceback \ 
			 -cpp -ffree-form -Wobsolescent -fno-second-underscore
	else
		# Intel Release
		OPTF90 = -fast -fpe0 \ 
		      -cpp -ffree-form -Wobsolescent -fno-second-underscore
		OPT = -fast -fpe0	
	endif
endif

#######################################################################
#
#   F2PY
#
#######################################################################
#general version for signature file extraction and linking
PYTHON_EXE := $(or $(PYTHON_EXE), python3)

#general version for signature file extraction and linking
ifeq ($(Config),"Debug")
	F2PY = $(PYTHON_EXE) -m numpy.f2py
else
	F2PY = $(PYTHON_EXE)  -m numpy.f2py --quiet
endif
#Linker
LD = $(FC)
#additional flags for linker
F2PY_L = $(F2PY) 

# Directories
WORK_DIR = $(CURDIR)
LIB_DIR?="$(WORK_DIR)/lib"
# New line define
define \n


endef
#######################################################################
#
#   Files
#
#######################################################################
PHOJET_INCS = ./include/phojet
PHOJET_SRCS :=$(wildcard ./src/phojet/*.f)
PHOJET_OBJS :=$(PHOJET_SRCS:.f=.o)
DPMJET_INCS = ./include/dpmjet
DPMJET_FLUKA_INCS = ./include/flinclude #./flukaadd
DPMJET_SRCS :=$(wildcard ./src/dpmjet/*.f)
DPMJET_OBJS :=$(DPMJET_SRCS:.f=.o)
PYTHIA_INCS = ./include/pythia
PYTHIA_SRCS :=$(wildcard ./src/pythia/*.f)
PYTHIA_OBJS :=$(PYTHIA_SRCS:.f=.o)

APP_SRCS :=$(wildcard ./src/exe/*.f)
APP_OBJS :=$(APP_SRCS:.f=.o)
APP_EXE :=$(APP_OBJS:.o=)
APP_EXE :=$(subst ./src/exe/,,$(APP_EXE))

DUMMY_SRCS :=$(wildcard ./common/*.f)
DUMMY_OBJS :=$(DUMMY_SRCS:.f=.o)

# Portability (I know that this is insane...)
ifeq ($(OS),Windows_NT)
  DEL_COMMAND = del /q /f
  MKDIR_COMMAND = if not exist "$(LIB_DIR)" mkdir
  COPY_COMMAND = copy /b
  COPY_DUMP = > nul  2>&1
  CAT_COMMAND = type
  EXESUFX = .exe
  PATHSEP2=\\
  PATHSEP=$(strip $(PATHSEP2))
  # Shared library suffix
  LEXT?=$(shell $(PYTHON_EXE) -c "import sysconfig; print('.cp' + sysconfig.get_config_var('py_version_nodot') + '-' + sysconfig.get_platform().replace('-','_') + sysconfig.get_config_var('EXT_SUFFIX'))")
  space := $(null) #
  comma := ,
  
  PHOJET_SRCS_CMMA := $(subst $(space),$(comma),$(strip $(PHOJET_SRCS)))
  DPMJET_SRCS_CMMA := $(subst $(space),$(comma),$(strip $(DPMJET_SRCS)))
  PYTHIA_SRCS_CMMA := $(subst $(space),$(comma),$(strip $(PYTHIA_SRCS)))
  DUMMY_SRCS_CMMA := $(subst $(space),$(comma),$(strip $(DUMMY_SRCS)))
  
  PHOJET_SRCS_CMMA := $(subst /,\,$(strip $(PHOJET_SRCS_CMMA)))
  DPMJET_SRCS_CMMA := $(subst /,\,$(strip $(DPMJET_SRCS_CMMA)))
  PYTHIA_SRCS_CMMA := $(subst /,\,$(strip $(PYTHIA_SRCS_CMMA)))
  DUMMY_SRCS_CMMA := $(subst /,\,$(strip $(DUMMY_SRCS_CMMA)))
  
  PYF_SRCS := $(PHOJET_SRCS_CMMA) $(PYTHIA_SRCS_CMMA) $(DPMJET_SRCS_CMMA) $(DUMMY_SRCS_CMMA)
else
  DEL_COMMAND = rm -rf
  MKDIR_COMMAND = mkdir -p
  COPY_COMMAND = cp
  COPY_DUMP =
  CAT_COMMAND = cat
  EXESUFX = 
  PATHSEP=/
  LEXT?=$(shell python -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))")
  PYF_SRCS := $(PHOJET_SRCS) $(PYTHIA_SRCS) $(DPMJET_SRCS) $(DUMMY_SRCS)
endif

DPMJET_FUNCS = pho_event dt_init dt_kkinc \
idt_icihad dt_xsglau pycomp dt_initjs dt_rndmst dt_rndm dt_inucas idt_ipdgha dt_evtout
DPMJET_FUNCS += pho_init pho_setpar poevt1 poevt2 pho_pname pho_pmass pho_setmdl \
pho_setpdf pycomp pho_xsect pho_borncs pho_harmci pho_fitout pho_mcini pho_ptcut \
pytune pho_rregpar pho_sregpar pho_prevnt ipho_pdg2id ipho_id2pdg pho_harint \
impy_openlogfile impy_closelogfile pho_harxto pho_harxpt pho_setpcomb \
dt_phoxs dt_xshn dt_flahad dt_title pho_ghhias

INCLU = -I$(PYTHIA_INCS) -I$(PHOJET_INCS) -I$(DPMJET_INCS) -I$(DPMJET_FLUKA_INCS)

pylib = dpmjetIII191$(LEXT)

all: exe 

.PHONY: pylib
pylib: $(pylib)

$(pylib): lib/libDPMJET.a common/dpmjetIII191.pyf
	$(F2PY) -c $(F2PY_CCONF) --opt="$(OPT)" \
	     $(INCLU) common/dpmjetIII191.pyf $(DPMJET_OBJS) $(PHOJET_OBJS) $(PYTHIA_OBJS) $(DUMMY_OBJS)

.PHONY: install
install: $(pylib)
	$(COPY_COMMAND) *$(LEXT) $(LIB_DIR)

.PHONY: exe
exe: $(APP_OBJS) lib/libDPMJET.a
	$(foreach a, $(APP_EXE), $(LD) -o bin/$(a) ./src/exe/$(a).o -Llib -lDPMJET ${\n})

common/dpmjetIII191.pyf:
	$(CAT_COMMAND) $(PYF_SRCS) > f2pytemp.f
	gfortran -E -cpp f2pytemp.f > f2py_cpp.f
	$(F2PY) -m dpmjetIII191 -h common/dpmjetIII191.pyf \
	--include-paths $(DPMJET_INCS):$(PHOJET_INCS):$(PYTHIA_INCS):$(DPMJET_FLUKA_INCS) \
	--overwrite-signature only: $(DPMJET_FUNCS) : f2py_cpp.f
	$(DEL_COMMAND) f2pytemp.f f2py_cpp.f f2pytemp.s

lib/libDPMJET.a:  $(PHOJET_OBJS) $(PYTHIA_OBJS) $(DPMJET_OBJS) $(DUMMY_OBJS)
	ar -crs lib/libDPMJET.a $(DPMJET_OBJS) $(PHOJET_OBJS) $(PYTHIA_OBJS) $(DUMMY_OBJS)

.f.o:
	$(FC) -c -cpp $(CPPFLAGS) $(OPT) $(INCLU) -o $@ $<   

.PHONY: clean
clean:
	$(DEL_COMMAND) lib$(PATHSEP)libDPMJET.a *.so *.pyd common$(PATHSEP)*.o *.dSYM $(COPY_DUMP)
	$(DEL_COMMAND) *.o src$(PATHSEP)pythia$(PATHSEP)*.o src$(PATHSEP)phojet$(PATHSEP)*.o src$(PATHSEP)dpmjet$(PATHSEP)*.o src$(PATHSEP)exe$(PATHSEP)*.o $(COPY_DUMP)
	$(DEL_COMMAND) *.s src$(PATHSEP)pythia$(PATHSEP)*.s src$(PATHSEP)phojet$(PATHSEP)*.s src$(PATHSEP)dpmjet$(PATHSEP)*.s src$(PATHSEP)exe$(PATHSEP)*.s $(COPY_DUMP)
	$(DEL_COMMAND) $(addprefix bin$(PATHSEP),$(addsuffix $(EXESUFX), $(APP_EXE))) $(COPY_DUMP)

.PHONY: distclean
distclean: clean
	$(DEL_COMMAND) common$(PATHSEP)dpmjetIII191.pyf
