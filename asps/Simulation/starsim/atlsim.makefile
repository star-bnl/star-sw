#############################################################################
#            Removed references to STAF 20060331 --maxim--                  #
#############################################################################
#  This makefile describes rules to create a chared library for a modules   #
#############################################################################
#.SILENT:
.SUFFIXES:
.SUFFIXES: .a .sl .o .f .F .F90 .FF90 .age .g .c .cc .cxx .cdf .y
TAGS       = $@
1ST_DEPS   = $<
ALL_DEPS   = $+
SHELL     := /bin/sh
RMF       := /bin/rm  -f
CWD       := $(shell pwd)
AFS       := /usr/afsws/bin/sys

UNAME     := $(shell uname)

ifndef STAR
  BASE    := $(word 1,$(ATLAS) $(ATLAS_ROOT) /afs/cern.ch/atlas)
  BASE    := $(word 1,$(wildcard $(BASE)/software) $(BASE))
  SOURCE  := $(BASE)/dist/$(CMT_VERSION)
else
  BASE    := $(STAR)
  SOURCE  := $(STAR)
endif

# STAR specific, this variable is defined
ifeq (1,$(USE_64BITS))
  MACHOPT := -m64 -fPIC
else
  MACHOPT := -m32 -fPIC
endif


SHARE     := share

ifdef STAR_HOST_SYS
ARCH      := $(STAR_HOST_SYS)
else
ARCH      := $(shell (test -x $(AFS) && $(AFS)) || (test -x /usr/bin/sys && sys) || uname -s)
endif

ifneq (1,$(words $(G3)))
G3        := $(shell which geant3 2>/dev/null)
endif
ifneq (1,$(words $(G3)))
G3        := $(shell which agetof.exe 2>/dev/null)
endif


O         := -o
SL        := sl
ifeq (Linux,$(UNAME))
 # test if gfortran exists
 CMPL := $(shell which gfortran 2>/dev/null)
 ifeq (,$(CMPL))
   CMPL := g77
   DFLG := -Df2cFortran
 else
   CMPL := gfortran
   DFLG := 
 endif

 SYS      := linux
 G3       += -v  1
 PP       := $(CMPL) $(MACHOPT) -E -P $(DFLG)
 F77      := $(CMPL) $(MACHOPT) -g -w -O2 -fno-second-underscore -fno-automatic
 PGF      := $(PGI)/linux86/bin/pgf90
 EX       := -ffixed-line-length-132
 CC       := gcc $(MACHOPT) -g -w -O2
 CPP      := g++ $(MACHOPT) -g -w -O2
 LDS      := g++ $(MACHOPT) -shared -o
 LD90     := $(PGI)/linux86/bin/pgf90 -v -shared -o
endif
ifeq (SunOS,$(UNAME))
 SYS      := solaris
 O        :=
# PP      := cpp  -P -C
 PP       := /usr/ccs/lib/cpp -P
 F77      := f77  -g  -PIC -Nl100 -Nx1000 -Nq1500 -silent
 EX       := -e
 CC       := cc   -g
 CPP      := CC   -g -DDEBUG -compat=4
 LDS      := ld   -G -o
endif
ifeq (HP-UX,$(UNAME))
 SYS      := hpux
# PP      := /usr/ccs/lbin/cpp -P  # take out line numbers
 PP       := fort77 -F -WF,-P
 F77      := fort77 -g +ppu +T +z +DAportable
 EX       := +es
 CC       := cc     -g +z -Aa     -D_HPUX_SOURCE
#CPP      := aCC    -g -w -z +Z   +DAportable
 CPP      := aCC    +O2 -Wc,-ansi_for_scope,on -z +p  -Ic++ +Z
 LDS      := ld    -b  -lm -o
endif
ifeq (IRIX64,$(UNAME))
 SYS      := irix
 PP       := f77  -E -woff2047
 O        := >
 F77      := f77  -O2 -n32 -mips3 -OPT:Olimit=0
 EX       := -extend_source
 CC       := cc   -g  -n32 -mips3
 CPP      := CC   -g  -n32 -mips3  -DDEBUG
 LDS      := CC   -shared -o
endif
ifeq (OSF1,$(UNAME))
# 20.01.00, from man f77:
# f77->cpp macros: (__)LANGUAGE_FORTRAN(__), (__)unix(__), __alpha, __osf__.
# -warn nousage suppresses warning messages about questionable prog.practices.
# -warn nouncalled suppresses warnings when a statement funct is never called.
 SYS      := osf
 O        := -o
 PP       := cpp -P -DLANGUAGE_FORTRAN -D__LANGUAGE_FORTRAN__ -Dunix -D__unix__
 F77      := f77 -fpe2 -warn nousage -warn nouncalled
 EX       := -extend_source
 CC       := cc   -g
 CPP      := cc   -g
 LDS      := ld   -shared -expect_unresolved "*" -o
endif
ifeq (AIX,$(UNAME))
 SYS      := aix
 PP       := xlf -F
 F77      := xlf -O -q maxmem=-1 -q extname -q source
 EX       := -e
 GST      := xlf -NQ20000 -bnoquiet -bkeepfile:starmain.o -bkeepfile:agstar.o
 CC       := cc  -g
 LDS      := ld  -bnoentry -bE:$*.exp import.map -bh:8 -T512 -H512 -o
 LDOPT    := -lxlf90 -lxlf -lm -lc
 FLT      = egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq
endif

ifdef lib
 LDS :=  ar -ruc
 SL  :=  a
endif
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CPP_TIME  := $(shell date +%H%M)
CPP_DATE  := $(shell date +%y%m%d)
CPP_TITLE := $(shell echo $(version)|sed -e 's%[-].*%%;s%^$$%Unknown%')
CPP_VERS  := $(shell echo $(version)|sed -e 's%^[^-]*%%;s%^-%%;s%^$$%Unknown%')
FLAGS     += -DATLAS_TYPE \
             -DCERNLIB_TYPE \
	     -Df2cFortran   \
	     -DCPP_IVER="1" \
	     -DCPP_DATE="$(CPP_DATE)"    \
	     -DCPP_TIME="$(CPP_TIME)"
FLAGS     += -DCPP_VERS="'$(CPP_VERS)'"  \
	     -DCPP_CVER="'$(CPP_VERS)'"  \
	     -DCPP_NAME="'$(CPP_TITLE)'" \
	     -DCPP_TITLE_CH="'$(CPP_TITLE)'"
ifdef LHCXX
  FLAGS   += -DCLHEP_MAX_MIN_DEFINED -DCLHEP_ABS_DEFINED -DCLHEP_SQR_DEFINED
endif
# - - - - - - - - - -  - - - - define directories - - - - - - - - - - - - - - -
input     := config
output    := .
ifneq (.,$(output))
  OUTDIR  := $(output)/
endif
INC_DIR1  := $(OUTDIR).$(SHARE)/inc
INC_DIR2  := $(OUTDIR).$(ARCH)/inc

## need to comply with the new STAR layout
OBJ_DIR = .$(STAR_HOST_SYS)/obj
LIB_DIR = .$(STAR_HOST_SYS)/lib

## don't need this just yet
# BINDIR = .$(STAR_HOST_SYS)/bin

WRK_DIR   := $(OUTDIR).$(SHARE)/wrk

## see above
# OBJ_DIR   := $(OUTDIR).$(ARCH)/obj

## commented out
#ifdef TARGET
#  WRK_DIR := $(OUTDIR).$(SHARE)/$(TARGET)
#  OBJ_DIR := $(OUTDIR).$(SHARE)/$(TARGET)
#endif
## replaced
# LIB_DIR   := $(OUTDIR).$(ARCH)/lib

ifneq (config,$(input))
  OLIBS   := $(wildcard $(LIB_DIR)/*.$(SL))
# OLIBS   := $(shell ls $(LIB_DIR)/*.$(SL) 2> /dev/null)
endif
# - - - - - - - - - - - - - - include directory paths - - - - - - - - - - - - -
ifdef STAR
 LNK_SRC  := $(wildcard $(CWD)/pams/*/idl/*)
 LNK_SRC  += $(wildcard $(CWD)/pams/*/inc/*)
 LNK_SRC  := $(filter-out %/CVS,$(LNK_SRC))
else               # atlas includes - domain/package/package or package/include
#LNK_SRC iterations: alldirs->x/p/p or p/include/p->nonempty dirs->first source
 ifeq (Linux,$(UNAME))
  ALLDIR  := $(shell find $(CWD)/* -type d -maxdepth 2 -follow 2> /dev/null)
 else
  ALLDIR  := $(subst /.,,$(wildcard $(CWD)/*/.))
  ALLDIR  += $(subst /.,,$(wildcard $(CWD)/*/*/.))
  ALLDIR  += $(subst /.,,$(wildcard $(CWD)/*/*/*/.))
 endif
 ifeq (,$(OLIBS))
  ifeq (Linux,$(UNAME))
  ALLDIR  += $(shell find $(SOURCE)/* -type d -maxdepth 2 2> /dev/null|grep -v '-')
  else
  ALLDIR  += $(subst /.,,$(wildcard $(SOURCE)/*/.))
  ALLDIR  += $(subst /.,,$(wildcard $(SOURCE)/*/*/.))
# ALLDIR  += $(subst /.,,$(wildcard $(SOURCE)/*/*/*/.))
  endif
 endif

 ALLDIR1  := $(filter-out -%,$(filter-out %-,$(subst -,- -,$(ALLDIR))))
 ALLDIRS  := $(filter-out %/CVS,$(filter-out %/cmt,$(ALLDIR1)))
 KKK      := $(words $(ALLDIRS))

 LNKCAND  := $(foreach p,$(ALLDIRS),\
             $(p)/$(notdir $(p))-*/include/$(notdir $(p))\
             $(p)/$(notdir $(p))-*/$(notdir $(p))\
             $(p)/include/$(notdir $(p))\
             $(p)/$(notdir $(p)))
 LLL      := $(words $(LNKCAND))
 LNKDIRA  := $(wildcard $(LNKCAND) )
 LNKDIRB  := $(subst / , ,$(dir $(foreach p,$(LNKDIRA),$(word 1,\
              $(wildcard $(addprefix $(p)/*.,inc h hh))))) )
 MMM      := $(words $(LNKDIRB))

# discard old empty  alternatives
 LNKDIRS  := $(LNKDIRB)
# LNKDIRS  := $(subst / , ,$(dir $(foreach p,$(LNKDIRB), \
              $(word 1,$(wildcard $(addsuffix /*.*,$(p)))))) )
 NNN      := $(words $(LNKDIRS))
 
 INCDIRS  := $(sort $(notdir $(LNKDIRS)))

 LNK_SRC  := $(foreach d,$(INCDIRS),$(word 1,$(filter %/$(d),$(LNKDIRS))))

# exceptions and additions
### LNK_SRC  += $(SOURCE)/geant3/geant321
 LNK_SRC  += $(wildcard $(SOURCE)/geant3/geant3-*/geant321)

 ifdef LHCXX
  LNK_SRC += $(LHCXX)/specific/@sys/CLHEP/new/include/CLHEP
 endif
 LNK_SPEC := $(INC_DIR2)/commons/machine.h
endif

#- - - - - - - - - - - - - - define input files and vpath - - - - - - - - - - -
inputp    := $(filter-out -%,$(input))
inputm    := $(filter     -%,$(input))
inputn    := $(filter-out %.cxx %.cdf %.cc %.c %.age %.g %.F %.F90 %.FF90 %.f %.y,$(inputm))
inputL    := $(filter -L%,$(inputn))
inputl    := $(filter -l%,$(inputn))
inputm    := $(filter-out $(inputL) $(inputl),$(inputm))
inputm    := $(subst  -X,,$(inputm))
inputm    := $(subst   -,,$(inputm)) CVS
inputlib  := $(subst -l,,$(filter %.a,$(inputl)))
inputl    := $(filter-out %.a,$(inputl))

inputa    := $(foreach dir,$(inputp),$(wildcard $(dir)) \
$(filter-out $(addprefix %/,$(wildcard $(dir))),$(wildcard $(SOURCE)/$(dir))))

#       exception one (STAR): targets under pams:
ifdef STAR
ifeq ($(words $(inputa)),0)
inputa    := $(foreach dir,$(addprefix pams/,$(inputp)),$(wildcard $(dir)) \
$(filter-out $(addprefix %/,$(wildcard $(dir))),$(wildcard $(SOURCE)/$(dir))))
endif
ifeq ($(words $(inputa)),0)
inputa    := $(foreach dir,$(addprefix pams/*/,$(inputp)),$(wildcard $(dir)) \
$(filter-out $(addprefix %/,$(wildcard $(dir))),$(wildcard $(SOURCE)/$(dir))))
endif
endif
#       exception two: local files listed directly
ifeq ($(words $(inputa)),0)
 inputa   := $(wildcard $(inputp).*)
endif

# extend directories to a list of files:
inputx    := $(wildcard $(inputa) $(addsuffix /*,$(inputa)))
inputy    := $(wildcard $(inputx) $(addsuffix /*,$(inputx)))

# ...   substruct negative input
inputd    := $(subst / , ,$(sort $(dir $(inputy))) )
inputb    := $(filter     $(addprefix %/,$(inputm)),$(inputd))
inputu    := $(filter-out $(addsuffix %, $(inputb)),$(inputy))
inputz    := $(filter-out $(addprefix %/,$(inputm)),$(inputu))

FILES_SRC := $(filter %.cxx %.cdf %.cc %.c %.age %.g %.F %.F90 %.FF90 %.f %.y,$(inputz))
FILES_F90 := $(filter %.F90 %.FF90,$(inputz))
ifneq (,$(FILES_F90))
 LDS      := $(LD90)
endif
VPATH     := $(sort $(dir $(FILES_SRC)))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ifdef STAR
FILES_EXT := $(addsuffix .o,$(basename $(filter $(SOURCE)%,$(FILES_SRC))))
FILES_EXT := $(subst pams,$(OBJ_DIR)/pams,$(FILES_EXT))
FILES_IDL := $(sort $(filter %.idl,$(inputz)))
FILES_CDF := $(sort $(filter %.cdf,$(inputz)))
ifneq (,$(FILES_IDL))
 FILES_MOD:= $(shell egrep -l 'interface.*:.*amiModule' $(FILES_IDL))
endif
FILES_IDL := $(filter-out $(FILES_MOD),$(FILES_IDL))
endif

# outlib should always be generated, otherwise "no rule" warning is not issued
outlib    := $(basename $(subst *,,$(notdir $(subst /*,,$(word 1,$(input))))))
outlib    := $(word 1,  $(subst -, ,$(outlib)))

# The first input element - even A.dummy - gives NAME to the library:
# absence of NAME will generate "no rule" warning in case of errenious input
extlib    := $(filter .start .init .dummy,$(suffix $(word 1,$(input))))
ifneq (0,$(words $(inputa) $(extlib)))
  NAME    := $(outlib)
endif

# generate a special "newlib" target for a new shared library
OLIB      := $(shell (test -f  $(LIB_DIR)/$(outlib).$(SL)) || echo newlib)

# - - - - - - - - - - - - include rules - - - - - - - - - - - - - - - - - - - -
INC_ALL   := -I$(INC_DIR2)
INC_ALL   += -I$(INC_DIR1)

ifdef STAR  # stic has its own inc_all   (STAR+STAF)
#               obsoleted:   INC_ALL += -I$(STAF)/inc
  INC_ALL += -I$(STAR)/.$(STAR_HOST_SYS)/include
  INC_ALL += -I$(STAR)/include
  INCLOC  := inc
else
  INCLOC  := include
endif
ifdef CERN_ROOT
  INC_ALL += -I$(CERN_ROOT)/include
  INC_ALL += -I$(CERN_ROOT)/include/cfortran
else
  INC_ALL += -I/cern/pro/include
  INC_ALL += -I/cern/pro/include/cfortran
endif
ifdef ROOTSYS
  INC_ALL += -I$(ROOTSYS)/include
endif
ifdef LHCXX
  INC_ALL += -I$(LHCXX)/share
  INC_ALL += -I$(LHCXX)/specific/@sys/CLHEP/dev/include
endif
INCS       =  $(FLAGS) -Iinclude
UPDIR      =  $(dir $(subst / ,,$(dir $+) ))
INCS      += -I$(UPDIR) -I$(UPDIR)$(INCLOC)
# next line is needed for ipatrec - it has includes without prefix :
INCS      += -I$(UPDIR)$(word 1,$(subst -, ,$(notdir $(subst / ,,$(UPDIR) ))))
INCS      += -I$(dir $+) -I$(dir $+)$(INCLOC)
INCS      += $(INC_ALL)
#                                     list all files:
LIST_s    := $(sort $(basename $(notdir $(FILES_SRC))))       # pure names
LIST_h    := $(basename $(notdir $(FILES_INC)))               # not done yet
LIST_o    := $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(LIST_s)))
LIST_i    := $(addprefix $(INC_DIR)/, $(addsuffix .inc, $(LIST_h)))

LIST_M    := $(sort $(basename $(notdir $(FILES_MOD))))
ifdef STAR
ifneq (,$(LIST_M))
LIST_cc   := $(addprefix $(WRK_DIR)/, $(addsuffix _i.cc,$(LIST_M)))
LIST_co   := $(addprefix $(OBJ_DIR)/, $(addsuffix _i.o,$(LIST_M)))
INIT_cc   := $(WRK_DIR)/$(outlib)_init.cc
LIST_co   += $(OBJ_DIR)/$(outlib)_init.o
endif
else
ifeq (,$(filter $(outlib),$(LIST_s)))
# no item in the sources has the library name, entry point has to be generated:
INIT_gg   := $(WRK_DIR)/$(outlib)_init.g
LIST_go   := $(OBJ_DIR)/$(outlib)_init.o
endif
endif

LNK_TRG   := $(addprefix $(INC_DIR1)/,$(sort $(notdir $(LNK_SRC))))
DIRS      := $(sort $(WRK_DIR) $(OBJ_DIR) $(LIB_DIR) $(INC_DIR1) $(INC_DIR2))
LIB_OLD   := $(shell ls -ltra .lib 2> /dev/null | awk '{print $$11}')

#-------------------------------- targets -----------------------------------
#  phony - not a file
.PHONY    : workdirs links sources objects library debug trace clean mark
all       : workdirs links sources library
clean     : erase all
trace     : show  all
debug     : show more all
sources   : $(LIST_cc) $(INIT_cc)
objects   : $(LIST_co) $(LIST_o) $(LIST_go)
library   : $(LIB_DIR)/$(outlib).$(SL)
$(LIST_o) : $(LIST_i)
#---------------------------- working dirs ----------------------------------
erase:;    rm -rf   $(LIST_cc) $(INIT_cc) $(LIST_co) $(LIST_o) $(LIST_go)\
                    $(INIT_gg) $(LNK_TRG) $(LIB_DIR)/$(outlib).$(SL)
workdirs:  $(DIRS)  $(OLIB)               #  $(G3TAR)
$(DIRS):;    mkdir -p $(TAGS)
agetof.def:; ln -sf $(G3DEF) agetof.def

newlib:
ifdef STAR
ifneq (0,$(words $(inputa)))
ifneq (,$(FILES_EXT))
	cd $(OBJ_DIR); for f in $(FILES_EXT); do rm -f `basename $$f`; \
			if [ -f $$f ]; then ln -s $$f .; fi; done ;
endif
endif
endif
#----------------------------- link dirs ------------------------------------
links:  $(LNK_TRG) $(LNK_SPEC)
ifneq ($(LIB_OLD),$(LIB_DIR))
	@rm -f .lib
	ln -s $(LIB_DIR) .lib
endif
$(LNK_TRG):
	@rm -f  $(TAGS)
	@ln -s  $(filter %/$(notdir $(TAGS)),$(LNK_SRC)) $(TAGS)
$(LNK_SPEC):
	@rm -rf   $(INC_DIR2)/commons
	@mkdir -p $(INC_DIR2)/commons
	ln -sf $(SOURCE)/commons/config/$(SYS).h  $(INC_DIR2)/commons/machine.h
	ln -sf $(SOURCE)/commons/commons-*/config/$(SYS).h  $(INC_DIR2)/commons/machine.h
#------------------------------ library -------------------------------------
$(LIB_DIR)/$(NAME).$(SL): $(LIST_o) $(LIST_co) $(LIST_go)
	$(LDS) $(LIB_DIR)/$(NAME).$(SL) $(LIST_o) $(LIST_co) $(LIST_go) \
               $(inputL) $(LLIBS) $(inputl) $(inputlib) 
#-L$(STAR_LIB) -lStarMagFieldNoDict
	@rm -f $(LIB_DIR)/tagsl*; touch $(LIB_DIR)/tagsl$(subst /,-,$(SOURCE))
#------------------------- idm  conversion rules ----------------------------
$(LIST_cc):  $(WRK_DIR)/%_i.cc: %.idl
	ls $+;  cp -p $+ $(WRK_DIR)/.;  cd $(WRK_DIR); stic -q \
	-I$(CWD)/$(INC_DIR1) -I$(STAR)/include \
	-I$(dir $+) -I$(dir $(subst / ,,$(dir $+) )) $*.idl || rm $*_i.cc
#------------------------- module interface ----------------------------
$(INIT_cc): $(FILES_MOD)
	@if [ -f $(TAGS) ]; then  rm $(TAGS) ; fi
	@echo '/* '$(NAME)' package interface  */'          >  $(TAGS)
	@echo '/* automatically generated file */'          >> $(TAGS)
ifneq ($(LIST_M),)
	@for p in $(LIST_M); do echo '#include "'$$p'.h"'   >> $(TAGS); done
	@echo 'extern "C" void $(NAME)_init_() ; '          >> $(TAGS)
	@echo '           void $(NAME)_init_() { '          >> $(TAGS)
	@for p in $(LIST_M); do echo "$${p}_load_ami(ami);" >> $(TAGS); done
	@echo '                      return ;  } '          >> $(TAGS)
endif
#------------------------- dummy entry point --------------------------------
$(INIT_gg): mark
	@rm -rf  $(TAGS).tmp
	@echo "* $(outlib)-lib interface for $(SOURCE)"     >  $(TAGS).tmp
	@echo "* automatically generated file, do not edit ">> $(TAGS).tmp
	@echo "subroutine  $(outlib)_init"                  >> $(TAGS).tmp
	@echo "+CDE,GCFLAG"                                 >> $(TAGS).tmp
	@echo "	if (idebug>0) print *,' using $(outlib) shlib ',">> $(TAGS).tmp
	@echo "		' created on ',CPP_DATE,' at ',CPP_TIME" >> $(TAGS).tmp
#	@echo in gg extlib = $(extlib)
ifeq (.start,$(extlib))
	@echo "	return"                                     >> $(TAGS).tmp
	@echo "	entry $(outlib)_start"                      >> $(TAGS).tmp
	@echo "	call  $(outlib)"                            >> $(TAGS).tmp
endif
	@echo "end"                                         >> $(TAGS).tmp
	@if [ ! -f $(TAGS) ]; then cp -p $(TAGS).tmp $(TAGS); fi
	@diff $(TAGS).tmp $(TAGS)>/dev/null || mv -f $(TAGS).tmp $(TAGS)
#----------------------- compilation generic rules --------------------------
$(OBJ_DIR)/%.o:   %.age
	@ls $+; cat $+ > $(WRK_DIR)/$*.F
	@rm -rf          $(WRK_DIR)/$*.g      $(OBJ_DIR)/$*.o
	$(PP) $(INCS)    $(WRK_DIR)/$*.F $(O) $(WRK_DIR)/$*.g
	@$(G3)           $(WRK_DIR)/$*.g  -o  $(WRK_DIR)/$*.f
	$(F77)        -c $(WRK_DIR)/$*.f  -o  $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.g
	@ls $+; cat $+ > $(WRK_DIR)/$*.F
	@rm -rf          $(WRK_DIR)/$*.g      $(OBJ_DIR)/$*.o
	$(PP) $(INCS)    $(WRK_DIR)/$*.F $(O) $(WRK_DIR)/$*.g
	@$(G3)           $(WRK_DIR)/$*.g  -o  $(WRK_DIR)/$*.f
	$(F77)        -c $(WRK_DIR)/$*.f  -o  $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.F90
	@ls $+; rm -f $(OBJ_DIR)/$*.o; cat $+ > $(WRK_DIR)/$*.F90
	@cat $(WRK_DIR)/$*.F90 > $(WRK_DIR)/$*.F
	$(PP) $(INCS)      $(WRK_DIR)/$*.F $(O) $(WRK_DIR)/$*.f90
	$(PGF) $(INCS)  -c $(WRK_DIR)/$*.f90 -o $(OBJ_DIR)/$*.o
#       @rm $(WRK_DIR)/../../$*.stb
$(OBJ_DIR)/%.o:   %.FF90
	@ls $+; rm -f $(OBJ_DIR)/$*.o; cat $+ > $(WRK_DIR)/$*.FF90
	@cat $(WRK_DIR)/$*.FF90 > $(WRK_DIR)/$*.F
	$(PP) $(INCS)      $(WRK_DIR)/$*.F $(O) $(WRK_DIR)/$*.f90
	@cat $(WRK_DIR)/$*.f90 > $(WRK_DIR)/$*.f
	$(PGF) $(INCS)  -c $(WRK_DIR)/$*.f -o $(OBJ_DIR)/$*.o
#	@rm $(WRK_DIR)/../../$*.stb
$(OBJ_DIR)/%.o:   %.F
ifdef STAR
ifeq (Linux,$(UNAME))
	@ls $+; cat $+ > $(WRK_DIR)/$*.F
	$(PP) $(INCS)    $(WRK_DIR)/$*.F $(O) $(WRK_DIR)/$*.g
	@$(RMF)          $(WRK_DIR)/$*.f
	@$(G3) -V 1 -V f $(WRK_DIR)/$*.g  -o  $(WRK_DIR)/$*.f
	if [ -f $(WRK_DIR)/$*.f ]; \
	then $(F77)   -c $(WRK_DIR)/$*.f  -o  $(OBJ_DIR)/$*.o \
          && $(RMF)      $(WRK_DIR)/$*.g; \
	else $(F77) $(INCS) -c $(WRK_DIR)/$*.F -o $(OBJ_DIR)/$*.o $(EX) \
          && $(RMF)      $(WRK_DIR)/$*.g; \
	fi
else
	@ls $+;  cat $+ > $(WRK_DIR)/$*.F; rm -f $(OBJ_DIR)/$*.o
	$(F77) $(INCS) -c $(WRK_DIR)/$*.F    -o  $(OBJ_DIR)/$*.o $(EX)
endif
else
	@ls $+;  cat $+ > $(WRK_DIR)/$*.F; rm -f $(OBJ_DIR)/$*.o
	$(F77) $(INCS) -c $(WRK_DIR)/$*.F    -o  $(OBJ_DIR)/$*.o
endif

$(OBJ_DIR)/%.o:   %.f;	
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(F77)                 -c $+ -o $(OBJ_DIR)/$*.o
$(LIST_go): $(OBJ_DIR)/%.o:   $(WRK_DIR)/%.g
	@ls $+
	@$(G3)                $(WRK_DIR)/$*.g  -o  $(WRK_DIR)/$*.F
	$(F77) $(FLAGS)    -c $(WRK_DIR)/$*.F  -o  $(OBJ_DIR)/$*.o
$(LIST_co): $(OBJ_DIR)/%.o:   $(WRK_DIR)/%.cc
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(CPP) $(INCS)         -c $+ -o $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.c
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(CC)  $(INCS)         -c $+ -o $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.cc
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(CPP) $(INCS)         -c $+ -o $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.cxx
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(CPP) $(INCS)         -c $+ -o $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.cdf
	@ls $+; kuipc $+ $(WRK_DIR)/$*.c
	@$(CC)        -c $(WRK_DIR)/$*.c  -o  $(OBJ_DIR)/$*.o

%.idl:; cd $(INC_DIR1); find $(SOURCE) -name $*.idl -exec ln -sf {} . \;
%.inc:; cd $(INC_DIR1); find $(SOURCE) -name $*.inc -exec ln -sf {} . \;
%.h:;   cd $(INC_DIR1); find $(SOURCE) -name $*.h   -exec ln -sf {} . \;
#---------------------------- debuging section --------------------------------
show:
	@echo "***************************************************************"
	@echo SYS       = $(SYS)
	@echo G3        = $(G3)
	@echo G3DEF     = $(G3DEF)
	@echo ARCH      = $(ARCH)
	@echo BASE      = $(BASE)
	@echo SOURCE    = $(SOURCE)
	@echo INC_DIR1  = $(INC_DIR1)
	@echo INC_DIR2  = $(INC_DIR2)
	@echo WRK_DIR   = $(WRK_DIR)
	@echo OBJ_DIR   = $(OBJ_DIR)
	@echo LIB_DIR   = $(LIB_DIR)
	@echo LIB_OLD   = $(LIB_OLD)
	@echo input     = $(input)
	@echo inputp    = $(inputp)
	@echo inputm    = $(inputm)
	@echo inputl    = $(inputl)
	@echo inputa    = $(inputa)
	@echo inputb    = $(inputb)
#	@echo inputx    = $(inputx)
#	@echo inputu    = $(inputu)
#	@echo inputy    = $(inputy)
#	@echo inputv    = $(inputv)
#	@echo inputw    = $(inputw)
#	@echo inputz    = $(inputz)
	@echo NAME      = $(NAME)
	@echo outlib    = $(outlib)
	@echo extlib    = $(extlib)
	@echo OLIB      = $(OLIB)
	@echo OLIBS     = $(OLIBS)
#	@echo '-------------------'
#	@echo ALLDIRS   = $(ALLDIRS)
#	@echo LNKCAN1   = $(LNKCAN1)
#	@echo LNKCAN2   = $(LNKCAN2)
#	@echo LNKCAN3   = $(LNKCAN3)
#	@echo LNKCAN4   = $(LNKCAN4)
	@echo LNKCAND   = $(LNKCAND)
	@echo LNKDIRA   = $(LNKDIRA)
	@echo LNKDIRB   = $(LNKDIRB)
	@echo INCDIRS   = $(INCDIRS)
	@echo KKK       = $(KKK)
	@echo LLL       = $(LLL)
	@echo MMM       = $(MMM)
#	@echo LNK_SRC   = $(LNK_SRC)
#	@echo LNK_TRG   = $(LNK_TRG)
	@echo NNN       = $(NNN)
	@echo '-------------------'
#	@echo INIT_cc   = $(INIT_cc)
#	@echo LIST_cc   = $(LIST_cc)
#	@echo LIST_co   = $(LIST_co)
	@echo INIT_gg   = $(INIT_gg)
	@echo LIST_go   = $(LIST_go)
	@echo FILES_SRC = $(FILES_SRC)
	@echo FILES_F90 = $(FILES_F90)
	@echo LDS = $(LDS)
more:
	@echo FILES_CDF = $(FILES_CDF)
	@echo FILES_EXT = $(FILES_EXT)
	@echo FILES_OXT = $(FILES_OXT)
	@echo FILES_MOD = $(FILES_MOD)
	@echo FILES_IDL = $(FILES_IDL)
	@echo INCS      = $(INCS)
	@echo VPATH     = $(VPATH)
	@echo LIST_o    = $(LIST_o)
	@echo LIST_i    = $(LIST_i)
	@echo LIST_s    = $(LIST_s)
#	@echo ALLDIRS   = $(ALLDIRS)
	@echo LNKCAND   = $(LNKCAND)
	@echo LNK_SRC   = $(LNK_SRC)
	@echo INCDIRS   = $(INCDIRS)

#############################################################################
# copyright Pavel Nevski, Victor Perevozchikov                BNL, 30/04/99 #
#############################################################################




