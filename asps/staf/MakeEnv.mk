#
#	Determine Make variables
ALL_TAGS = $@
ALL_DEPS = $^
1ST_DEPS = $<
NEW_DEPS = $?
FUL_DEPS = $+
STEM     = $*

#
# Current Working Directory
#
  CWD := $(shell pwd)

#
# Determine STAF main env variables.
#


ifndef STAF_HOME
  STAF_HOME := $(CWD)
endif

ifndef STAF_SYS_LEVEL
   STAF_SYS_LEVEL := dev
endif
# 	


UNAMES := $(shell uname -s)
UNAMER := $(shell uname -r)
UNAMEP := $(shell uname -p)
UNAMESRP := $(UNAMES)_$(UNAMER)_$(UNAMEP)
#
# Determine TULL_ARCH variable.
#
TULL_ARCH := unknown
ifeq (AIX,$(UNAMES))
  TULL_ARCH := aix
endif        

ifeq (HP-UX,$(UNAMES))
  TULL_ARCH := hpux
endif        

ifeq (IRIX_4,$(findstring IRIX_4,$(UNAMESRP)))
  TULL_ARCH := irix
endif        

ifeq (IRIX_5,$(findstring IRIX_5,$(UNAMESRP)))
  TULL_ARCH := irix
endif        

ifeq (IRIX64,$(UNAMES))
  TULL_ARCH := irix64
endif        

ifeq (Linux,$(UNAMES))
  TULL_ARCH := linux
endif        

ifeq (OSF1,$(UNAMES))
  TULL_ARCH := osf1
endif        

ifeq (SunOS_4,$(findstring SunOS_4,$(UNAMESRP)))
  TULL_ARCH := sun4
endif        

ifeq (SunOS_5,$(findstring SunOS_5,$(UNAMESRP)))
  ifeq (86,$(findstring 86,$(UNAMEP))) 
    TULL_ARCH := sun4os5pc
  else
    TULL_ARCH := sun4os5
  endif        
endif        

#
# Determine STAF_ARCH variable.
#
STAF_ARCH := $(shell sys)

#
# Determine Experiment variable.
#
ifndef EXPERIMENT
#	Is it STAR?
  QWE := $(strip $(filter /star/% /afs/rhic/star/%,$(CWD)))
  ifdef QWE
    EXPERIMENT :=STAR
  endif
#	Is it PHENIX?
  QWE := $(strip $(filter /phenix/% /afs/rhic/phenix/%,$(CWD)))
  ifdef QWE
    EXPERIMENT :=PHENIX
  endif
endif
  
#
#	Default value for STAF_SYS
ifndef STAF_SYS  
  ifeq (STAR,$(EXPERIMENT))
    STAF_SYS := /afs/rhic/common/pro  
  endif
  ifeq (PHENIX,$(EXPERIMENT))
    STAF_SYS := /afs/rhic/phenix/software/pro/sys  
  endif
endif


#
#	Default value for STAF_ANA
ifndef STAF_ANA  
  ifeq (STAR,$(EXPERIMENT))
    STAF_ANA := /afs/rhic/star/packages/pro  
  endif
  ifeq (PHENIX,$(EXPERIMENT))
    STAF_ANA := /afs/rhic/phenix/software/pro/ana  
  endif
endif



#	STAF system reference includes and libraries 
ifdef STAF_SYS
  QWE := $(strip $(wildcard $(STAF_SYS)))
  ifndef QWE
    QWE := $(shell echo $(STAF_SYS) Ignored: does not exist)
    STAF_SYS :=
  endif
endif

ifdef STAF_SYS
  STAF_SYS_INCS := $(STAF_SYS)/inc
  QWE := $(strip $(wildcard $(STAF_SYS_INCS)))
  ifndef QWE
#	complicated STAR case
    STAF_SYS_INCS := $(shell find $(STAF_SYS) -name inc -type d)
  endif
#
#	default staf libs
  STAF_SYS_LIBS := $(STAF_SYS)/lib
  QWE := $(strip $(wildcard $(STAF_SYS_LIBS)))
  ifndef QWE
    STAF_SYS_LIBS := $(STAF_SYS)/.$(STAF_ARCH)/lib
  endif
  
  STAF_SYS_LIBS := $(wildcard $(STAF_SYS_LIBS)/lib*.a)

  STAF_SYS_BIN := $(STAF_SYS)/bin
  QWE := $(strip $(wildcard $(STAF_SYS_BIN)))
  ifndef QWE
    STAF_SYS_LIBS := $(STAF_SYS)/.$(STAF_ARCH)/bin
  endif
  STIC := $(STAF_SYS_BIN)/stic
  STAFGEN := $(STAF_SYS_BIN)/stafGen
  PAMIGEN := $(STAF_SYS_BIN)/pamiGen.csh
  MAKE_PAMSWITCH := $(STAF_SYS_BIN)/make_pamSwitch

endif

#	STAF pams reference includes and libraries 
ifdef STAF_ANA
  QWE := $(strip $(wildcard $(STAF_ANA)))
  ifndef QWE
    QWE := $(shell echo $(STAF_ANA) Ignored: does not exist)
    STAF_ANA :=
  endif
endif

ifdef STAF_ANA
  STAF_ANA_INCS := $(STAF_ANA)/inc
  QWE := $(strip $(wildcard $(STAF_ANA_INCS)))
  ifndef QWE
#	complicated  case
    STAF_ANA_INCS := $(shell find $(STAF_ANA) -name inc -type d)
  endif
#
#	STAF PAM default libs
  STAF_ANA_LIBS:= $(STAF_ANA)/lib
  QWE := $(strip $(wildcard $(STAF_ANA_LIBS)))
  ifndef QWE
    STAF_ANA_LIBS := $(STAF_ANA)/.$(STAF_ARCH)/lib
  endif
   STAF_ANA_LIBS := $(wildcard $(STAF_ANA_LIBS)/lib*.a)
endif

ifndef STAF_CERN
  STAF_CERN := /cern/pro
endif
STAF_CERN_INCS := $(STAF_CERN)/include/cfortran 
STAF_CERN_LIBS := $(shell cernlib geant321 pawlib graflib mathlib)

ifndef STAF_UTILS
  STAF_UTILS := /afs/rhic/phenix/software/pro/utils
endif
STAF_UTILS_INCS := $(STAF_UTILS)/include
STAF_UTILS_LIBS := $(wildcard $(STAF_UTILS)/lib/lib*.a)



