#
#	Determine Make variables
ALL_TAGS = $@
TARGET   = $@
ALL_DEPS = $^
1ST_DEPS = $<
NEW_DEPS = $?
FUL_DEPS = $+
STEM     = $*

WINDOWNT := $(OS)

#
# Current Working Directory
#
  CWD := $(shell pwd)

#
# Determine STAF main env variables.
#



ifndef AFS
  AFS := /afs
  ifdef WINDOWNT
    AFS :=//sol/afs
  endif  
endif  
ifndef AFS_RHIC
  AFS_RHIC := /afs/rhic
  ifdef WINDOWNT
    AFS_RHIC :=//sol/afs_rhic
  endif  
endif  
# 	

UNAMES := $(shell uname -s)
UNAMER := $(shell uname -r)
ifneq ($(UNAMES),HP-UX)
  UNAMEP := $(shell uname -p)
endif

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
ifdef STAR_SYS
  STAF_ARCH := $(STAR_SYS)
else
  STAF_ARCH := $(shell sys)
endif
#
#	Default value for STAF_SYS
ifndef STAF_SYS  
  QWE := $(wildcard ./asps)
  STAF_SYS := $(STAR)
  ifdef QWE
    STAF_SYS := $(CWD)
  endif
endif
STAF_SYS := $(strip $(wildcard $(STAF_SYS)))


#
#	Default value for STAF_ANA
ifndef STAF_ANA  
    STAF_ANA := $(STAR)  
endif
STAF_ANA := $(strip $(wildcard $(STAF_ANA)))


ifdef STAF_SYS
  STAF_SYS_INCS := $(STAF_SYS)/inc
#
#	default staf libs
  STAF_SYS_LIB := $(STAF_SYS)/.$(STAF_ARCH)/lib
  STAF_SYS_BIN := $(STAF_SYS)/.$(STAF_ARCH)/bin
  STIC := $(STAF_SYS_BIN)/stic
  STAFGEN := $(STAF_SYS_BIN)/stafGen
  PAMIGEN := $(STAF_SYS_BIN)/pamiGen.csh
  MAKE_PAMSWITCH := $(STAF_SYS_BIN)/make_pamSwitch

endif
