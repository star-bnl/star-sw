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
#
# Determine STAF_ARCH variable.
#
STAF_ARCH := $(shell sys)
#

UNAMES := $(shell uname -s)
UNAMER := $(shell uname -r)
ifneq (HP-UX,$(UNAMES))
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
override UNAMES := HPUX
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





