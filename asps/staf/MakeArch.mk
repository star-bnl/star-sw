#  $Log: MakeArch.mk,v $
#  Revision 1.7  1998/05/01 12:21:27  fisyak
#  Test automail
#
#  Revision 1.6  1998/05/01 12:16:32  fisyak
#  Test automail
#
#  Revision 1.5  1998/04/06 00:00:52  fisyak
#  hpux modification
#
#  Revision 1.4  1998/04/02 21:40:50  fisyak
#  Add grep
#
#  Revision 1.3  1998/04/01 18:42:34  fisyak
#  Add hpux
#
#  Revision 1.2  1998/03/29 23:19:25  fisyak
#  RL98c fixes
#
#  Revision 1.1  1998/03/16 00:47:38  fisyak
#  V.Perev. Makefiles
#
#  Revision 1.4  1998/02/13 14:18:20  fisyak
#  Simplify Makefile, reduce SLibrary
#
#  Revision 1.3  1998/02/12 13:35:09  fisyak
#  Add versioning, new Makefile with domain/package libraries
#
#  Revision 1.2  1998/02/10 00:06:10  fisyak
#  SL98a second version
#
#  Revision 1.1  1998/01/31 23:32:52  fisyak
#  New Environment variables
#
#  Revision 1.2  1998/01/30 12:42:16  fisyak
#  Save changes before moving to SL97b
#
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#  Revision ?.?.?.?  1998/02/07           perev
#
#          Last modification $Date: 1998/05/01 12:21:27 $ 
#. default setings
#include $(STAF_HOME)/MakeEnv.mk
#
#	Determine Make   variables
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
ifndef STAF_ARCH
   STAF_ARCH := $(shell sys)
endif
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





GREP := grep
#GCC := /afs/rhic/opt/rhic/bin/gcc
GCC      :=  gcc
CC       :=  $(GCC)
CFLAGS   := -fpic -w
#CXX      :=  g++
CXX      :=  gcc
CXXFLAGS := -fpic -w
FC 	 := f77
AR       := ar
ARFLAGS  := rvu
LD 	 := $(CXX)
SO	 := $(CC)
SOFLAGS	 := 
SOEXT    := so
CLIBS    :=
FLIBS    :=
CPPFLAGS := -D$(UNAMES) -D$(STAF_ARCH) -D$(TULL_ARCH) 
OSFID    :=
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))

#  case IBMRT
#  ====================

  OSFID :=aix
  ifdef GCC.
    CXXFLAGS :=  -fsigned-char -w  
    CFLAGS   :=  -fsigned-char -w 
    LDFLAGS  := 
    SOFLAGS  :=  -shared 
    CLIBS  :=  -lXm -lXt -lX11 -lg++ -lm -lld
  else
    CXX    := xlC
    CC     := xlC
    LD     := $(CXX)
    SO     := ???
    SOFLAGS := ???
    CXXFLAGS := -w -qchars=signed -qnoro -qnoroconst 
    CFLAGS   := -w -qchars=signed -qnoro -qnoroconst 
    LDFLAGS  := 
    CLIBS  :=  -lXm -lXt -lX11  -lld  -lm -lc -lcurses
  endif

  FC         = xlf
  FLIBS  :=  -lxlf90 -lxlf
  FFLAGS  := -qextname  -qrndsngl -qcharlen=6000 -e
endif 

ifneq (,$(findstring $(STAF_ARCH),i386_linux2))
#    case linux
#  ====================
  OSFID :=lnx
ifndef PGI_dummy
  FC       := g77
  LD       := $(CXX)
  SO	   := $(CXX)
  GCC      := $(GCC) -Dlynx
  CXXFLAGS := -fPIC -Dlynx -ansi
  CFLAGS   := -fPIC -Dlynx -ansi
  LDFLAGS  := 
  SOFLAGS  := -shared 
  CLIBS    := -L/usr/X11R6/lib -lXpm -lXt -lXext -lX11 -lg++ -lm -ldl -rdynamic
# CLIBS    := -L/usr/X11R6/lib -lXm -lXpm -lXt -lXext -lX11 -lg++ -lm -ldl -rdynamic
  FLIBS    := -lU77
  FFLAGS   := -export -dynamic -fno-second-underscore -e
#              -w -Nx800 -NC200 
else
  FC       := /usr/pgi/linux86/bin/pgf77
#  CC       := /usr/pgi/linux86/bin/pgcc
#  CXX      := /usr/pgi/linux86/bin/pgCC
   GCC      := $(GCC) -Dlynx
  CXXFLAGS   := -Dlynx  
  CFLAGS    :=  -Dlynx
  LD       := $(CXX)
  LDFLAGS    := 
  SOFLAGS    :=  -shared 
  CLIBS    :=  -L/usr/X11R6/lib -lXm -lXpm _-lXt -lXext -lX11 -lg++ -lm -ldl -rdynamic
  FLIBS    := -lU77
  FFLAGS   := -DPGI -w -Mextend

endif

endif

ifneq (,$(findstring $(STAF_ARCH),alpha_osf1 alpha_osf32c alpha_dux40))
#    case "alpha":
#  ====================
  OSFID := osf
  ifdef GCC.
    CXXFLAGS := -w  -D__osf__ -D__alpha 
    CFLAGS   := -w  -D__osf__ -D__alpha 
    LDFLAGS  :=
    SOFLAGS  := -shared 
    CLIBS  := -lXm -lXt -lX11 -lg++ -lm -lPW -lm -lm_4sqrt -lots -lc
 
 else
    CXX    :=cxx
    CC     :=cc
    LD     :=$(CXX)
    SO     :=$(CXX)
    CXXFLAGS := -w -D__osf__ -D__alpha -Dcxxbug -DALPHACXX 
    CFLAGS   := -w  
    LDFLAGS  := 
    SOFLAGS  :=  -call_shared -expect_unresolved '*'
    CLIBS  :=  -lXm -lXt -lX11 -lm -lPW -lm -lm_4sqrt -lots -lc
 
  endif

  FLIBS    :=  -lUfor -lfor -lFutil 
  FFLAGS   := -pic  -static -fpe2 -extend_source

endif

ifneq (,$(findstring $(STAF_ARCH),hp_ux102 hp700_ux90))

#    case "hp":
#  ====================
  OSFID :=hpux

    CXX     := aCC
    CC      := cc
    FC      :=fort77
    LD      := $(CXX)
    SO      := $(CXX)
    CXXFLAGS:= -w -z +Z +DAportable -Dextname                  # P.Nevski
    CFLAGS  := +z -Aa +DAportable -D_HPUX_SOURCE -Dextname     # P.Nevski 
    FFLAGS  += +DAportable +U77 +ppu +B +Z +es 
    LDFLAGS := -z -Wl,+s -Wl,-E 
    SOFLAGS := -b -z -Wl,+vnocompatwarnings 
    CLIBS   := -lm -lPW -ldld
#   CLIBS   := -L/usr/lib/Motif1.2 -lXm -lXt -lX11 -lm -lPW -ldld
#   CLIBS   := -lm -lPW -ldld
    SOEXT   := sl
    FLIBS   := /opt/fortran/lib/libU77.a 
    CFLAGS  += 
endif



ifneq (,$(findstring $(STAF_ARCH),sgi_52 sgi_53))
#  ====================
  OSFID := sgi
  GREP  := /afs/rhic/asis/sgi_52/usr.local/bin/ggrep
  FFLAGS    := -Nn20000 -static -trapuv  -extend_source
  CC        :=    cc
  CFLAGS    := -signed -fullwarn
  CXX       :=   CC
  CXXFLAGS  := -signed  -fullwarn #-D_STYPES #. add _STYPES due to ulong in stream.h ?
  ARFLAGS   :=   slrvu
  LD        :=   $(CXX)
  LDFLAGS   :=
  SO        :=   $(CXX)
  SOFLAGS   :=  -shared
  CLIBS     := -lsun  -lm -lc -lPW -lXext
  FLIBS     :=   -lftn 

endif

ifneq (,$(findstring $(STAF_ARCH),sgi_64 ))

  OSFID := sgi
  FFLAGS    :=  -32 -Nn20000 -static -trapuv -extend_source
  CC        :=    cc
  CFLAGS    :=  -32  -fullwarn	
  CXX       :=    CC
  CXXFLAGS  :=  -32 -fullwarn
  LD        :=   $(CXX)
  LDFLAGS   :=  -32 
  SO        :=   $(CXX)
  SOFLAGS   :=  -32 -shared
  CLIBS     :=  -lsun  -lm -lc -lPW -lXext
  FLIBS     :=  -lftn 

endif


ifneq (,$(findstring $(STAF_ARCH),sun4x_55 sun4x_56))
 
  OSFID := sun
  GCC   := cpp -B -C
# GCC   := cc -xM1
  CC :=  /opt/SUNWspro/bin/cc
  CXX := /opt/SUNWspro/bin/CC
  LD  := $(CXX)
  SO  := $(CXX)
  FC  := /opt/SUNWspro/bin/f77

  FFLAGS   := -xl -KPIC -w -e
  CFLAGS   := -KPIC 
  CXXFLAGS := -KPIC 
  LDFLAGS  :=
  SOFLAGS  :=   -G
  CLIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib -lM77 -lF77 -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lsunmath
  FLIBS    := -lM77 -lF77 
endif


ifneq (,$(findstring $(STAF_ARCH),sunx86_55))
  OSFID := sun
  CC       :=  /opt/SUNWspro/bin/cc
  CXX      := /opt/SUNWspro/bin/CC
  SO       := $(CXX)
  FC       := /opt/SUNWspro/bin/f77
  LD       := $(CXX)
  FFLAGS   := -xl -KPIC  -e
  CFLAGS   := -KPIC +w2
  CXXFLAGS := -KPIC +w2
  LDFLAGS  :=
  SOFLAGS  :=    -G
  CLIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib  -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb
  FLIBS    := -lM77 -lF77 

endif
  FFLAGS   += -g
  CFLAGS   += -g
  CXXFLAGS += -g

LD_LIBS := $(CLIBS) $(FLIBS)




