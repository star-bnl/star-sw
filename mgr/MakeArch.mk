#  $Log: MakeArch.mk,v $
#  Revision 1.27  1998/09/16 21:52:13  fisyak
#  Add dependencies for StRoot
#
#  Revision 1.26  1998/09/10 23:32:47  perev
#  add ASU_MALLOC_ON
#
#  Revision 1.25  1998/09/09 07:42:47  fisyak
#  For HP optimization is -O
#
#  Revision 1.24  1998/09/08 06:46:07  fisyak
#  Standard extension for HP StAF libraries
#
#  Revision 1.23  1998/08/28 22:46:48  perev
#  MakeArch introduce flag NEW_ARRAY_ON for SGI,GCC and aCC
#
#  Revision 1.22  1998/08/28 14:56:57  fisyak
#  Fix for hp
#
#  Revision 1.21  1998/08/22 01:26:00  perev
#  QUIET_ASP to MakeArch
#
#  Revision 1.20  1998/08/21 15:44:47  fisyak
#  Add reco_ds
#
#  Revision 1.19  1998/08/13 02:48:46  perev
#  cleanup
#
#  Revision 1.18  1998/08/10 23:23:03  fisyak
#  add CERNLIB_SOLARISPC
#
#  Revision 1.17  1998/08/02 19:07:16  perev
#  New Pavel makefile and bug in MakeArch
#
#  Revision 1.16  1998/07/20 20:15:24  perev
#  *.mk small impovement
#
#  Revision 1.15  1998/07/14 01:22:17  perev
#  static EXE for sgi_64
#
#  Revision 1.14  1998/07/14 00:23:21  perev
#  things
#
#  Revision 1.10  1998/05/19 16:36:38  perev
#  Makefiles
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
#             Last modification $Date: 1998/09/16 21:52:13 $ 
#. default setings

RM := rm -f
MV := mv -f
RMDIR := rm -rf
CP := cp
CD := cd
LN := ln -sf
SLASH :=/
MKDIR := mkdir -p

COUT := -o 
LOUT := -o 

  O     :=o
  A     :=a
  Cxx   :=cxx
  So    :=so

CERN_LEVEL =pro
CERN_STAF = $(CERN)/$(CERN_LEVEL)
CERN_ROOT_INCS = $(CERN_ROOT)/include/cfortran 
CERN_ROOT_LIBS = $(shell cernlib geant321 pawlib graflib mathlib)

MOTIF :=YES
GCC      :=  gcc
CC       :=  $(GCC)
CFLAGS   := $(DEBUG) -fpic -w
CXX      :=  g++
CXXFLAGS := $(DEBUG) -fpic -w
FC 	 := f77
AR       := ar
ARFLAGS  := rvu
LD 	 := $(CXX)
SO	 := $(CC)
SOFLAGS	 := 
LDFLAGS	 := 
EXEFLAGS := NONE
So       :=so
O        :=o
A        :=a
Cxx   :=cc
CLIBS    :=
FLIBS    :=

CPPFLAGS := $(UNAMES) $(STAF_ARCH) $(TULL_ARCH) QUIET_ASP 
ifndef ASU_MALLOC_OFF
  CPPFLAGS += ASU_MALLOC_ON
endif
MKDEPFLAGS:= -MG -MM -w -nostdinc

OSFID    :=
STRID    :=
YACC     := yacc
YACCLIB  := -ly
LEX      := lex
LEXLIB   := -ll

DEBUG := -g 
ifdef NODEBUG
  DEBUG := -O2
endif
ifdef nodebug
  DEBUG := -O2
endif

ifneq (,$(findstring $(STAF_ARCH),intel_wnt))
#  case WIN32
#  ====================


  COUT := -Fo 
  LOUT := -out: 

  O     :=obj
  A     :=lib
  Cxx   :=cxx
  So    :=dll
  
  RM := del /Q
  CP := copy
  LN := xcopy
  SLASH := \\
  MKDIR :=mkdir
  OSFID   := VISUAL_CPLUSPLUS CERNLIB_WINNT CERNLIB_MSSTDCALL
  STRID   := wnt
  CXX     := cl
  CC      := cl
  LD      := $(CXX)
  SO      := link
  SOFLAGS := /DEBUG /NODEFAULTLIB /INCREMENTAL:NO /NOLOGO /DLL /PDB:$(PDB)_all
  CXXFLAGS:= $(cvarsdll) /MD /G5 /Zi /Fd$(PDB) /O2
  CFLAGS  := $(CXXFLAGS)
  LDFLAGS := $(conlflags)
  CLIBS   := $(guilibsdll)

  FC         = fl32
  FLIBS   := dfordll.lib
  FFLAGS  := /MD /G5 /Zi /Fd$(PDB) /fpp /Oxp
  FEXTEND := /extend_source
endif 

ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
#  case IBMRT
#  ====================

  OSFID :=aix AIX CERNLIB_IBMRT CERNLIB_UNIX CERNLIB_QMIBM
  STRID :=aix
  ifdef GCC.
    CXXFLAGS := $(DEBUG)  -fsigned-char -w  
    CFLAGS  := $(DEBUG)  -fsigned-char -w 
    LDFLAGS  := 
    SOFLAGS  :=  -shared 
    CLIBS  :=  -lXm -lXt -lX11 -lg++ -lm -lld
  else
    CXX    := xlC
    CC     := xlC
    LD     := $(CXX)
    SO     := ???
    SOFLAGS := ???
    CXXFLAGS := $(DEBUG) -w -qchars=signed -qnoro -qnoroconst 
    CFLAGS  := $(DEBUG) -w -qchars=signed -qnoro -qnoroconst 
    LDFLAGS  := 
    CLIBS  :=  -lXm -lXt -lX11  -lld  -lm -lc -lcurses
  endif

  FC         = xlf
  FLIBS   := -lxlf90 -lxlf
  FFLAGS  := $(DEBUG) -qextname  -qrndsngl -qcharlen=6000 
  FEXTEND := -e
endif 

ifneq (,$(findstring $(STAF_ARCH),i386_linux2 i386_redhat50))
#    case linux
#  ====================
  LINUX :=YESS
  MOTIF :=
  CERN_LEVEL :=pgf98
  OSFID    := lnx Linux linux LINUX CERNLIB_LINUX CERNLIB_UNIX CERNLIB_LNX CERNLIB_QMLNX NEW_ARRAY_ON
  STRID    := lnx
  FC       := /usr/pgi/linux86/bin/pgf77
  LD       := $(CXX)
  SO	   := $(CXX)
  CXXFLAGS := $(DEBUG) -fPIC
  CFLAGS   := $(DEBUG) -fPIC
  CPPFLAGS += f2cFortran
  LDFLAGS  := $(DEBUG) -Wl,-Bstatic
  EXEFLAGS := $(DEBUG) -Wl,-Bdynamic  
  SOFLAGS  := $(DEBUG) -shared  
##CLIBS    := -L/usr/X11R6/lib -Wl,-Bdynamic -lXpm -lXt -lXext -lX11 -lg++ -lpgc -lm -ldl -rdynamic
  CLIBS    := -L/usr/pgi/linux86/lib -L/usr/X11R6/lib -L/usr/lib -lXt -lXpm -lX11 -lcrypt -lg++ -lpgc -lm -ldl  -rdynamic
##FLIBS    := -L/usr/pgi/linux86/lib -lpgftnrtl 
  FLIBS    := -L/opt/star/lib -lpgf77S -lpgf77A 
  FFLAGS   := -DPGI  $(DEBUG)
  FEXTEND  := -Mextend
  YACC     := bison -y
  YACCLIB  := 
  LEX      := flex
  LEXLIB   := -lfl
endif

ifneq (,$(findstring $(STAF_ARCH),alpha_osf1 alpha_osf32c alpha_dux40))
#    case "alpha":
#  ====================
  OSFID := osf ALPHA alpha CERNLIB_QMVAOS CERNLIB_DECS CERNLIB_UNIX
  STRID := osf
  ifdef GCC.
    CXXFLAGS := -w  -D__osf__ -D__alpha 
    CFLAGS  := -w  -D__osf__ -D__alpha 
    LDFLAGS  :=
    SOFLAGS  := -shared 
    CLIBS  := -lXm -lXt -lX11 -lg++ -lm -lPW -lm -lm_4sqrt -lots -lc
 
 else
    CXX    :=cxx
    CC     :=cc
    LD     :=$(CXX)
    SO     :=$(CXX)
    CXXFLAGS := $(DEBUG) -w -D__osf__ -D__alpha -Dcxxbug -DALPHACXX 
    CFLAGS  := $(DEBUG) -w  
    LDFLAGS  := $(DEBUG) 
    SOFLAGS  := $(DEBUG)  -call_shared -expect_unresolved '*'
    CLIBS  :=  -lXm -lXt -lX11 -lm -lPW -lm -lm_4sqrt -lots -lc
 
  endif

  FLIBS    :=  -lUfor -lfor -lFutil 
  FFLAGS   := $(DEBUG)  -pic  -static -fpe2 
  FEXTEND  :=  -extend_source 
endif

ifneq (,$(findstring $(STAF_ARCH),hp_ux102 hp700_ux90))

#    case "hp":
#  ====================
  HPUX := Yess
  OSFID := HPUX CERNLIB_HPUX CERNLIB_UNIX 
  STRID := hpu
  ifdef GCC
    CXXFLAGS  := $(DEBUG) -fPIC  -I/usr/include/X11R5 -Dextname -D_HPUX_SOURCE 
    CFLAGS    := $(DEBUG) -fPIC  -I/usr/include/X11R5 -Dextname -D_HPUX_SOURCE
    LDFLAGS   := -b $(DEBUG) 
    SOFLAGS   := -shared $(DEBUG) 
    CLIBS   := -L/usr/lib/Motif1.2 -L/usr/lib/X11R5 -L/usr/lib -lXm -lXt -lX11 -lg++ -lm -lPW -ldld -L/opt/CC/lib -lC.ansi -lcxx -lcl -lc
    FLIBS   :=  /opt/fortran/lib/libU77.a 

  endif

  ifndef noACC.
ifdef NODEBUG
  DEBUG := -O
endif
ifdef nodebug
  DEBUG := -O
endif

  OSFID += NEW_ARRAY_ON
    CXX     := aCC
    CC      := cc
    LD      := $(CXX)
    SO      := $(CXX)
    CXXFLAGS  := $(DEBUG) -z +Z  -Dextname  
    CFLAGS   := $(DEBUG) -Ae -z +Z -Dextname  
    LDFLAGS   := $(DEBUG)  -z -Wl,+s -Wl,-E 
    EXEFLAGS  := $(LDFLAGS) -Wl,-N
    SOFLAGS   := $(DEBUG)  -b -z  
    CLIBS   :=   -lXm -lXt -lX11 -lm -lPW -ldld

  else
    CXX     :=  CC
    CC      := cc
    LD      := $(CXX)
    SO      := $(CXX)
    CXXFLAGS  := $(DEBUG) +a1 -z +Z -w -Dextname  -D_HPUX_SOURCE +DAportable
    CFLAGS   :=  $(DEBUG) -Ae -z +Z -Dextname   -D_HPUX_SOURCE +DAportable
    LDFLAGS   := $(DEBUG) +a1 -z -Wl,+s -Wl,-E 
    SOFLAGS   := $(DEBUG) -b +a1 -z 
    CLIBS   :=   -L/usr/lib/X11R5 -lXm -lXt -lX11 -lm -lPW -ldld
  endif
#  So := sl
  FC        :=fort77
##FLIBS     := /opt/fortran/lib/libU77.a /opt/langtools/lib/end.o
  FLIBS     := /opt/fortran/lib/libU77.a 
  FFLAGS    := $(DEBUG) +DA1.0 +ppu +Z  +U77 -K
  FEXTEND   := +es
endif



ifneq (,$(findstring $(STAF_ARCH),sgi_52 sgi_53))
#  ====================
  OSFID := SGI IRIX CERNLIB_SGI CERNLIB_UNIX NEW_ARRAY_ON
  STRID := sgi
  FFLAGS    :=  -Nn20000 -static -trapuv  
  FEXTEND   :=  -extend_source
  CC        :=    cc
  CFLAGS    :=   $(DEBUG) -signed -fullwarn
  CXX       :=   CC
  CXXFLAGS  :=   $(DEBUG) -signed  -fullwarn
  ARFLAGS   :=   slrvu
  LD        :=   $(CXX)
  LDFLAGS   :=$(DEBUG) 
  SO        :=   $(CXX)
  SOFLAGS   :=  $(DEBUG) -shared
  CLIBS     := -lsun -lmalloc  -lm -lc -lPW -lXext
  FLIBS     :=   -lftn 

endif

ifneq (,$(findstring $(STAF_ARCH),sgi_64 ))

  SGI64 := Yess
  OSFID :=  irix64 sgi64 SGI64 IRIX64 CERNLIB_QMIRIX64 CERNLIB_SGI CERNLIB_UNIX NEW_ARRAY_ON
  STRID := sgi
  FFLAGS    :=  $(DEBUG) -n32  -static -trapuv 
  FEXTEND   :=  -extend_source
  CC        :=    cc
  CFLAGS    :=  $(DEBUG) -n32  -fullwarn	
  CXX       :=    CC
  CXXFLAGS  := $(DEBUG)  -n32 -fullwarn
  LD        :=   $(CXX)
  LDFLAGS   := $(DEBUG)  -n32 -multigot
  SO        :=   $(CXX)
  SOFLAGS   := $(DEBUG)  -n32 -shared -multigot
  CLIBS     :=  -lsun  -lm -lc -lPW -lXext -lmalloc
  FLIBS     :=  -lftn 

endif


ifneq (,$(findstring $(STAF_ARCH),sun4x_55 sun4x_56))
  CPPFLAGS := $(filter-out SunOS,$(CPPFLAGS))
  OSFID :=  sun SUN SOLARIS Solaris CERNLIB_UNIX CERNLIB_SUN
  STRID :=  sun
  CC :=  /opt/SUNWspro/bin/cc
  CXX := /opt/SUNWspro/bin/CC
  LD  := $(CXX)
  SO  := $(CXX)
  FC  := /opt/SUNWspro/bin/f77

  FFLAGS   :=  $(DEBUG)  -KPIC -w 
  FEXTEND  :=  -e
  CFLAGS   :=  $(DEBUG)  -KPIC 
  CXXFLAGS :=  $(DEBUG)  -KPIC  
  LDFLAGS  :=  $(DEBUG)  -Bstatic 
  EXEFLAGS :=  $(DEBUG)  -Bdynamic -t 
  SOFLAGS  :=  $(DEBUG) -G
  CLIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib  -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc
  FLIBS    := -lM77 -lF77 -lsunmath
endif


ifneq (,$(findstring $(STAF_ARCH),sunx86_55))
  CPPFLAGS := $(filter-out SunOS,$(CPPFLAGS))
  OSFID :=  sun SUN SOLARIS SOLARISPC CERNLIB_UNIX CERNLIB_SOLARISPC
  STRID :=  sun
  CC       :=  /opt/SUNWspro/bin/cc
  CXX      := /opt/SUNWspro/bin/CC
  SO       := $(CXX)
  FC       := /opt/SUNWspro/bin/f77
  LD       := $(CXX)
  FFLAGS   :=  $(DEBUG) -KPIC  
  FEXTEND  := -e
  CFLAGS   :=  $(DEBUG) -KPIC +w2 -I/usr/dt/share/include -I/usr/openwin/share/include
  CXXFLAGS :=  $(CFLAGS)
  LDFLAGS  :=  $(DEBUG)  -z muldefs -Bstatic
  EXEFLAGS :=  $(DEBUG)  -z muldefs -Bdynamic -t
  SOFLAGS  :=  $(DEBUG)  -G
  CLIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib  -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc
  FLIBS    := -lM77 -lF77 -lsunmath
  NOROOT   := YES
endif

CPPFLAGS := $(filter-out HP-UX,$(CPPFLAGS) $(OSFID))
CPPFLAGS := $(sort $(addprefix -D,$(CPPFLAGS)))

FOR72 := $(FC)
FC  := $(FC) $(FEXTEND)

ifeq ($(EXEFLAGS),NONE)
  EXEFLAGS := $(LDFLAGS)
endif

