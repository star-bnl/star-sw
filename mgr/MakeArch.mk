#  $Log: MakeArch.mk,v $
#  Revision 1.55  1999/01/20 02:16:47  fisyak
#  Active STAR_HOST_SYS for egcs
#
#  Revision 1.54  1999/01/14 13:56:39  fisyak
#  Add Victors MakeFun.mk, Add StMagF
#
#  Revision 1.53  1998/12/24 16:39:40  fine
#  Some extra checl of env variable INCLUDE under Windows
#
#  Revision 1.52  1998/12/17 17:21:00  fisyak
#  Add Akio's insure++
#
#  Revision 1.51  1998/12/12 00:58:32  fisyak
#  remove STAF
#
#  Revision 1.50  1998/12/11 22:08:11  perev
#  redhat51
#
#  Revision 1.48  1998/12/04 01:17:29  fisyak
#  fix for fortran source in StRoot
#
#  Revision 1.47  1998/12/02 20:42:10  perev
#  cleanup
#
#  Revision 1.46  1998/12/01 01:52:47  fisyak
#  Merge with NT
#
#  Revision 1.45  1998/11/29 21:19:16  fisyak
#  new merge with NT
#
#  Revision 1.44  1998/11/16 01:26:44  fisyak
#  New merging with NT
#
#  Revision 1.43  1998/11/14 01:16:58  fisyak
#  Post NT updates
#
#  Revision 1.42  1998/11/13 15:48:43  fisyak
#  Merged version with NT
#
##  Revision 1.41  1998/11/13 00:19:30  fisyak
#  Add flags for SCL St_trs_Maker
#
#  Revision 1.40  1998/11/07 16:52:27  fisyak
#  remove OBJY inlcudes to avoid clash with protection
#
#  Revision 1.39  1998/11/05 23:18:42  perev
#  MOTIF
#
#  Revision 1.38  1998/11/05 21:47:40  fisyak
#  Move OBJY definition to bottom
#
#  Revision 1.37  1998/11/05 20:09:23  fisyak
#  add OBJY CPPFLAGS
#
#  Revision 1.36  1998/10/29 20:53:31  perev
#  ObjectSpace added
#
#  Revision 1.35  1998/10/24 22:32:33  perev
#   Corr for RedHat5.1
#
#  Revision 1.34  1998/10/20 01:41:59  fisyak
#  debug only for db
#
#  Revision 1.33  1998/10/14 21:40:48  fisyak
#  Add versioning of shared libraries for ROOT wrappers
#
#  Revision 1.32  1998/09/24 18:01:42  didenko
#  correcotion
#
#  Revision 1.31  1998/09/24 15:43:51  perev
#   -muldef added for sun
#
#  Revision 1.30  1998/09/22 02:21:31  fisyak
#  Fix NOROOT version
#
#  Revision 1.29  1998/09/21 20:30:55  perev
#  add sgi_62
#
#  Revision 1.28  1998/09/21 14:46:51  perev
#  add CERNLIB_SOLARIS
#
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
#             Last modification $Date: 1999/01/20 02:16:47 $ 
#. default setings

MAKE  := gmake
PWD   := pwd
GETCWD:= getcwd
UNAME := uname
SYS   := sys
SO_SUBDIR := lib

MOTIF := Yess
RM := rm -f
MV := mv -f
RMDIR := rm -rf
CP := cp
CD := cd
LN := ln -sf
SLASH :=/
MKDIR := mkdir -p
CAT    := cat

COUT := -o 
LOUT := -o 
FOUT := -o 
SoOUT := -o 
DEBUG := -g 
ifdef NODEBUG
  DEBUG := -O2
endif
ifdef nodebug
  DEBUG := -O2
endif

  O     :=o
  A     :=a
  Cxx   :=cxx
  So    :=so

CERN_LEVEL =pro
CERN_STAF = $(CERN)/$(CERN_LEVEL)
CERN_ROOT_INCS = $(CERN_ROOT)/include/cfortran 

MAKECERNLIB := cernlib

GCC      :=  gcc
CXX      :=  g++
ifdef INSURE
  GCC      :=  insure -g -Zoi "compiler_c gcc"
  CXX      :=  insure -g -Zoi "compiler_cpp g++"
endif
ifdef CODEWIZ
  GCC      :=  codewizard -g -Zoi "compiler_c gcc"
  CXX      :=  codewizard -g -Zoi "compiler_cpp g++"
endif
CC       :=  $(GCC)
CFLAGS   := $(DEBUG) -fpic -w
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
Cxx      :=cc

CLIBS    :=
FLIBS    :=

CPPFLAGS := $(UNAMES) $(STAF_ARCH) $(TULL_ARCH) QUIET_ASP DS_ADVANCED
STIC       := $(STAR_BIN)/stic
GEANT3     := $(STAR_BIN)/geant3

ifndef ASU_MALLOC_OFF
  CPPFLAGS += ASU_MALLOC_ON
endif
ifneq ($(STAR_SYS),hp_ux102)   
CPPFLAGS += $(UNAMES)
endif


MKDEPFLAGS:= -MG -MM -w -nostdinc

MAKEDEPEND = $(GCC) $(MKDEPFLAGS)
ROOTCINT   =rootcint

OSFID    :=
STRID    :=
YACC     := yacc
YACCLIB  := -ly
LEX      := lex
LEXLIB   := -ll


ifneq (,$(findstring $(STAR_SYS),intel_wnt))
#  case WIN32
#  ====================

  GEANT3:= $(STAR_BIN)/geant3.exe
  SUNRPC:= $(AFS_RHIC)/star/packages/dsl/intel_wnt/inc
  MAKE  := make.exe
  PWD   := pwd.exe
  GETCWD:= getcwd.exe
  UNAME := uname.bat
  SYS   := sys.bat
  SO_SUBDIR := bin
  MOTIF :=
  DEBUG :=  
#  SHELL := echo ==== cmd /C
  NT     := intel_wnt
  CAT    := type
  COUT   := -Fo
  FOUT   := -Fo
  CINP   := -Tc
  CXXINP := -Tp
  FINP   := -Fo
  LOUT   := -out:
  SoOUT  := -out:
  COPT   := -O2
  CXXOPT := -O2
  O     :=obj
  A     :=lib
  Cxx   :=cc
  So    :=dll
ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif
  MAKECERNLIB := $(subst \,\\,$(subst /,\,$(STAR_MAKE_HOME)/cernlib.bat)) 
# MAKECERNLIB = call Y:\wrk\mgr\cernlib.bat

  MAKEDEPEND =echo Please RUN this makefile from UNIX, first
  ROOTCINT   =rootcint

  RM := del /Q
  RMDIR := rmdir /S /Q
  CP := copy
  LN := xcopy
  SLASH := \\
  MKDIR :=mkdir
  OSFID   := VISUAL_CPLUSPLUS CERNLIB_WINNT CERNLIB_MSSTDCALL WIN32
  STRID   := wnt
  AR      := lib
  ARFLAGS := -nologo -MACHINE:IX86 
  CXX     := cl
  CC      := cl
  LD      := $(CXX)
  SO      := link
#  SOFLAGS := -DEBUG -NODEFAULTLIB -INCREMENTAL:NO -NOLOGO -DLL -PDB:$(PDB)_all
  SOFLAGS := -DEBUG  -NODEFAULTLIB -INCREMENTAL:NO -NOLOGO -DLL 
  CXXFLAGS:= $(cvarsdll) -MD -G5 -Zi -nologo -DASU_MALLOC_OFF
#  CXXFLAGS:= $(cvarsdll) -MD -G5 -Zi -Fd$(PDB)
  CFLAGS  := $(CXXFLAGS)
  LDFLAGS := $(conlflags)
  CLIBS   := ws2_32.lib mswsock.lib user32.lib kernel32.lib msvcrt.lib oldnames.lib MSVCIRT.LIB
# CLIBS   := $(guilibsdll) 

  FC         = fl32
  FLIBS   := dfordll.lib
#  FFLAGS  := -MD -G5 -Zi -Fd$(PDB) -fpp -Oxp
  FFLAGS  := -MD -G5 -Zi -fpp -Oxp -nokeep -nologo
  FEXTEND := -extend_source
  KUIPC   :=kuipc.exe
  FLIBS   := DFORDLL.LIB
# check whether the system INCLUDE path was defined 
  ifeq (,$(INCLUDE))
  CXX  := echo ** ERROR ** Please define Visual C++ INCLUDE environment variable first
  CC := $(CXX)
  FC := (CXX)
  endif
endif 

ifneq (,$(findstring $(STAR_SYS),rs_aix31 rs_aix32 rs_aix41))
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

ifneq (,$(findstring $(STAR_SYS),i386_linux2 i386_redhat50))
#    case linux
#  ====================
ifndef EGCS_ON
  MOTIF :=
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

else 
#   	EGCS compiler

#  EGCS := /afs/rhic/asis/i386_linux2/usr.local/egcs
#   CC  := /usr/local/egcs/bin/gcc 
#   CXX := /usr/local/egcs/bin/g++ 
  CC  := /opt/star/egcs/bin/gcc 
  CXX := /opt/star/egcs/bin/g++ 

  MOTIF :=
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
##CLIBS    := -L/usr/X11R6/lib -Wl,-Bdynamic -lXpm -lXt -lXext -lX11 -lpgc -lm -ldl -rdynamic
  CLIBS    := -L/usr/pgi/linux86/lib -L/usr/X11R6/lib -L/usr/local/lib  -lXt -lXpm -lX11  -lpgc -lm -ldl  -rdynamic
##FLIBS    := -L/usr/pgi/linux86/lib -lpgftnrtl 
  FLIBS    := -L/opt/star/lib -lpgf77S -lpgf77A 
  FLIBS    += -L/opt/rhic/lib/gcc-lib/i686-pc-linux-gnu/2.8.1 -lg2c
  FFLAGS   := -DPGI  $(DEBUG)
  FEXTEND  := -Mextend
  YACC     := bison -y
  YACCLIB  := 
  LEX      := flex
  LEXLIB   := -lfl
endif
endif

ifneq (,$(findstring $(STAR_SYS),i386_redhat51 i386_redhat52))
#    case linux but gcc is EGCS
#  ====================
  LINUX :=YESS
  MOTIF :=
  OSFID    := lnx Linux linux LINUX CERNLIB_LINUX CERNLIB_UNIX CERNLIB_LNX CERNLIB_QMLNX NEW_ARRAY_ON GNU_GCC ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES
  STRID    := lnx
  FC       := /usr/pgi/linux86/bin/pgf77
  LD       := $(CXX)
  SO	   := $(CXX)
  CXXFLAGS := $(DEBUG) -fPIC -Wall -fno-rtti -fno-exceptions -fno-for-scope
#                                             -fpipe
  CFLAGS   := $(DEBUG) -fPIC
  CPPFLAGS += f2cFortran
  LDFLAGS  := $(DEBUG) -Wl,-Bstatic
  EXEFLAGS := $(DEBUG) -Wl,-Bdynamic   
  SOFLAGS  := $(DEBUG) -shared  
##CLIBS    := -L/usr/X11R6/lib -Wl,-Bdynamic -lXpm -lXt -lXext -lX11 -lpgc -lm -ldl -rdynamic
  CLIBS    := -L/usr/pgi/linux86/lib -L/usr/X11R6/lib  -lXt -lXpm -lX11  -lpgc -lm -ldl  -rdynamic
##FLIBS    := -L/usr/pgi/linux86/lib -lpgftnrtl 
  FLIBS    := -L/opt/star/lib -lpgf77S -lpgf77A -lg2c
  FFLAGS   := -DPGI  $(DEBUG)
  FEXTEND  := -Mextend
  YACC     := bison -y
  YACCLIB  := 
  LEX      := flex
  LEXLIB   := -lfl
endif

ifneq (,$(findstring $(STAR_SYS),alpha_osf1 alpha_osf32c alpha_dux40))
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

ifneq (,$(findstring $(STAR_SYS),hp_ux102 hp700_ux90))

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
	ifdef SCL_OPTIMISE # from Brian
		CXXFLAGS += +Olibcalls +Onolimit
	else
		CXXFLAGS += +d
	endif              # from Brian
    LDFLAGS   := $(DEBUG)  -z -Wl,+s -Wl,-E,+vnocompatwarnings
    EXEFLAGS  := $(LDFLAGS) -Wl,-N 
    SOFLAGS   := $(DEBUG)  -b -z  
#    CLIBS   :=   -lXm -lXt -lX11 -lm -lPW -ldld /usr/local/lib/libMagick.a
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



ifneq (,$(findstring $(STAR_SYS),sgi_52 sgi_53))
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
ifneq (,$(findstring $(STAR_SYS),sgi_62 ))
#  sgi_62 in sgi_52 compatible mode (VP)
  SGI62 := Yess
  OSFID :=  irix62 sgi62 SGI62 IRIX62 CERNLIB_QMIRIX53 CERNLIB_SGI CERNLIB_UNIX 
  STRID := sgi
  FFLAGS    :=  $(DEBUG) -32  -static -trapuv 
  FEXTEND   :=  -extend_source
  CC        :=    cc
  CFLAGS    :=  $(DEBUG) -32  -fullwarn	
  CXX       :=    CC
  CXXFLAGS  := $(DEBUG)  -32 -fullwarn
  LD        :=   $(CXX)
  LDFLAGS   := $(DEBUG)  -32 -multigot
  SO        :=   $(CXX)
  SOFLAGS   := $(DEBUG)  -32 -shared -multigot
  CLIBS     :=  -lsun  -lm -lc -lPW -lXext -lmalloc
  FLIBS     :=  -lftn 

endif

ifneq (,$(findstring $(STAR_SYS),sgi_64 ))

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


ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56))
  CPPFLAGS := $(filter-out SunOS,$(CPPFLAGS))
  STDHOME := /afs/rhic/star/packages/ObjectSpace/2.0m
  STAF_UTILS_INCS += $(STDHOME) $(STDHOME)/ospace/std  $(STDHOME)/ospace

  OSFID :=  sun SUN SOLARIS Solaris CERNLIB_UNIX CERNLIB_SOLARIS CERNLIB_SUN ST_NO_MEMBER_TEMPLATES ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_TEMPLATE_DEF_ARGS ST_NO_NAMESPACES
  STRID :=  sun
  CC :=  /opt/SUNWspro/bin/cc
  CXX := /opt/SUNWspro/bin/CC
  ifdef INSURE
    CC       :=  insure -g -Zoi "compiler_c cc"
    CXX      :=  insure -g -Zoi "compiler_cpp CC"
  endif
  ifdef CODEWIZ
    CC       :=  codewizard -g -Zoi "compiler_c cc"
    CXX      :=  codewizard -g -Zoi "compiler_cpp CC"
  endif
  LD  := $(CXX)
  SO  := $(CXX)
  FC  := /opt/SUNWspro/bin/f77

  FFLAGS   :=  $(DEBUG)  -KPIC -w 
  FEXTEND  :=  -e
  CFLAGS   :=  $(DEBUG)  -KPIC 
  CXXFLAGS :=  $(DEBUG)  -KPIC -features=castop +w
  LDFLAGS  :=  $(DEBUG)  -Bstatic 
  EXEFLAGS :=  $(DEBUG)  -z muldefs -Bdynamic -t 
  SOFLAGS  :=  $(DEBUG) -G
  CLIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib  -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc
  FLIBS    := -lM77 -lF77 -lsunmath
endif


ifneq (,$(findstring $(STAR_SYS),sunx86_55))
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

# add root working directory to include path
CWD := $(shell $(PWD))
ifndef INPDIR
  override INPDIR := $(CWD)
endif
ifeq (,$(strip $(filter /%,$(INPDIR))))
  override INPDIR := $(CWD)/$(INPDIR)
endif
#	ROOT DIR (Not a Rene ROOT)
PAMS    := $(findstring /pams,$(INPDIR))
ifndef PAMS
  PAMS    := $(findstring /StRoot,$(INPDIR))
endif
ifndef PAMS
  PAMS    := $(findstring /.share,$(INPDIR))
endif
ROOTDIR:= $(word 1,$(subst $(PAMS), ,$(INPDIR)))
CPPFLAGS += -I$(ROOTDIR)

# for Objy
ifdef OBJY_HOME
OBJY_HOME := $(subst /.,,$(wildcard $(OBJY_HOME)/.))
ifdef OBJY_HOME
  CPPFLAGS += -DOBJYBASE -I$(BFWORK)/include -I$(BFWORK)/tmp/$(BFARCH) -I$(BFDIST)/releases/$(BFCURRENT)/include -I$(OBJYBASE)/$(OBJY_ARCH)/include
endif
endif

FOR72 := $(FC)
FC  := $(FC) $(FEXTEND)

ifeq ($(EXEFLAGS),NONE)
  EXEFLAGS := $(LDFLAGS)
endif

ifdef STDHOME
  CLIBS += -L$(STDHOME)/lib -lospace
endif  
  

CERN_ROOT_LIBS := $(shell $(MAKECERNLIB) geant321 pawlib graflib mathlib)

# Current Working Directory
#
CWD := $(shell $(PWD))

