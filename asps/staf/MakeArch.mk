#  $Log: MakeArch.mk,v $
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
#             Last modification $Date: 1998/03/16 00:47:38 $ 
#. default setings
GREP := grep
#GCC := /afs/rhic/opt/rhic/bin/gcc
GCC      :=  gcc
CC       :=  $(GCC)
CFLAGS   := -g -fpic -w
#CXX      :=  g++
CXX      :=  gcc
CXXFLAGS := -g -fpic -w
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
    CXXFLAGS := -g  -fsigned-char -w  
    CFLAGS  := -g  -fsigned-char -w 
    LDFLAGS  := 
    SOFLAGS  :=  -shared 
    CLIBS  :=  -lXm -lXt -lX11 -lg++ -lm -lld
  else
    CXX    := xlC
    CC     := xlC
    LD     := $(CXX)
    SO     := ???
    SOFLAGS := ???
    CXXFLAGS := -g -w -qchars=signed -qnoro -qnoroconst 
    CFLAGS  := -g -w -qchars=signed -qnoro -qnoroconst 
    LDFLAGS  := 
    CLIBS  :=  -lXm -lXt -lX11  -lld  -lm -lc -lcurses
  endif

  FC         = xlf
  FLIBS  :=  -lxlf90 -lxlf
  FFLAGS  := -g -qextname  -qrndsngl -qcharlen=6000 -e
endif 

ifneq (,$(findstring $(STAF_ARCH),i386_linux2))
#    case linux
#  ====================
  OSFID :=lnx
ifndef PGI
  FC       := g77
  LD       := $(CXX)
  SO	   := $(CXX)
  CXXFLAGS   := -g -fPIC  
  CFLAGS    :=  -g -fPIC 
  LDFLAGS    := 
  SOFLAGS    :=  -shared 
  CLIBS    :=  -L/usr/X11R6/lib -lXm -lXpm _-lXt -lXext -lX11 -lg++ -lm -ldl -rdynamic
  FLIBS    := -lU77
  FFLAGS   := -g -Nx800 -NC200 -w -export -dynamic -fno-second-underscore -e
else
  FC       := /usr/pgi/linux86/bin/pgf77
# CC       := /usr/pgi/linux86/bin/pgcc
# CXX      := /usr/pgi/linux86/bin/pgCC
  GCC      := $(GCC) -Dlynx
  CXXFLAGS   := -g -Dlynx  
  CFLAGS    :=  -g -Dlynx
  LD       := $(CXX)
  LDFLAGS    := 
  SOFLAGS    :=  -shared 
  CLIBS    :=  -L/usr/X11R6/lib -lXm -lXpm _-lXt -lXext -lX11 -lg++ -lm -ldl -rdynamic
  FLIBS    := -lU77
  FFLAGS   := -DPGI -w -g 

endif

endif

ifneq (,$(findstring $(STAF_ARCH),alpha_osf1 alpha_osf32c alpha_dux40))
#    case "alpha":
#  ====================
  OSFID := osf
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
    CXXFLAGS := -w -D__osf__ -D__alpha -Dcxxbug -DALPHACXX 
    CFLAGS  := -g -w  
    LDFLAGS  := 
    SOFLAGS  :=  -call_shared -expect_unresolved '*'
    CLIBS  :=  -lXm -lXt -lX11 -lm -lPW -lm -lm_4sqrt -lots -lc
 
  endif

  FLIBS    :=  -lUfor -lfor -lFutil 
  FFLAGS   :=   -pic  -static -fpe2 -extend_source

endif

ifneq (,$(findstring $(STAF_ARCH),hp_ux102 hp700_ux90))

#    case "hp":
#  ====================
  OSFID :=hpu
  ifdef GCC
    CXXFLAGS  := -fPIC   -I/usr/include/X11R5 
    CFLAGS   := -fPIC   -I/usr/include/X11R5 
    LDFLAGS   := -b 
    SOFLAGS   := -shared 
    CLIBS   := -L/usr/lib/Motif1.2 -L/usr/lib/X11R5 -L/usr/lib -lXm -lXt -lX11 -lg++ -lm -lPW -ldld -L/opt/CC/lib -lC.ansi -lcxx -lcl -lc
    FLIBS   :=  /opt/fortran/lib/libU77.a 

  endif

  ifdef ACC.
    CXX     := aCC
    CC      := cc
    LD      := $(CXX)
    SO      := $(CXX)
    CXXFLAGS  := -z +Z -w 
    CFLAGS   := -Ae -z +Z 
    LDFLAGS   :=  -z -Wl,+s -Wl,-E 
    SOFLAGS   :=  -b -z -Wl,+vnocompatwarnings 
    CLIBS   :=   -lXm -lXt -lX11 -lm -lPW -ldld

  else
    CXX     :=  CC
    CC      := cc
    LD      := $(CXX)
    SO      := $(CXX)
    CXXFLAGS  := +a1 -z +Z -w 
    CFLAGS   :=  -Ae -z +Z 
    LDFLAGS   := +a1 -z -Wl,+s -Wl,-E 
    SOFLAGS   := -b +a1 -z 
    CLIBS   :=   -L/usr/lib/X11R5 -lXm -lXt -lX11 -lm -lPW -ldld
  endif
  SOEXT := sl
  FC        :=fort77
  FLIBS     := /opt/fortran/lib/libU77.a 
  FFLAGS    := +DA1.0 +ppu +Z +es +U77
endif



ifneq (,$(findstring $(STAF_ARCH),sgi_52 sgi_53))
#  ====================
  OSFID := sgi
  GREP  := /afs/rhic/asis/sgi_52/usr.local/bin/ggrep
  FFLAGS    :=  -Nn20000 -static -trapuv  -extend_source -D
  CC        :=    cc
  CFLAGS    :=   -signed -fullwarn
  CXX       :=   CC
  CXXFLAGS  :=   -signed  -fullwarn #-D_STYPES #. add _STYPES due to ulong in stream.h ?
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
  CC :=  /opt/SUNWspro/bin/cc
  CXX := /opt/SUNWspro/bin/CC
  LD  := $(CXX)
  SO  := $(CXX)
  FC  := /opt/SUNWspro/bin/f77

  FFLAGS   :=    -xl -KPIC -w -e
  CFLAGS   :=    -KPIC 
  CXXFLAGS :=   -KPIC 
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
  FFLAGS   :=   -xl -KPIC  -e
  CFLAGS   :=   -KPIC +w2
  CXXFLAGS :=   -KPIC +w2
  LDFLAGS  :=
  SOFLAGS  :=    -G
  CLIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib  -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb
  FLIBS    := -lM77 -lF77 

endif

LD_LIBS := $(CLIBS) $(FLIBS)




