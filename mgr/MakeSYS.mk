#  $Id: MakeSYS.mk,v 1.2 1998/03/09 14:36:31 fisyak Exp $
#  $Log: MakeSYS.mk,v $
#  Revision 1.2  1998/03/09 14:36:31  fisyak
#  Switch varibales
#
#  Revision 1.1  1998/03/09 13:31:50  fisyak
#  Remove environment Variables
#
#  Revision 1.6  1998/02/22 02:07:10  fisyak
#  Add DATA
#
#  Revision 1.5  1998/02/17 18:06:48  fisyak
#  Add dropit for PATH
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
#
#             Last modification $Date: 1998/03/09 14:36:31 $ 
ALL_DEPS  = $^
FIRST_DEP = $<
FIRSTF    = $(<D)/$(<F)
ALL_TAGS  = $@
STEM      = $*
STEMF     = $(*D)/$(*F)
STIC      = stic
KUIPC           := kuipc
KUIPC_FLAGS     :=
EMPTY    :=
FOUR     :=4
TWO      :=2
#
CC         := gcc
CFLAGS     := -fpic -w
CXX        := gcc
CXXFLAGS   :=-fpic -w
ARFLAGS    := rvu
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),rs_aix31 rs_aix32 rs_aix41))
CXX        := xlC
FFLAGS     :=-O -qextname -qcharlen=6000 -WF,-D
F_EXTENDED :=-e
LDFLAGS    :=-bnoentry -bE:$*.exp import.map -bh:8 -T512 -H512
LD_LIBS    :=-lld -lxlf90 -lxlf -lm -lc
OPSYS      := AIX
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),alpha_osf32c))
CXX        := cxx
CFLAGS     := -w
FFLAGS     := -pic  -static -fpe2
F_EXTENDED := -extend_source
LDFLAGS    := -shared -expect_unresolved \*\
LD_LIBS    := lUfor -lfor -lFutil -lm -lm_4sqrt -lots -lc
OPSYS      := OSF1V32
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),hp700_ux90))
CXX        :=      CC
CXXFLAGS   := -w -O +a1
CFLAGS     :=  +z  -Aa -D_HPUX_SOURCE
FFLAGS     := +ppu +z +O2
LDFLAGS    := -b +a1 -z
LD_LIBS    := /opt/fortran/lib/libU77.a
CC_LIBS    := -L/opt/CC/lib -lC.ansi -lcxx -lcl -lc
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),hp_ux102))
CXX        :=      CC
CXXFLAGS   := -w +a1 -z +Z
#       CC         :=       cc
#CFLAGS     :=  -w +a1 -z +z
FFLAGS     := +ppu -K +z -w
F_EXTENDED := +es
CPPFLAGS   := -Dextname
ARFLAGS    :=     slrvu
RANLIB     := /bin/true
LD         := CC
LDFLAGS    := -b -z
LD_LIBS    := /opt/fortran/lib/libU77.a  -lf
CC_LIBS    := -L/usr/lib -L/lib -L/opt/CC/lib -lC.ansi -lcxx -lcl -lisamstub -lc
CERN_LIBS  :=
#LIBRARIES  :=
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),hp_ux102_aCC))
CXX        := aCC
#ifndef NEW
#CXXFLAGS   := -w -z +Z
#else /* NEW */
#CXXFLAGS   := -w -z +Z
#CXXFLAGS   := -w -z +Z +DAportable -Wl,-E            # from P.Nevski
CXXFLAGS   := +Z                                     # from ROOT
#endif /* NEW */
CC         := cc
#ifndef NEW
#CFLAGS     :=  -Ae -w -z +z
#FFLAGS     := +ppu +z +O2
#LDFLAGS    := -b -z
#LD         := aCC
#LD_LIBS    := /opt/fortran/lib/libU77.a
#CC_LIBS    := -lm
#else /* NEW */
CFLAGS     := +DA1.0 -g +Z  -Wl,+s,+b${STAR_LIB} -Ae # from DSPACK
FC         := fort77
FFLAGS     := +DA1.0 +U77 +ppu +Z                    # from DSPACK
F_EXTENDED := +es
LDFLAGS    := -b                                     # from DSPACK 
#LDFLAGS    := -b -g -z                               # from ROOT
        LD         := ld                                     # from DSPACK
#LD         := aCC                                    # from ROOT
LD_LIBS    := -L/opt/fortran/lib -lU77 -lV3 -lm -lf -lC
#LD_LIBS    := -L/opt/fortran/lib -lU77 -lV3 -lm -lf -lC -lXm -lXt -lX11 -lm -lPW -ldld # from ROOT
#endif /* NEW */
CERN_LIBS  :=
#LIBRARIES  :=
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),hp_ux102_gcc))
FFLAGS     := +ppu +z +O2
LDFLAGS    := -b
LD_LIBS    := /opt/fortran/lib/libU77.a
CC_LIBS    := -L/opt/CC/lib -lC.ansi -lcxx -lcl -lc
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sgi_52 sgi_53))
FFLAGS     :=-Nn20000 -static -trapuv -KPIC
#                                                  -u
F_EXTENDED :=-extend_source
CC         := cc
CFLAGS     :=   -KPIC -kpicopt -w
#CFLAGS     :=   -fpic -fPIC -w
CXX        := CC
CXXFLAGS   :=  -32 ${CFLAGS} -xansi -w
#                                               -use_cfront 
ARFLAGS    :=     slrvu
#LD         := ld
LD         := CC
LDFLAGS    := -32 -shared
        LD_LIBS    := -lsun -lftn -lm -lc -lPW -lmalloc
OPSYS      := IRIX53
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sgi_63 sgi_64))
FFLAGS     :=-32 -Nn20000 -static -trapuv -KPIC
#                                                       -u
F_EXTENDED :=-extend_source
CC         := cc
CFLAGS     :=   -32 -ansi -KPIC -kpicopt -w
CXX        :=  CC
#CXXFLAGS   :=  ${CXXFLAGS} -32
CXXFLAGS   :=  -32 ${CFLAGS} -xansi -use_cfront -w
#CFLAGS     :=   ${CFLAGS} -32
LD         := CC
LDFLAGS    := -32 -shared
LD_LIBS    := -lXext -lm
#LIBRARIES  := 
OPSYS      := IRIX64_32
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sgi_64_n32))
CXX        :=  CC
CXXFLAGS   := -n32
CFLAGS     := -n32
FFLAGS     := -n32 -Nn20000 -O2
F_EXTENDED := -extend_source
LDFLAGS    := -n32 -shared
LD_LIBS    := -lXext -lm
OPSYS      := IRIX64_n32
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sgi_64_64))
CXX        :=  CC
CXXFLAGS   := -64
CFLAGS     := -64
FFLAGS     := -64 -Nn20000 -O2
F_EXTENDED := -extend_source
LDFLAGS    := -64 -shared
LD_LIBS    := -lXext -lm
OPSYS      := IRIX64_64
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),i386_linux2))
#  ====================
FFLAGS     :=-w -O2 -export-dynamic -fno-second-underscore
F_EXTENDED :=-e 
LDFLAGS    := -shared
LD_LIBS    := -ldl -L/usr/X11R6/lib/ -lX11 -lXm -lXt
OPSYS      := Linux
endif
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sun4m_54 sun4m_55 sun4x_55 sun4x_56))
CPPFLAGS   -DSUN -DSOLARIS -Dsun
#ifndef NEW
FFLAGS     :=-xl -PIC -w
#else /* NEW */
#FFLAGS     :=-xl -PIC -w -DOSV=5  #DSPACK
#endif /* NEW */
F_EXTENDED :=-e
#ifndef NEW
CC         := gcc
CFLAGS     := -ansi -fpic -fPIC -w
CXX        := g++
CXXFLAGS   := -w
LD         := $CXX
#else /* NEW */
#CC         := cc                   # DSPACK
#CFLAGS     :=   -Xc -KPIC            # DSPACK
#CXX        := CC
#CXXFLAGS   :=  -w -KPIC             # ROOT
#endif /* NEW */
LDFLAGS    := -G
#ifndef NEW
LD_LIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib -lM77 -lF77 -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb
#else /* NEW */
#LD_LIBS    := -L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib -lM77 -lF77 -lm -lC -lc -L/usr/ucblib -R/usr/ucblib -lucb
#LD_LIBS    := -lm -lsocket -lgen -lsunmath # ROOT
#endif /* NEW */
#                                                                                    -lsunmath 
CC_LIBS    := -L/usr/ucblib -R/usr/ucblib -lm -ldl -lform -lmalloc
endif 
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sunx86_55))
CXX        :=  CC
#CXXFLAGS   :=  -w -pic -D_cplusplus
CXXFLAGS   :=  -KPIC                       # V.P.
CC         :=  cc                          # V.P.
CFLAGS     :=  -KPIC                       # V.P.
#FFLAGS     :=-w -pic -Nq1500 -Nl100
#FFLAGS     :=-xl -KPIC                     # V.P.
FFLAGS     :=-KPIC                         # V.P. -xl
F_EXTENDED :=-e
#LD         := $CXX                         # V.P.
LDFLAGS    := -G -t -z muldefs
#LD_LIBS    := -L/opt/SUNWspro/SC4.2/lib/libp -lM77 -lF77 -lsunmath
#LDFLAGS    := -G                           # V.P.
LD_LIBS    :=-L/opt/SUNWspro/lib -L/opt/SUNWspro/SC4.2/lib -lM77 -lF77 -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb
OPSYS      := sun4os5pc
endif





