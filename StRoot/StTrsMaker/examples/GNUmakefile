############################################################################
#
# $Id: GNUmakefile,v 1.3 1999/01/20 15:56:45 lasiuk Exp $
#
# Author: Brian, October 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.3  1999/01/20 15:56:45  lasiuk
# simple mod
#
# Revision 1.2  1998/11/16 14:46:26  lasiuk
# do not use architecture for exectuable
#
# Revision 1.1  1998/11/08 17:44:55  lasiuk
# Initial Revision
#
# Revision 1.4  1999/01/26 20:41:17  lasiuk
# examples on SUN
#
############################################################################

include ../local/GNUmakefile.architecture
#SCL_HOME from above
#aT_BNL  = 1

LIB_PATH	:=	-L$(SCL_HOME)/$(STAR_SYS)/lib \
		-I$(TRS_HOME)/include

LIBS    	:=	-lscl -lpacklib -lStTrs 
			-L$(shell pwd)/$(TRS_HOME)/$(STAR_SYS)

LIBS      :=	-lscl -lpacklib -lStTrs 

	CXX           :=  aCC
	CXXFLAGS      := -g
# --------               HP-aCC               ---------

			 -I$(SCL_HOME)/include \
			 -I$(TRS_HOME)/include
	LD	      := $(CXX)
ifeq ($(STAR_SYS),hp_ux102)
	CPPFLAGS      := -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS \
			 -I/opt/aCC/include/SC \
	LIB_PATH      += -L/cern/pro/lib
	LDFLAGS       := -Wl,+vnocompatwarnings +p
endif

	CXX          := g++
	CXXFLAGS     := -pipe -fno-for-scope
	CPPFLAGS     := -DGNU_GCC -DST_NO_MEMBER_TEMPLATES
        CPPFLAGS     += -DST_NO_NUMERIC_LIMITS -DST_NO_EXCEPTIONS
	CPPFLAGS     += -DST_NO_NAMESPACES
	CPPFLAGS     += -I$(SCL_HOME)/include \
		        -I$(TRS_HOME)/include
	LD           :=  $(CXX)
# ----- i386_linux2, i386_redhat50, i386_redhat51 ------

ifeq ($(findstring i386, $(STAR_SYS)),i386)
	ifdef TRS_OPTIMISE
		CXXFLAGS += -O 
	else
		CXXFLAGS += -g 
	endif
	LDFLAGS      :=
	ifdef AT_BNL
		CERN_LEVEL  := pgf98
		LIB_PATH    += -L/cern/pgf98/lib -L/usr/pgi/linux86/lib
		LIBS        += -lpgftnrtl -lcrypt -lpgc -ldl
	else
		LIB_PATH    += -L/cern/pro/lib
	endif

endif
	CXX       :=      CC
	LIB_PATH  += -L/cern/pro/lib -L$(STDHOME)/lib
	LIBS      += -lstd15d
	LD_FLAGS  := -PPPP
	LIB_PATH  += -L/cern/pro/lib -L$(STDHOME)/lib -L/afs/rhic/star/packages/ObjectSpace/2.0m/lib
	LIBS      += -lospace -lF77 -lM77 -lsocket -lnsl -lsunmath

endif

# --------             SGI/IRIX                ---------
ifeq ($(findstring sgi,$(STAR_SYS)),sgi)
	LIB_PATH  +=    -L$(STDHOME)/lib
	LIBS      +=    -lospace -lmx
endif
all:	trsExample
#all:	trsChargeDistribution
#all:	trsSector
#all:	trsExample
all:	trsDataBase
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LDFLAGS) $(LIB_PATH) $(LIBS)
	@echo Compliling...
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LIB_PATH) $(ROOTLIBS) $(LIBS)
	@rm -f trsSector trsChargeDistribution core *.o $(SCL_TEMPLATE_REPOSITORY)

clean:
	@rm -rf trsSector trsExample trsDataBase core *.o
