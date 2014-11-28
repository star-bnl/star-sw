############################################################################
#
# $Id: GNUmakefile,v 1.6 2006/08/15 21:42:18 jeromel Exp $
#
# Author: Brian, October 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.6  2006/08/15 21:42:18  jeromel
# Fix rhic -> rhic.bnl.gov
#
# Revision 1.5  1999/02/28 20:11:23  lasiuk
# add programs
#
# Revision 1.4  1999/01/26 20:41:17  lasiuk
# examples on SUN
#
############################################################################

include ../local/GNUmakefile.architecture
#SCL_HOME from above
#aT_BNL  = 1

TRS_HOME  = ..

############### Default Libraries
CPPFLAGS  +=	-I$(SCL_HOME)/include \
		-I$(TRS_HOME)/include

LIB_PATH  :=	-L$(SCL_HOME)/lib/$(STAR_SYS) \
			-L$(shell pwd)/$(TRS_HOME)/$(STAR_SYS)

LIBS      :=	-lscl -lpacklib -lStTrs 

# --------               HP-aCC               ---------

ifeq ($(STAR_SYS),hp_ux102)
	CPPFLAGS      := -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS \
			 -I/opt/aCC/include/SC \
			-I$(SCL_HOME)/include \
			-I$(TRS_HOME)/include
	LIB_PATH      += -L/cern/pro/lib
	LDFLAGS       := -Wl,+vnocompatwarnings +p
endif

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

# --------            SUN/Solaris              ---------
ifeq ($(findstring sun4x,$(STAR_SYS)),sun4x)
	LD_FLAGS  := -PPPP
	LIB_PATH  += -L/cern/pro/lib -L$(STDHOME)/lib -L/afs/rhic.bnl.gov/star/packages/ObjectSpace/2.0m/lib
	LIBS      += -lospace -lF77 -lM77 -lsocket -lnsl -lsunmath
endif

# --------             SGI/IRIX                ---------
ifeq ($(findstring sgi,$(STAR_SYS)),sgi)
	LIB_PATH  +=    -L$(STDHOME)/lib
	LIBS      +=    -lospace -lmx
endif


#all:	trsSector
#all:	trsExample
#all:	trsDataBase
all:	maker
#all:	trsCoordinate

%:	%.cc GNUmakefile
	@echo Compliling...
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LIB_PATH) $(ROOTLIBS) $(LIBS)
#	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LDFLAGS) $(LIB_PATH) $(LIBS)

clean:
	@rm -rf trsSector trsExample trsDataBase maker trsCoordinate core *.o
