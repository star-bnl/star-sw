############################################################################
#
# $Id: GNUmakefile,v 1.2 1998/11/16 14:46:26 lasiuk Exp $
#
# Author: Brian, October 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: GNUmakefile,v $
# Revision 1.2  1998/11/16 14:46:26  lasiuk
# do not use architecture for exectuable
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

aT_BNL  = 1
include ../local/GNUmakefile.architecture
#SCL_HOME from above
#aT_BNL  = 1

LIBS      =     -L$(SCL_HOME)/$(STAR_SYS)/lib \
		-L$(shell pwd)/$(TRS_HOME)/$(STAR_SYS) \
		-L/cern/pro/lib \
		-lscl -lStTrs -lpacklib
			-L$(shell pwd)/$(TRS_HOME)/$(STAR_SYS)

LIBS      :=	-lscl -lpacklib -lStTrs 

	CXX           :=  aCC
	CXXFLAGS      := -g
# --------               HP-aCC               ---------

			 -I$(SCL_HOME)/include \
			 -I$(TRS_HOME)/include
	LD	      := $(CXX)
	CPPFLAGS      := -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS \
			 -I/opt/aCC/include/SC \
	LIB_PATH      += -L/cern/pro/lib
	LDFLAGS       := -Wl,+vnocompatwarnings +p
endif

	CXX          := g++ 
	CPPFLAGS     := -DGNU_GCC -DST_NO_MEMBER_TEMPLATES
        CPPFLAGS     += -DST_NO_NUMERIC_LIMITS -DST_NO_EXCEPTIONS
	CPPFLAGS     += -DST_NO_NAMESPACES
	CXXFLAGS     += -pipe -fno-for-scope
	LD           :=  $(CXX)
# ----- i386_linux2, i386_redhat50, i386_redhat51 ------

ifeq ($(findstring i386, $(STAR_SYS)),i386)
	ifdef TRS_OPTIMISE
		CXXFLAGS += -O 
		CXXFLAGS += -g 
		LDFLAGS =  -Wl,-Bdynamic -Wl,-noinhibit-exec 
		LIBS 	= 	-L$(SCL_HOME)/$(STAR_SYS)/lib \
				-L$(TRS_HOME)/$(STAR_SYS)/ \
				-lStTrs -lscl `cernlib ` \
				-L/usr/pgi/linux86/lib -lpgftnrtl \
				-L/usr/lib -lm -lpgc
		LIBS        += -lpgftnrtl -lcrypt -lpgc -ldl
	else
		LIB_PATH    += -L/cern/pro/lib
	endif

endif
	CXX       =      CC
	LIBS      +=    -L$(STDHOME)/lib -lstd15d
	LD_FLAGS  := -PPPP
	LIB_PATH  += -L/cern/pro/lib -L$(STDHOME)/lib -L/afs/rhic/star/packages/ObjectSpace/2.0m/lib
	LIBS      += -lospace -lF77 -lM77 -lsocket -lnsl -lsunmath

endif
	LIBS       +=    -L$(STDHOME)/lib -lospace -lmx
ifeq ($(findstring sgi,$(STAR_SYS)),sgi)
	LIB_PATH  +=    -L$(STDHOME)/lib
	LIBS      +=    -lospace -lmx
all:	trsSector
#all:	trsSector
#all:	trsExample
all:	trsDataBase
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LDFLAGS) $(LIBS)
	@echo Compliling...
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $< -o $@ $(LIB_PATH) $(ROOTLIBS) $(LIBS)
	@rm -f trsSector core *.o $(SCL_TEMPLATE_REPOSITORY)

clean:
	@rm -rf trsSector trsExample trsDataBase core *.o
