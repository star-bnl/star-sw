############################################################################
#
# $Id: Makefile,v 1.4 1998/11/13 21:33:42 lasiuk Exp $
#
# Author: Brian, October 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: Makefile,v $
# Revision 1.4  1998/11/13 21:33:42  lasiuk
# update
#
# Revision 1.3  1998/11/13 00:24:35  lasiuk
# TRUE/FALSE, pntrs in Db
#
# Revision 1.2  1998/11/12 22:39:56  lasiuk
# compatibility at BNL
#
# Revision 1.1  1998/11/10 17:11:59  fisyak
# Put Brian trs versin into StRoot
#
# Revision 1.1  1998/11/08 17:44:55  lasiuk
# Initial Revision
#
############################################################################

AT_BNL        = 1

SCL_SYSTEM    = $(STAR_SYS)

## Change this for your local SCL installation
#SCL_HOME      = /data/STAR/SCL/StarClassLibrary/scl
SCL_HOME      = /star/u2b/lasiuk/SCL/StarClassLibrary/scl

TRS_HOME      = ..

CXXFLAGS      = -I$(TRS_HOME)/include \
		-I$(SCL_HOME)/include

############ default libraries
#LIBS	      = -L$(SCL_HOME)/$(STAR_SYS)/lib \
#		-L$(TRS_HOME)/$(STAR_SYS)/lib \
#                -L/cern/pro/lib \
#                -lStTrs -lscl -lStTrs -lpacklib -l++


## Compiler FLAGS---see SCL/TRS manual
## check compiler type
# --------               HP-aCC               ---------

ifeq ($(STAR_SYS)),hp_ux102)
	CXX           =  aCC
	CXXFLAGS      += -g -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS\
	CXXFLAGS      += -I/opt/aCC/include/SC
	LD	      = aCC -Wl,+vnocompatwarnings +p
endif

# ----- i386_linux2, i386_redhat50, i386_redhat51 ------

ifeq ($(findstring i386, $(STAR_SYS)),i386)
	CXX          := g++ 
	CPPFLAGS     := -DGNU_GCC -DST_NO_MEMBER_TEMPLATES
        CPPFLAGS     += -DST_NO_NUMERIC_LIMITS -DST_NO_EXCEPTIONS
	CPPFLAGS     += -DST_NO_NAMESPACES
	CXXFLAGS     += -pipe -fno-for-scope
	LD           =  $(CXX)
	ifdef TRS_OPTIMISE
		CXXFLAGS += -O 
	else
		CXXFLAGS += -g 
	endif
	ifdef AT_BNL
		LDFLAGS =  -Wl,-Bdynamic -Wl,-noinhibit-exec 
		LIBS 	= 	-L$(SCL_HOME)/$(STAR_SYS)/lib \
				-L$(TRS_HOME)/$(STAR_SYS)/ \
				-lStTrs -lscl `cernlib ` \
				-L/usr/pgi/linux86/lib -lpgftnrtl \
				-L/usr/lib -lm -lpgc
	endif
endif


DEST	      = .


MAKEFILE      = Makefile

#PROGRAM       = trsDataBase
#PROGRAM       = trsLandau
#PROGRAM       = trsTransparency
#PROGRAM       = trsDiffusion
#PROGRAM       = trsIonization
PROGRAM       = trsSector

OBJS	      = $(PROGRAM).o

SRCS          =	$(PROGRAM).cc

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS)
		@echo "Linking $(PROGRAM) ..."
		$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:
		rm -f $(PROGRAM).o
		rm -f $(PROGRAM)
		rm -f last.kumac
		rm -f core

