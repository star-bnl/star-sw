############################################################################
#
# $Id: Makefile,v 1.1 1998/11/10 17:11:59 fisyak Exp $
#
# Author: Brian, October 1998
# --------------------------------------------------------------------------
#
# --------------------------------------------------------------------------
#
# $Log: Makefile,v $
# Revision 1.1  1998/11/10 17:11:59  fisyak
# Put Brian trs versin into StRoot
#
# Revision 1.1  1998/11/08 17:44:55  lasiuk
# Initial Revision
#
############################################################################
SCL_SYSTEM    = hp_ux102

## Change this for your local SCL installation
SCL_HOME      = /data/STAR/SCL/StarClassLibrary/scl

TRS_HOME      = ../prototype

## Compiler FLAGS---see SCL manual
CXXFLAGS      = -g -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS\
		-I/opt/aCC/include/SC \
		-I../prototype/include \
		-I$(SCL_HOME)/include 

CXX           = aCC

DEST	      = .

LD	      = aCC -Wl,+vnocompatwarnings +p

LIBS	      = -L$(SCL_HOME)/$(STAR_SYS)/lib \
		-L$(TRS_HOME)/$(STAR_SYS) \
		-L/cern/pro/lib \
		-lscl -lStTrs -lpacklib -l++

MAKEFILE      = Makefile

#PROGRAM       = trsDataBase
#PROGRAM       = trsLandau
#PROGRAM       = trsTransparency
PROGRAM       = trsDiffusion
#PROGRAM       = trsSector

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

