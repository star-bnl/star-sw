# $Id: Makefile,v 1.2 2004/07/16 22:53:20 potekhin Exp $

############################### vmc Makefile ###########################

PACKAGE   = vmcApp

ifndef PLATFORM
  PLATFORM = $(shell uname)
endif

TOPDIR     = $(shell pwd)/..
BINDIR     = $(TOPDIR)/tmp/.$(STAR_HOST_SYS)/$(PACKAGE)
LIBDIR     = $(TOPDIR)/lib/.$(STAR_HOST_SYS)

include $(TOPDIR)/config/Makefile.$(PLATFORM)

############################### Sources #######################################
#
# Rules
#
include $(TOPDIR)/config/MakeRules

# Dictionary
#
GDICT    := $(BINDIR)/$(PACKAGE)Cint.cxx
GDICTH   := $(GDICT:.cxx=.h)
GDICTO   := $(patsubst %.cxx,$(BINDIR)/%.o,$(GDICT))

# Sources
#
# If we wanted a blanket compile, we'd use this: CXXSRC	:= $(wildcard src/*.cxx)
# However, we want to be selective, hence
# CXXSRC	:= \
#src/StarGenerator.cxx \
#src/StarKineGenerator.cxx \
#src/StarParticle.cxx \
#src/StarMCApplication.cxx \
#src/StarRootManager.cxx \
#src/StarStack.cxx \
#src/StarDetectorConstruction.cxx \
#src/StarChamberParametrization.cxx \
#src/StarTrackerHit.cxx \
#src/StarTrackerSD.cxx \
#src/StarRndm.cxx

CXXSRC	:= $(wildcard src/*.cxx)

SRCS	:= $(CXXSRC)

# C++ Headers
#
HDRS    := $(wildcard include/*.h)
HDRS    := $(filter-out include/$(PACKAGE)LinkDef.h,$(HDRS))
HDRS    += include/$(PACKAGE)LinkDef.h

# Objects
#
CXXOBJ	:= $(patsubst %.cxx,$(BINDIR)/%.o,$(CXXSRC))
OBJS	:= $(CXXOBJ) $(GDICTO)

# Make include list
#
INCDIR  := -Iinclude

# C++ compilation flags
# Note I added no deprecated --max--
CXXFLAGS := $(CXXOPTS) -Wno-deprecated -I. $(INCDIR) -I$(ROOTSYS)/include 

DEPINC 		+= -I. -I$(ROOTSYS)/include $(INCDIR)

############################### Targets #######################################


SLIBRARY	= $(LIBDIR)/lib$(PACKAGE).$(SL)
ALIBRARY	= $(LIBDIR)/lib$(PACKAGE).a

default:	depend $(SLIBRARY)


$(LIBDIR)/lib$(PACKAGE).$(SL):  $(OBJS)
$(LIBDIR)/lib$(PACKAGE).a:      $(OBJS)

DICT:=	 	$(GDICT)

$(DICT): 	$(HDRS)

depend:		$(SRCS)

show:

	@echo Platform: $(PLATFORM)
	@echo ' '
	@echo System:   $(STAR_HOST_SYS)
	@echo ' '
	@echo Sources:  $(CXXSRC)
	@echo ' '
	@echo Headers:  $(HDRS)
	@echo ' '
	@echo Options:  $(CXXOPTS)
	@echo ' '
	@echo CXXOBJ:  $(CXXOBJ)
	@echo ' '
	@echo GDICTO:  $(GDICTO)

TOCLEAN		= $(BINDIR)
TOCLEANALL	= $(BINDIR) $(LIBDIR)

include $(TOPDIR)/config/MakeMacros

############################### Dependencies ##################################

-include $(BINDIR)/Make-depend
