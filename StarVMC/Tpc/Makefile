# $Id: Makefile,v 1.1 2004/07/12 20:37:15 potekhin Exp $

############################### vmc Makefile ###########################

PACKAGE   = tpc

ifndef PLATFORM
  PLATFORM = $(shell uname)
endif

TOPDIR     = $(shell pwd)/..
BINDIR     = $(TOPDIR)/tmp/.$(STAR_HOST_SYS)/$(PACKAGE)
LIBDIR     = $(TOPDIR)/lib/.$(STAR_HOST_SYS)

include $(TOPDIR)/config/Makefile.$(PLATFORM)

############################### Sources #######################################
include $(TOPDIR)/config/MakeRules

# Dictionary
GDICT    := $(BINDIR)/$(PACKAGE)Cint.cxx
GDICTH   := $(GDICT:.cxx=.h)
GDICTO   := $(patsubst %.cxx,$(BINDIR)/%.o,$(GDICT))

CXXSRC	:= $(wildcard src/*.cxx)
SRCS	:= $(CXXSRC)

# C++ Headers
#
HDRS    := $(wildcard include/*.h)
HDRS    := $(filter-out include/$(PACKAGE)LinkDef.h,$(HDRS))
HDRS    += $(wildcard ../vmcApp/include/*.h)
HDRS    := $(filter-out ../vmcApp/include/vmcAppLinkDef.h,$(HDRS))
HDRS    += include/$(PACKAGE)LinkDef.h

# Objects
#
CXXOBJ	:= $(patsubst %.cxx,$(BINDIR)/%.o,$(CXXSRC))
OBJS	:= $(CXXOBJ) $(GDICTO)

# Make include list
INCDIR  := -Iinclude
INCDIR  += -I../vmcApp/include

# C++ compilation flags
CXXFLAGS := $(CXXOPTS) -I. $(INCDIR) -I$(ROOTSYS)/include 
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
	@echo ' '
	@echo OBJS:    $(OBJS)

TOCLEAN		= $(BINDIR)
TOCLEANALL	= $(BINDIR) $(LIBDIR)

include $(TOPDIR)/config/MakeMacros

############################### Dependencies ##################################

-include $(BINDIR)/Make-depend
