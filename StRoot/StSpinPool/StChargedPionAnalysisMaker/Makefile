# $Id: Makefile,v 1.1 2008/12/29 16:01:39 kocolosk Exp $

# @author Adam Kocoloski
# Generates standalone library for reading StChargedPionEvent trees.

INCLUDES := -I../.. -I../../StarClassLibrary -I../../St_base \
	-I$(ROOTSYS)/include
CXXFLAGS := -D__ROOT__ $(INCLUDES)
ROOT_ARCH := $(shell root-config --arch)
ROOT_VERSION := $(shell root-config --version)
LIBS := -lCore -lCint -lPhysics -lMathCore -lTable
LDFLAGS := -dynamiclib -L$(ROOTSYS)/lib

SCL_SRCS = \
	StHelix.cc \
	StParticleDefinition.cc \
	StParticleTable.cc \
	StPhysicalHelix.cc \
	StPionPlus.cc \
	StThreeVector.cc

SCL_OBJS = $(SCL_SRCS:.cc=.o)
SCL_HDRS = $(SCL_SRCS:.cc=.hh) StThreeVectorF.hh

BASE_SRCS = \
	StAutoBrowse.cxx \
	StObject.cxx

BASE_OBJS = $(BASE_SRCS:.cxx=.o)
BASE_HDRS = $(BASE_SRCS:.cxx=.h)

STEVENT_SRCS = \
	StRunInfo.cxx

STEVENT_OBJS = $(STEVENT_SRCS:.cxx=.o)
STEVENT_HDRS = $(STEVENT_SRCS:%.cxx=StEvent/%.h)

MINIMC_SRCS = \
	StTinyMcTrack.cxx \
	StTinyRcTrack.cxx

MINIMC_OBJS = $(MINIMC_SRCS:.cxx=.o)
MINIMC_HDRS = $(MINIMC_SRCS:%.cxx=StMiniMcEvent/%.h) StMiniMcEvent/StMiniMcPair.h

SRCS = \
	StChargedPionBaseEv.cxx \
	StChargedPionEvent.cxx \
	StChargedPionJet.cxx \
	StChargedPionJetParticle.cxx \
	StChargedPionMcEvent.cxx \
	StChargedPionTrack.cxx \
	StChargedPionVertex.cxx 

OBJS = $(SRCS:.cxx=.o) $(SCL_OBJS) $(STEVENT_OBJS) $(MINIMC_OBJS) $(BASE_OBJS) \
	StChargedPionEvent_Cint.o
HDRS = $(SRCS:.cxx=.h) $(SCL_HDRS) $(STEVENT_HDRS) $(MINIMC_HDRS) $(BASE_HDRS)

MAIN = StChargedPionEvent.so

.PHONY: depend clean

all: $(MAIN)
	@echo StChargedPionEvent.so has been compiled for $(ROOT_VERSION)

rootcint: LinkDef.h
	rootcint -f StChargedPionEvent_Cint.cxx -c -DROOT_CINT $(CXXFLAGS) $(HDRS) LinkDef.h

LinkDef.h: 
	cp Link_Def.h LinkDef.h

$(MAIN): rootcint $(OBJS)
ifeq '5.20/00' '$(ROOT_VERSION)'
	$(CXX) $(LDFLAGS) $(OBJS) $(LIBS) -lGenVector -o $(MAIN)
else
ifeq '$(ROOT_ARCH)' 'linux'
	$(CXX) -shared -Wl,-Bdynamic -L$(ROOTSYS)/lib $(OBJS) $(LIBS) -lGpad -lGraf -lGraf3d -lHist -lMatrix -o $(MAIN)
else
	$(CXX) $(LDFLAGS) $(OBJS) $(LIBS) -o $(MAIN)
endif
endif

StChargedPionEvent_Cint.o: rootcint
	$(CXX) $(CXXFLAGS) -c StChargedPionEvent_Cint.cxx -o $@
%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@
%.o: ../../StarClassLibrary/%.cc
	$(CXX) $(CXXFLAGS) -c $< -o $@
%.o: ../../St_base/%.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@
%.o: ../../StEvent/%.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@
%.o: ../../StMiniMcEvent/%.cxx
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	$(RM) *.o $(MAIN) LinkDef.h *_Cint.*

depend: $(SRCS)
	makedepend $(INCLUDES) $^

# $Log: Makefile,v $
# Revision 1.1  2008/12/29 16:01:39  kocolosk
# support for a standalone StChargedPionEvent library. Requires checked-out
# copies of St_base, StarClassLibrary, StEvent, StMiniMcEvent, and of course
# StChargedPionAnalysisMaker.
#

