#**********************************************************************
#*
#* $Id: Makefile,v 1.1.1.1 2000/08/01 13:57:55 jgreid Exp $
#*
#* Author: Jeff Reid, UW, July 2000
#*
#**********************************************************************
#*
#* Description:  This is the makefile which is necessary for building
#*               libEvent.so, the shared library which contains the
#*               EbyEDST specification.  This library is necessary for
#*               using the EbyE DST in plain ROOT, independent of STAR
#*               infrastructure.
#*
#**********************************************************************
#*
#* $Log: Makefile,v $
#* Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
#* EbyE DST creation and access tools
#*
#*
#**********************************************************************

ObjSuf        = o
SrcSuf        = cxx
ExeSuf        =
DllSuf        = so
EVENTLIB      = $(EVENTO)
OutPutOpt     = -o

ROOTCFLAGS    = $(shell root-config --cflags)
ROOTLIBS      = $(shell root-config --libs)
ROOTGLIBS     = $(shell root-config --glibs)

# Linux with egcs
CXX           = g++
CXXFLAGS      = -O -Wall -fPIC
LD            = g++
LDFLAGS       = -g
SOFLAGS       = -shared


CXXFLAGS     += $(ROOTCFLAGS)
LIBS          = $(ROOTLIBS)
GLIBS         = $(ROOTGLIBS)

#------------------------------------------------------------------------------

EVENTO        = StEbyeTrack.$(ObjSuf) \
		StEbyeEvent.$(ObjSuf) \
		EventDict.$(ObjSuf)

EVENTS        = StEbyeTrack.$(SrcSuf) \
		StEbyeEvent.$(SrcSuf) \
		EventDict.$(SrcSuf)

EVENT         = StEbyeEvent$(ExeSuf)
EVENTSO       = libEvent.$(DllSuf)

OBJS          = $(EVENTO)

PROGRAMS      = $(EVENT) 

all:            $(PROGRAMS)

$(EVENT):       $(EVENTO)
		$(LD) $(SOFLAGS) $(LDFLAGS) $(EVENTO) $(OutPutOpt) $(EVENTSO)
		@echo "$(EVENT) done"

clean:
		@rm -f $(OBJS) *Dict.* core

.SUFFIXES: .$(SrcSuf)

###

StEbyeEvent.$(ObjSuf): StEbyeEvent.h
StEbyeTrack.$(ObjSuf): StEbyeTrack.h

EventDict.$(SrcSuf): StEbyeEvent.h StEbyeTrack.h
	@echo "Generating dictionary EventDict..."
	@$(ROOTSYS)/bin/rootcint -f EventDict.$(SrcSuf) -c StEbyeTrack.h StEbyeEvent.h

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS) -c $<
