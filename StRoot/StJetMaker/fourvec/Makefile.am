# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjFourVecCut.cxx \
	StjFourVecCutPt.cxx \
	StjFourVecCutTrgBHT.cxx \
	StjFourVecCutTrgBJP.cxx \
	StjFourVecList.cxx \
	StjFourVecListCut.cxx \
	StjFourVecPrint.cxx

headers	= \
	StjFourVecCut.h \
	StjFourVecCutPt.h \
	StjFourVecCutTrgBHT.h \
	StjFourVecCutTrgBJP.h \
	StjFourVecList.h \
	StjFourVecListCut.h \
	StjFourVecPrint.h

lib_LTLIBRARIES = libStJetMakerBase.la

libStJetMakerBase_la_SOURCES = $(sources) $(headers) BaseLinkDef.h
libStJetMakerBase_la_CXXFLAGS = -I../treer $(ROOTCFLAGS)
libStJetMakerBase_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerBase_la_SOURCES = BaseDict.cxx
BUILT_SOURCES = BaseDict.cxx
CLEANFILES = BaseDict.cxx BaseDict.h
BaseDict.cxx: $(headers) BaseLinkDef.h
	$(ROOTCINT) -f $@ -c $^
