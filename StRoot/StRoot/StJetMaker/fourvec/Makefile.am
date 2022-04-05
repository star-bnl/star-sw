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

lib_LTLIBRARIES = libStJetMakerFourvec.la

libStJetMakerFourvec_la_SOURCES = $(sources) $(headers) FourvecLinkDef.h
libStJetMakerFourvec_la_CXXFLAGS = -I../trigger -I../tree $(ROOTCFLAGS)
libStJetMakerFourvec_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerFourvec_la_SOURCES = FourvecDict.cxx
BUILT_SOURCES = FourvecDict.cxx
CLEANFILES = FourvecDict.cxx FourvecDict.h
FourvecDict.cxx: $(headers) FourvecLinkDef.h
	$(ROOTCINT) -f $@ -c -I../trigger $^
