# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjJetCut.cxx \
	StjJetCutDetectorEta.cxx \
	StjJetCutEta.cxx \
	StjJetCutNFourVecs.cxx \
	StjJetCutPt.cxx \
	StjJetCutRt.cxx \
	StjJetCutTrgBHT.cxx \
	StjJetCutTrgBJP.cxx \
	StjJetList.cxx \
	StjJetListCut.cxx \
	StjJetListVariation.cxx \
	StjJetVariation.cxx \
	StjJetVariationTrack.cxx \
	StjJetPrint.cxx

headers	= \
	StjJetCut.h \
	StjJetCutDetectorEta.h \
	StjJetCutEta.h \
	StjJetCutNFourVecs.h \
	StjJetCutPt.h \
	StjJetCutRt.h \
	StjJetCutTrgBHT.h \
	StjJetCutTrgBJP.h \
	StjJetList.h \
	StjJetListCut.h \
	StjJetListVariation.h \
	StjJetVariation.h \
	StjJetVariationTrack.h \
	StjJetPrint.h

lib_LTLIBRARIES = libStJetMakerJets.la

libStJetMakerJets_la_SOURCES = $(sources) $(headers) JetsLinkDef.h
libStJetMakerJets_la_CXXFLAGS = -I../tree -I../trigger -I../fourvec $(ROOTCFLAGS)
libStJetMakerJets_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerJets_la_SOURCES = JetsDict.cxx
BUILT_SOURCES = JetsDict.cxx
CLEANFILES = JetsDict.cxx JetsDict.h
JetsDict.cxx: $(headers) JetsLinkDef.h
	$(ROOTCINT) -f $@ -c -I../fourvec $^
