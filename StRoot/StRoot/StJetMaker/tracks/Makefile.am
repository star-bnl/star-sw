# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjTPC.cxx \
	StjTPCNull.cxx \
	StjTPCTree.cxx \
	StjTPCTxt.cxx \
	StjTrackCut.cxx \
	StjTrackCutDca.cxx \
	StjTrackCutDcaPtDependent.cxx \
	StjTrackCutEta.cxx \
	StjTrackCutFlag.cxx \
	StjTrackCutNHits.cxx \
	StjTrackCutPossibleHitRatio.cxx \
	StjTrackList.cxx \
	StjTrackListCut.cxx \
	StjTrackPrint.cxx \
	StjTrackToFourVec.cxx \
	StjTrackToTLorentzVector.cxx \
	StjTrackTowerEnergyListToFourVecList.cxx

headers	= \
	StjTPC.h \
	StjTPCNull.h \
	StjTPCTree.h \
	StjTPCTxt.h \
	StjTrackCut.h \
	StjTrackCutDca.h \
	StjTrackCutDcaPtDependent.h \
	StjTrackCutEta.h \
	StjTrackCutFlag.h \
	StjTrackCutNHits.h \
	StjTrackCutPossibleHitRatio.h \
	StjTrackList.h \
	StjTrackListCut.h \
	StjTrackPrint.h \
	StjTrackToFourVec.h \
	StjTrackToTLorentzVector.h \
	StjTrackTowerEnergyListToFourVecList.h


lib_LTLIBRARIES = libStJetMakerTracks.la

libStJetMakerTracks_la_SOURCES = $(sources) $(headers) TracksLinkDef.h
libStJetMakerTracks_la_CXXFLAGS = -I../fourvec -I../towers -I../tree $(ROOTCFLAGS)
libStJetMakerTracks_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerTracks_la_SOURCES = TracksDict.cxx
BUILT_SOURCES = TracksDict.cxx
CLEANFILES = TracksDict.cxx TracksDict.h
TracksDict.cxx: $(headers) TracksLinkDef.h
	$(ROOTCINT) -f $@ -c -I../fourvec -I../towers $^
