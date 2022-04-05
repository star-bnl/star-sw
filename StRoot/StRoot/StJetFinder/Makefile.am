# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StCdfChargedConeJetFinder.cxx \
	StCdfChargedConePars.cxx \
	StCdfChargedJetEtCell.cxx \
	StCdfChargedJetEtCellFactory.cxx \
	StConeJetFinder.cxx \
	StConeJetFinderBase.cxx \
	StConePars.cxx \
	StEtaPhiCell.cxx \
	StEtaPhiGrid.cxx \
	StJetEtCell.cxx \
	StJetEtCellFactory.cxx \
	StJetPars.cxx \
	StJetSpliterMerger.cxx \
	StKtCluJetFinder.cxx \
	StKtCluPars.cxx \
	StProtoJet.cxx \
	StProtoJetPair.cxx

headers = \
	AbstractFourVec.h \
	FourVec.h \
	Functors.h \
	StCdfChargedConeJetFinder.h \
	StCdfChargedConePars.h \
	StCdfChargedJetEtCell.h \
	StCdfChargedJetEtCellFactory.h \
	StConeJetFinder.h \
	StConeJetFinderBase.h \
	StConePars.h \
	StEtGridKey.h \
	StEtaPhiCell.h \
	StEtaPhiGrid.h \
	StJetEtCell.h \
	StJetEtCellFactory.h \
	StJetFinder.h \
	StJetPars.h \
	StJetSpliterMerger.h \
	StKtCluJetFinder.h \
	StKtCluPars.h \
	StProtoJet.h \
	StProtoJetPair.h

lib_LTLIBRARIES = libStJetFinder.la

libStJetFinder_la_SOURCES = $(sources) $(headers) StJetFinderLinkDef.h
libStJetFinder_la_CXXFLAGS = $(ROOTCFLAGS)
libStJetFinder_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetFinder_la_SOURCES = StJetFinderDict.cxx
BUILT_SOURCES = StJetFinderDict.cxx
CLEANFILES = StJetFinderDict.cxx StJetFinderDict.h
StJetFinderDict.cxx: $(headers) StJetFinderLinkDef.h
	$(ROOTCINT) -f $@ -c $^
