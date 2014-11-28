# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjBEMC.cxx \
	StjBEMCNull.cxx \
	StjBEMCTree.cxx \
	StjBEMCTxt.cxx \
	StjEEMC.cxx \
	StjEEMCNull.cxx \
	StjEEMCTxt.cxx \
	StjTowerEnergyCorrectionForTracks.cxx \
	StjTowerEnergyCut.cxx \
	StjTowerEnergyCut2003BemcTower.cxx \
	StjTowerEnergyCutAdc.cxx \
	StjTowerEnergyCutBemcStatus.cxx \
	StjTowerEnergyCutBemcWestOnly.cxx \
	StjTowerEnergyCutEnergy.cxx \
	StjTowerEnergyCutEt.cxx \
	StjTowerEnergyCutTowerId.cxx \
	StjTowerEnergyList.cxx \
	StjTowerEnergyListCut.cxx \
	StjTowerEnergyListVariation.cxx \
	StjTowerEnergyPrint.cxx \
	StjTowerEnergyToFourVec.cxx \
	StjTowerEnergyToTLorentzVector.cxx \
	StjTowerEnergyVariation.cxx \
	StjTowerEnergyVariationEnergy.cxx

headers	= \
	StjBEMC.h \
	StjBEMCNull.h \
	StjBEMCTree.h \
	StjBEMCTxt.h \
	StjEEMC.h \
	StjEEMCNull.h \
	StjEEMCTxt.h \
	StjTowerEnergyCorrectionForTracks.h \
	StjTowerEnergyCut.h \
	StjTowerEnergyCut2003BemcTower.h \
	StjTowerEnergyCutAdc.h \
	StjTowerEnergyCutBemcStatus.h \
	StjTowerEnergyCutBemcWestOnly.h \
	StjTowerEnergyCutEnergy.h \
	StjTowerEnergyCutEt.h \
	StjTowerEnergyCutTowerId.h \
	StjTowerEnergyList.h \
	StjTowerEnergyListCut.h \
	StjTowerEnergyListVariation.h \
	StjTowerEnergyPrint.h \
	StjTowerEnergyToFourVec.h \
	StjTowerEnergyToTLorentzVector.h \
	StjTowerEnergyVariation.h \
	StjTowerEnergyVariationEnergy.h


lib_LTLIBRARIES = libStJetMakerTowers.la

libStJetMakerTowers_la_SOURCES = $(sources) $(headers) TowersLinkDef.h
libStJetMakerTowers_la_CXXFLAGS = -I../tracks -I../fourvec -I../tree -I../trigger $(ROOTCFLAGS)
libStJetMakerTowers_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerTowers_la_SOURCES = TowersDict.cxx
BUILT_SOURCES = TowersDict.cxx
CLEANFILES = TowersDict.cxx TowersDict.h
TowersDict.cxx: $(headers) TowersLinkDef.h
	$(ROOTCINT) -f $@ -c -I../tracks -I../fourvec $^
