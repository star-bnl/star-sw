# Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>

sources	= \
	StjBEMC.cxx \
	StjBEMCNull.cxx \
	StjBEMCTree.cxx \
	StjBEMCTxt.cxx \
	StjEEMC.cxx \
	StjEEMCNull.cxx \
	StjEEMCTxt.cxx \
	StjFourVecCut.cxx \
	StjFourVecCutPt.cxx \
	StjFourVecCutTrgBHT.cxx \
	StjFourVecCutTrgBJP.cxx \
	StjFourVecList.cxx \
	StjFourVecListCut.cxx \
	StjFourVecPrint.cxx \
	StjMC.cxx \
	StjMCNull.cxx \
	StjMCParticleCut.cxx \
	StjMCParticleCutEta.cxx \
	StjMCParticleCutEtaForStatus.cxx \
	StjMCParticleCutFirstMotherId.cxx \
	StjMCParticleCutStatus.cxx \
	StjMCParticleList.cxx \
	StjMCParticleListCut.cxx \
	StjMCParticleListToFourVecList.cxx \
	StjMCParticlePrint.cxx \
	StjMCParticleToFourVec.cxx \
	StjMCTree.cxx \
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
	StjFourVecCut.h \
	StjFourVecCutPt.h \
	StjFourVecCutTrgBHT.h \
	StjFourVecCutTrgBJP.h \
	StjFourVecList.h \
	StjFourVecListCut.h \
	StjFourVecPrint.h \
	StjMC.h \
	StjMCNull.h \
	StjMCParticleCut.h \
	StjMCParticleCutEta.h \
	StjMCParticleCutEtaForStatus.h \
	StjMCParticleCutFirstMotherId.h \
	StjMCParticleCutStatus.h \
	StjMCParticleList.h \
	StjMCParticleListCut.h \
	StjMCParticleListToFourVecList.h \
	StjMCParticlePrint.h \
	StjMCParticleToFourVec.h \
	StjMCTree.h \
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


lib_LTLIBRARIES = libStJetMakerBase.la

libStJetMakerBase_la_SOURCES = $(sources) $(headers) BaseLinkDef.h
libStJetMakerBase_la_CXXFLAGS = -I../tracks -I../tree -I../trigger $(ROOTCFLAGS)
libStJetMakerBase_la_LDFLAGS = -version-info 1:0:0
nodist_libStJetMakerBase_la_SOURCES = BaseDict.cxx
BUILT_SOURCES = BaseDict.cxx
CLEANFILES = BaseDict.cxx BaseDict.h
BaseDict.cxx: $(headers) BaseLinkDef.h
	$(ROOTCINT) -f $@ -c -I../tracks $^
