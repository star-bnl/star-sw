#include <StMaker.h>

#include <StjTPC.h>
#include <StjBEMC.h>

#include <StjTrackListCut.h>
#include <StjTrackCutDca.h>
#include <StjTrackCutEta.h>
#include <StjTrackCutPossibleHitRatio.h>
#include <StjTrackCutFlag.h>
#include <StjTrackCutNHits.h>

#include <StjTowerEnergyListCut.h>
#include <StjTowerEnergyCutBemcWestOnly.h>
#include <StjTowerEnergyCutEnergy.h>
#include <StjTowerEnergyCutBemcStatus.h>
#include <StjTowerEnergyCutAdc.h>

#include <StjTowerEnergyCorrectionForTracks.h>

#include <StjTowerEnergyListVariation.h>
#include <StjTowerEnergyVariationEnergy.h>

#include <StjTrackTowerEnergyListToFourVecList.h>

#include <StjFourVecListCut.h>
#include <StjFourVecCutPt.h>

#include <StjRunJetFinder.h>
#include <StJetFinder/StConePars.h>

#include <StjJetListCut.h>
#include <StjJetCutPt.h>

#include <StjTrackPrint.h>
#include <StjTowerEnergyPrint.h>
#include <StjFourVecPrint.h>
#include <StjJetPrint.h>


class StJetMakerA : public StMaker {

public:
  
  StJetMakerA(const Char_t *name, StjTPC* tpc, StjBEMC* bemc)
    : _tpc(tpc), _bemc(bemc) { }
  virtual ~StJetMakerA() { }

private:

  StjTPC*  _tpc;
  StjBEMC* _bemc;

  StjTrackListCut _tpcCut1;
  StjTrackListCut _tpcCut2;

  StjTowerEnergyListCut _bemcCut1;
  StjTowerEnergyListCut _bemcCut2;

  StjTowerEnergyCorrectionForTracks _towerEnergyCorrectionForTracks;

  StjTowerEnergyListVariation _energyVariationNull;
  StjTowerEnergyListVariation _energyVariationPlus5; 
  StjTowerEnergyListVariation _energyVariationMinus5;
  StjTowerEnergyListVariation _energyVariationPlus10;
  StjTowerEnergyListVariation _energyVariationMinus10;

  StjTrackTowerEnergyListToFourVecList _toP4;

  StjFourVecListCut _fourCut;

  StjRunJetFinder _jetFinder;

  StjJetListCut _jetCut;

  Int_t Init()
  {
    _tpcCut1.addCut(new StjTrackCutDca(3.0));
    _tpcCut1.addCut(new StjTrackCutEta(-2.0, 2.0));
    _tpcCut1.addCut(new StjTrackCutPossibleHitRatio(0.51));

    _bemcCut1.addCut(new StjTowerEnergyCutBemcWestOnly());
    _bemcCut1.addCut(new StjTowerEnergyCutEnergy(0.0));
    _bemcCut1.addCut(new StjTowerEnergyCutBemcStatus(1));
    _bemcCut1.addCut(new StjTowerEnergyCutAdc(0, 2.0));

    _tpcCut2.addCut(new StjTrackCutFlag(0));
    _tpcCut2.addCut(new StjTrackCutNHits(20));

    _bemcCut2.addCut(new StjTowerEnergyCutEnergy(0.0));

    _energyVariationNull.addVariation(new StjTowerEnergyVariationEnergy(0));
    _energyVariationPlus5.addVariation(new StjTowerEnergyVariationEnergy(0.05));
    _energyVariationMinus5.addVariation(new StjTowerEnergyVariationEnergy(-0.05));
    _energyVariationPlus10.addVariation(new StjTowerEnergyVariationEnergy(0.1));
    _energyVariationMinus10.addVariation(new StjTowerEnergyVariationEnergy(-0.1));

    _fourCut.addCut(new StjFourVecCutPt(0.2));

    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(56, -1.6, 1.6, 120, -3.141592613589793, 3.141592613589793);
    cpars->setConeRadius(0.4);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.5);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    _jetFinder.Init(cpars);

    _jetCut.addCut(new StjJetCutPt(5.0));

    return kStOk;
  }

  Int_t Make()
  {
    StjTrackPrint trackprint;
    StjTowerEnergyPrint towerprint;
    StjFourVecPrint fourprint;
    StjJetPrint jetprint;

    StjTrackList trackList = _tpc->getTrackList();
    StjTowerEnergyList energyList = _bemc->getEnergyList();

    towerprint(energyList);

    trackList = _tpcCut1(trackList);
    energyList = _bemcCut1(energyList);

    StjTowerEnergyList energyList0   =  _energyVariationNull(energyList);
    StjTowerEnergyList energyListP5  = _energyVariationPlus5(energyList);
    StjTowerEnergyList energyListM5  = _energyVariationMinus5(energyList);
    StjTowerEnergyList energyListP10 = _energyVariationPlus10(energyList);
    StjTowerEnergyList energyListM10 = _energyVariationMinus10(energyList);

    energyList0   = _towerEnergyCorrectionForTracks(energyList0, trackList);
    energyListP5  = _towerEnergyCorrectionForTracks(energyListP5, trackList);
    energyListM5  = _towerEnergyCorrectionForTracks(energyListM5, trackList);
    energyListP10 = _towerEnergyCorrectionForTracks(energyListP10, trackList);
    energyListM10 = _towerEnergyCorrectionForTracks(energyListM10, trackList);

    trackList = _tpcCut2(trackList);

    energyList0   = _bemcCut2(energyList0);
    energyListP5  = _bemcCut2(energyListP5);
    energyListM5  = _bemcCut2(energyListM5);
    energyListP10 = _bemcCut2(energyListP10);
    energyListM10 = _bemcCut2(energyListM10);

    StjFourVecList fourList0   = _toP4(trackList, energyList0);
    StjFourVecList fourListP5  = _toP4(trackList, energyListP5);
    StjFourVecList fourListM5  = _toP4(trackList, energyListM5);
    StjFourVecList fourListP10 = _toP4(trackList, energyListP10);
    StjFourVecList fourListM10 = _toP4(trackList, energyListM10);

    fourList0   = _fourCut(fourList0);
    fourListP5  = _fourCut(fourListP5);
    fourListM5  = _fourCut(fourListM5);
    fourListP10 = _fourCut(fourListP10);
    fourListM10 = _fourCut(fourListM10);

    vector<StjJet> jetList0   = _jetFinder(fourList0);
    vector<StjJet> jetListP5  = _jetFinder(fourListP5);
    vector<StjJet> jetListM5  = _jetFinder(fourListM5);
    vector<StjJet> jetListP10 = _jetFinder(fourListP10);
    vector<StjJet> jetListM10 = _jetFinder(fourListM10);

    jetList0   = _jetCut(jetList0);
    jetListP5  = _jetCut(jetListP5);
    jetListM5  = _jetCut(jetListM5);
    jetListP10 = _jetCut(jetListP10);
    jetListM10 = _jetCut(jetListM10);

    jetprint(jetList0);
    jetprint(jetListP5);
    jetprint(jetListM5);
    jetprint(jetListP10);
    jetprint(jetListM10);

    return kStOk;
  }

  Int_t Finish()
  {
    return kStOk;
  }

  ClassDef(StJetMakerA, 1)
};
