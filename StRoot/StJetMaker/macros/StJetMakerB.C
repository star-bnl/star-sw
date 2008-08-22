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
#include <StjJetCutTrgBHT.h>
#include <StjJetCutTrgBJP.h>
#include <StjJetCutDetectorEta.h>

#include <StjTrgBEMCJetPatchTowerIdMap2005.h>

#include <StjJetListWriter.h>
#include <StjDijetListWriter.h>

#include <StjTrackPrint.h>
#include <StjTowerEnergyPrint.h>
#include <StjFourVecPrint.h>
#include <StjJetPrint.h>


#include <StjDijetList.h>
#include <StjDijetPrint.h>

#include <StjFormDijet.h>

#include <StjTrg.h>
#include <StjVertex.h>

#include <TDirectory.h>

class StJetMakerB : public StMaker {

public:
  
  StJetMakerB(const Char_t *name,
	      StjTPC* tpc, StjBEMC* bemc,
	      StjVertex* vertex,
	      StjTrg* trgBJP1, StjTrg* trgBJP2, StjTrg* trgBHT1, StjTrg* trgBHT2,
	      TDirectory* file)
    : StMaker(name), _file(file)
    , _tpc(tpc), _bemc(bemc)
    , _vertex(vertex)
    , _trgBJP1(trgBJP1), _trgBJP2(trgBJP2), _trgBHT1(trgBHT1), _trgBHT2(trgBHT2)
  { }
  virtual ~StJetMakerB() { }

private:

  TDirectory* _file;

  StjTPC*  _tpc;
  StjBEMC* _bemc;

  StjVertex* _vertex;

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
  StjJetListCut _jetCutBHT2;
  StjJetListCut _jetCutBJP2;
  StjJetListCut _jetCut2;

  StjJetListWriter* _jetListWriter;

  StjFormDijet _formDijet;

  StjDijetListWriter* _dijetListWriter;


  StjTrg* _trgBJP1;
  StjTrg* _trgBJP2;
  StjTrg* _trgBHT1;
  StjTrg* _trgBHT2;


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

    _jetCutBHT2.addCut(new StjJetCutTrgBHT(_trgBHT2));
    _jetCutBJP2.addCut(new StjJetCutTrgBJP(_trgBJP2, new StjTrgBEMCJetPatchTowerIdMap2005));

    _jetListWriter = new StjJetListWriter("jets", "fours", _file);

    _jetCut2.addCut(new StjJetCutDetectorEta(0.2, 0.8));

    _dijetListWriter = new StjDijetListWriter("dijets", _file);

    return kStOk;
  }

  Int_t Make()
  {
    StjTrackPrint trackprint;
    StjTowerEnergyPrint towerprint;
    StjFourVecPrint fourprint;
    StjJetPrint jetprint;
    StjDijetPrint dijetprint;

    StjTrackList trackList = _tpc->getTrackList();
    StjTowerEnergyList energyList = _bemc->getEnergyList();

    //    towerprint(energyList);

    trackList = _tpcCut1(trackList);
    energyList = _bemcCut1(energyList);

    energyList   = _towerEnergyCorrectionForTracks(energyList, trackList);

    trackList = _tpcCut2(trackList);

    energyList   = _bemcCut2(energyList);

    StjFourVecList fourList   = _toP4(trackList, energyList);

    fourList   = _fourCut(fourList);

    vector<StjJet> jetList   = _jetFinder(fourList);

    jetList = _jetCut(jetList);

    vector<StjJet> jetListBHT2 = _jetCutBHT2(jetList);
    vector<StjJet> jetListBJP2 = _jetCutBJP2(jetList);

    _jetListWriter->Fill(jetList, fourList);

    jetList = _jetCut2(jetList);

    StjDijetList dijetList = _formDijet(jetList);

    dijetprint(dijetList);

    _dijetListWriter->Fill(dijetList);

    return kStOk;
  }

  Int_t Finish()
  {
    _jetListWriter->Finish();

    _dijetListWriter->Finish();

    return kStOk;
  }

  ClassDef(StJetMakerB, 1)
};
