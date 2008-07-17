// -*- mode: c++;-*-
// $Id: StJetMakerII.h,v 1.2 2008/07/17 02:19:09 tai Exp $
#ifndef STJETMAKERII_HH
#define STJETMAKERII_HH

#include "StMaker.h"

#include "TrackTowerEnergyListToFourList.h"

#include <vector>

class TTree;

class StJetPars;
class StJetTreeEntryMaker;
class StBET4pMakerImp;

namespace StSpinJet {
class StJetTPC;
class StJetBEMC;
class StJetTPCTrackCut;
class StJetBEMCEnergyCut;
class CorrectTowerEnergyForTracks;
class RunJetFinder;
class StJetTowerEnergyVariation;
}

class StJetMakerII : public StMaker {

public:

  StJetMakerII(const Char_t *name, StJetTreeEntryMaker* entryMaker);
  virtual ~StJetMakerII();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMakerII.h,v 1.2 2008/07/17 02:19:09 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  StJetTreeEntryMaker* _entryMaker;
  StBET4pMakerImp* _dataSource;

  StSpinJet::StJetTPC*  _tpc;
  StSpinJet::StJetBEMC* _bemc;

  StSpinJet::StJetTPCTrackCut* _tpcCut1;
  StSpinJet::StJetTPCTrackCut* _tpcCut2;

  StSpinJet::StJetBEMCEnergyCut* _bemcCut;

  StSpinJet::CorrectTowerEnergyForTracks* _towerEnergyCorrectionForTracks;

  StSpinJet::StJetTowerEnergyVariation* _energyVariationNull;
  StSpinJet::StJetTowerEnergyVariation* _energyVariationPlus5; 
  StSpinJet::StJetTowerEnergyVariation* _energyVariationMinus5;
  StSpinJet::StJetTowerEnergyVariation* _energyVariationPlus10;
  StSpinJet::StJetTowerEnergyVariation* _energyVariationMinus10;

  StSpinJet::RunJetFinder *_jetFinder;
  StSpinJet::TrackTowerEnergyListToFourList _toP4;

  ClassDef(StJetMakerII, 0)

};

#endif // STJETMAKER_HH
