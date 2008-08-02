// -*- mode: c++;-*-
// $Id: StJetMakerII.h,v 1.9 2008/08/02 22:43:05 tai Exp $
#ifndef STJETMAKERII_H
#define STJETMAKERII_H

#include "StMaker.h"

#include "StjTrackTowerEnergyListToFourVecList.h"

#include <vector>

class TTree;
class TDirectory;

class StJetPars;
class StjTreeEntryMaker;
class StBET4pMakerImp;

class StjJetListWriter;

namespace StSpinJet {
class StjTPC;
class StjBEMC;
class StjTrackListCut;
class StjTowerEnergyListCut;
class StjFourVecListCut;
class StjTowerEnergyCorrectionForTracks;
class RunJetFinder;
class StjTowerEnergyVariation;
class StjJetListCut;
}


class StJetMakerII : public StMaker {

public:

  StJetMakerII(const Char_t *name, TDirectory* file, StjTreeEntryMaker* entryMaker);
  virtual ~StJetMakerII();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMakerII.h,v 1.9 2008/08/02 22:43:05 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StjTreeEntryMaker* _entryMaker;
  StBET4pMakerImp* _dataSource;

  StSpinJet::StjTPC*  _tpc;
  StSpinJet::StjBEMC* _bemc;

  StSpinJet::StjTrackListCut* _tpcCut1;
  StSpinJet::StjTrackListCut* _tpcCut2;

  StSpinJet::StjTowerEnergyListCut* _bemcCut1;
  StSpinJet::StjTowerEnergyListCut* _bemcCut2;

  StSpinJet::StjTowerEnergyCorrectionForTracks* _towerEnergyCorrectionForTracks;

  StSpinJet::StjTowerEnergyVariation* _energyVariationNull;
  StSpinJet::StjTowerEnergyVariation* _energyVariationPlus5; 
  StSpinJet::StjTowerEnergyVariation* _energyVariationMinus5;
  StSpinJet::StjTowerEnergyVariation* _energyVariationPlus10;
  StSpinJet::StjTowerEnergyVariation* _energyVariationMinus10;

  StSpinJet::RunJetFinder *_jetFinder;
  StSpinJet::StjTrackTowerEnergyListToFourVecList _toP4;

  StSpinJet::StjFourVecListCut *_fourCut;

  StSpinJet::StjJetListCut* _jetCut;

  StjJetListWriter* _jetTreeWriter0;
  StjJetListWriter* _jetTreeWriterP5;
  StjJetListWriter* _jetTreeWriterM5;
  StjJetListWriter* _jetTreeWriterP10;
  StjJetListWriter* _jetTreeWriterM10;

  ClassDef(StJetMakerII, 0)
};

#endif // STJETMAKERII_H
