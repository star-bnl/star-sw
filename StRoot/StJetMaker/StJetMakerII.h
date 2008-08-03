// -*- mode: c++;-*-
// $Id: StJetMakerII.h,v 1.10 2008/08/03 00:26:17 tai Exp $
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

class StjTPC;
class StjBEMC;
class StjTrackListCut;
class StjTowerEnergyListCut;
class StjFourVecListCut;
class StjTowerEnergyCorrectionForTracks;
class StjRunJetFinder;
class StjTowerEnergyVariation;
class StjJetListCut;

class StJetMakerII : public StMaker {

public:

  StJetMakerII(const Char_t *name, TDirectory* file, StjTreeEntryMaker* entryMaker);
  virtual ~StJetMakerII();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMakerII.h,v 1.10 2008/08/03 00:26:17 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StjTreeEntryMaker* _entryMaker;
  StBET4pMakerImp* _dataSource;

  StjTPC*  _tpc;
  StjBEMC* _bemc;

  StjTrackListCut* _tpcCut1;
  StjTrackListCut* _tpcCut2;

  StjTowerEnergyListCut* _bemcCut1;
  StjTowerEnergyListCut* _bemcCut2;

  StjTowerEnergyCorrectionForTracks* _towerEnergyCorrectionForTracks;

  StjTowerEnergyVariation* _energyVariationNull;
  StjTowerEnergyVariation* _energyVariationPlus5; 
  StjTowerEnergyVariation* _energyVariationMinus5;
  StjTowerEnergyVariation* _energyVariationPlus10;
  StjTowerEnergyVariation* _energyVariationMinus10;

  StjRunJetFinder *_jetFinder;
  StjTrackTowerEnergyListToFourVecList _toP4;

  StjFourVecListCut *_fourCut;

  StjJetListCut* _jetCut;

  StjJetListWriter* _jetTreeWriter0;
  StjJetListWriter* _jetTreeWriterP5;
  StjJetListWriter* _jetTreeWriterM5;
  StjJetListWriter* _jetTreeWriterP10;
  StjJetListWriter* _jetTreeWriterM10;

  ClassDef(StJetMakerII, 0)
};

#endif // STJETMAKERII_H
