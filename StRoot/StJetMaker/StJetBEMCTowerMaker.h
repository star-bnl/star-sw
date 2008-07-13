// -*- mode: c++;-*-
// $Id: StJetBEMCTowerMaker.h,v 1.3 2008/07/13 04:59:37 tai Exp $
#ifndef STJETBEMCTOWERMAKER_HH
#define STJETBEMCTOWERMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StMuDstMaker;

namespace StSpinJet {
  class StJetBEMC;
  class StJetBEMCEnergyCut;
}

class StJetBEMCTowerMaker : public StMaker {

public:

  StJetBEMCTowerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StJetBEMCTowerMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetBEMCTowerMaker.h,v 1.3 2008/07/13 04:59:37 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StSpinJet::StJetBEMC* _bemc;
  StSpinJet::StJetBEMCEnergyCut* _bemcCut;

  TTree* _tree;

  Int_t _runNumber;
  Int_t _eventId;
  Int_t _detectorId; // 9: BEMC, 13: EEMC
  Int_t _nTowers;
  Int_t    _towerId[4800];
  Double_t _towerX[4800];
  Double_t _towerY[4800];
  Double_t _towerZ[4800];
  Double_t _vertexX[4800];
  Double_t _vertexY[4800];
  Double_t _vertexZ[4800];
  Double_t _energy[4800];
  UInt_t   _adc[4800];
  Double_t _pedestal[4800];
  Double_t _rms[4800];
  Int_t    _status[4800];     // 1 is good for BEMC. 0 is good for EEMC

  ClassDef(StJetBEMCTowerMaker, 0)

};

#endif // STJETBEMCTOWERMAKER_HH
