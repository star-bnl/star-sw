// -*- mode: c++;-*-
// $Id: StjTowerEnergyListReader.h,v 1.4 2008/08/03 00:29:13 tai Exp $
#ifndef STJTOWERENERGYLISTREADER_H
#define STJTOWERENERGYLISTREADER_H

#include "StjTowerEnergyList.h"
#include <Rtypes.h>

class TTree;

class StjTowerEnergyListReader {

public:
  StjTowerEnergyListReader(TTree *tree);
  virtual ~StjTowerEnergyListReader() { }

  StjTowerEnergyList GetEntry(Long64_t entry);

private:

  TTree* _tree;

  Int_t    _runNumber;
  Int_t    _eventId;
  Int_t    _detectorId; // 9: BEMC, 13: EEMC
  Int_t    _nTowers;
  Int_t    _towerId[4800];
  Double_t _towerR[4800];
  Double_t _towerEta[4800];
  Double_t _towerPhi[4800];
  Double_t _vertexX;
  Double_t _vertexY;
  Double_t _vertexZ;
  Double_t _energy[4800];
  UInt_t   _adc[4800];
  Double_t _pedestal[4800];
  Double_t _rms[4800];
  Int_t    _status[4800];     // 1 is good for BEMC. 0 is good for EEMC

};

#endif // STJTOWERENERGYLISTREADER_H
