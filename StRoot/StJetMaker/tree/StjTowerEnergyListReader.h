// -*- mode: c++;-*-
// $Id: StjTowerEnergyListReader.h,v 1.6 2008/08/11 03:51:00 tai Exp $
#ifndef STJTOWERENERGYLISTREADER_H
#define STJTOWERENERGYLISTREADER_H

#include "StjTreeReader.h"

#include "StjTowerEnergyList.h"
#include <Rtypes.h>

class TTree;

class StjTowerEnergyListReader : public StjTreeReader {

public:
  StjTowerEnergyListReader(TTree *tree) : StjTreeReader(tree) { }
  virtual ~StjTowerEnergyListReader() { }

  StjTowerEnergyList getEnergyList() { return _list; }

private:

  void SetBranchAddress(TTree *tree);

  void clearEntry();
  void readEntry();

  StjTowerEnergyList _list;

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

  ClassDef(StjTowerEnergyListReader, 1)

};

#endif // STJTOWERENERGYLISTREADER_H
