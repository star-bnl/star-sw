// -*- mode: c++;-*-
// $Id: StJetTowerEnergyListWriter.h,v 1.2 2008/07/25 01:06:02 tai Exp $
#ifndef STJETTOWERENERGYLISTWRITER_HH
#define STJETTOWERENERGYLISTWRITER_HH

#include "TowerEnergyList.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StJetTowerEnergyListWriter {

public:

  StJetTowerEnergyListWriter(const char* treeName, TDirectory* file);
  virtual ~StJetTowerEnergyListWriter() { }

  void Fill(const StSpinJet::TowerEnergyList& theList);
  void Finish();
    
private:

  TDirectory* _file;
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

#endif // STJETTOWERENERGYLISTWRITER_HH
