// -*- mode: c++;-*-
// $Id: StJetBEMCEnergyCut.h,v 1.7 2008/07/16 22:28:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMCENERGYCUT_H
#define STJETBEMCENERGYCUT_H

#include "TowerEnergyList.h"
#include "TowerEnergyCut.h"

namespace StSpinJet {

class StJetBEMCEnergyCut {

public:
  StJetBEMCEnergyCut() { }
  virtual ~StJetBEMCEnergyCut() { }
  
  TowerEnergyList operator()(const TowerEnergyList& energyList);

  void addCut(StJetTowerEnergyCut::TowerEnergyCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetTowerEnergyCut::TowerEnergyCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const TowerEnergy& deposit);

  CutList _cutList;

};

}

#endif // STJETBEMCENERGYCUT_H
