// -*- mode: c++;-*-
// $Id: StjTowerEnergyListCut.h,v 1.1 2008/08/02 04:15:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMCENERGYCUT_H
#define STJETBEMCENERGYCUT_H

#include "StjTowerEnergyList.h"
#include "StjTowerEnergyCut.h"

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
