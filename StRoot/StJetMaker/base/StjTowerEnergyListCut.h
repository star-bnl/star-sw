// -*- mode: c++;-*-
// $Id: StjTowerEnergyListCut.h,v 1.2 2008/08/02 19:22:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMCENERGYCUT_H
#define STJETBEMCENERGYCUT_H

#include "StjTowerEnergyList.h"
#include "StjTowerEnergyCut.h"

namespace StSpinJet {

class StjTowerEnergyListCut {

public:
  StjTowerEnergyListCut() { }
  virtual ~StjTowerEnergyListCut() { }
  
  StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);

  void addCut(StJetTowerEnergyCut::StjTowerEnergyCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetTowerEnergyCut::StjTowerEnergyCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjTowerEnergy& deposit);

  CutList _cutList;

};

}

#endif // STJETBEMCENERGYCUT_H
