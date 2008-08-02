// -*- mode: c++;-*-
// $Id: StjTowerEnergyListCut.h,v 1.3 2008/08/02 22:43:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTCUT_H
#define STJTOWERENERGYLISTCUT_H

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

#endif // STJTOWERENERGYLISTCUT_H
