// -*- mode: c++;-*-
// $Id: StjTowerEnergyListCut.h,v 1.4 2008/08/03 00:26:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTCUT_H
#define STJTOWERENERGYLISTCUT_H

#include "StjTowerEnergyList.h"
#include "StjTowerEnergyCut.h"

class StjTowerEnergyListCut {

public:
  StjTowerEnergyListCut() { }
  virtual ~StjTowerEnergyListCut() { }
  
  StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);

  void addCut(StjTowerEnergyCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StjTowerEnergyCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjTowerEnergy& deposit);

  CutList _cutList;

};

#endif // STJTOWERENERGYLISTCUT_H
