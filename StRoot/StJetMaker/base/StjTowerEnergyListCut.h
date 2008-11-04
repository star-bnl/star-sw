// -*- mode: c++;-*-
// $Id: StjTowerEnergyListCut.h,v 1.7 2008/11/04 05:54:40 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTCUT_H
#define STJTOWERENERGYLISTCUT_H

#include "StkListCut.h"
#include "StjTowerEnergyList.h"

typedef StkListCut<StjTowerEnergy> StjTowerEnergyListCut;

// 
// #include <TObject.h>
// 
// #include "StjTowerEnergyList.h"
// #include "StjTowerEnergyCut.h"
// 
// #include <vector>
// 
// class StjTowerEnergyListCut : public TObject {
// 
// public:
//   StjTowerEnergyListCut() { }
//   virtual ~StjTowerEnergyListCut() { }
//   
//   StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);
// 
//   void addCut(StjTowerEnergyCut* cut) {
//     _cutList.push_back(cut);
//   }
// 
//   typedef std::vector<StjTowerEnergyCut*> CutList;
//   CutList getCutList() { return _cutList; }
// 
// private:
// 
//   bool shouldNotKeep(const StjTowerEnergy& deposit);
// 
//   CutList _cutList;
// 
//   ClassDef(StjTowerEnergyListCut, 1)
// 
// };

#endif // STJTOWERENERGYLISTCUT_H
