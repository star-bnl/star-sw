// -*- mode: c++;-*-
// $Id: TowerEnergyListToFourList.h,v 1.1 2008/07/17 06:36:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYLISTTOFOURLIST_H
#define TOWERENERGYLISTTOFOURLIST_H

#include <TowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class TowerEnergyToTLorentzVector;

class TowerEnergyListToFourList {

public:
  TowerEnergyListToFourList();
  virtual ~TowerEnergyListToFourList() { }

  FourList operator()(const TowerEnergyList& energyDepositList);

private:

  TowerEnergyToTLorentzVector& _energyTo4p;

};

}


#endif // TOWERENERGYLISTTOFOURLIST_H
