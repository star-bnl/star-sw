// -*- mode: c++;-*-
// $Id: EnergyListToFourList.h,v 1.4 2008/07/15 03:42:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef ENERGYLISTTOFOURLIST_H
#define ENERGYLISTTOFOURLIST_H

#include <TowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class TowerEnergyToTLorentzVector;

class EnergyListToFourList {

public:
  EnergyListToFourList();
  virtual ~EnergyListToFourList() { }

  FourList operator()(const TowerEnergyList& energyDepositList);

private:

  TowerEnergyToTLorentzVector& _energyTo4p;

};

}


#endif // ENERGYLISTTOFOURLIST_H
