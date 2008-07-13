// -*- mode: c++;-*-
// $Id: EnergyListToFourList.h,v 1.3 2008/07/13 10:02:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef ENERGYLISTTOFOURLIST_H
#define ENERGYLISTTOFOURLIST_H

#include <StJetFinder/AbstractFourVec.h>

#include <TowerEnergyList.h>

#include <TLorentzVector.h>

typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class EnergyListToFourList {

public:
  EnergyListToFourList() { }
  virtual ~EnergyListToFourList() { }

  FourList operator()(const TowerEnergyList& energyDepositList);

private:

  TLorentzVector constructFourMomentum(const TowerEnergy& deposit);

};

}


#endif // ENERGYLISTTOFOURLIST_H
