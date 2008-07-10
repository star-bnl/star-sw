// -*- mode: c++;-*-
// $Id: EnergyListToFourList.h,v 1.2 2008/07/10 20:15:20 tai Exp $
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
