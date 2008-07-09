// -*- mode: c++;-*-
// $Id: EnergyListToFourList.h,v 1.1 2008/07/09 10:58:09 tai Exp $
#ifndef ENERGYLISTTOFOURLIST_H
#define ENERGYLISTTOFOURLIST_H

#include <StJetFinder/AbstractFourVec.h>

#include <TowerEnergyDeposit.h>

#include <TLorentzVector.h>

typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class EnergyListToFourList {

public:
  EnergyListToFourList() { }
  virtual ~EnergyListToFourList() { }

  FourList operator()(const TowerEnergyDepositList& energyDepositList);

private:

  TLorentzVector constructFourMomentum(const TowerEnergyDeposit& deposit);

};

}


#endif // ENERGYLISTTOFOURLIST_H
