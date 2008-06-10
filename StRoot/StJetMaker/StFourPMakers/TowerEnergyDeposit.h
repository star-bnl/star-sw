// -*- mode: c++;-*-
// $Id: TowerEnergyDeposit.h,v 1.1 2008/06/10 08:07:09 tai Exp $
#ifndef TOWERENERGYDEPOSIT_H
#define TOWERENERGYDEPOSIT_H

#include <StDetectorId.h>

#include <TVector3.h>

namespace StSpinJet {

struct TowerEnergyDeposit {
  StDetectorId detectorId;
  int towerId;
  TVector3 towerLocation;
  double energy;
};

typedef std::vector<TowerEnergyDeposit> TowerEnergyDepositList;

}

#endif // TOWERENERGYDEPOSIT_H
