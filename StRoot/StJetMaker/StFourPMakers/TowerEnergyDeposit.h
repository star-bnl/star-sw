// -*- mode: c++;-*-
// $Id: TowerEnergyDeposit.h,v 1.3 2008/07/08 11:21:57 tai Exp $
#ifndef TOWERENERGYDEPOSIT_H
#define TOWERENERGYDEPOSIT_H

#include <StDetectorId.h>

#include <vector>

namespace StSpinJet {

struct TowerEnergyDeposit {
  StDetectorId   detectorId;
  int            towerId;
  double         towerX;
  double         towerY;
  double         towerZ;
  double         vertexX;
  double         vertexY;
  double         vertexZ;
  double         energy;
  unsigned int   adc;
  float          pedestal;
  float          rms;
  int            status;
};

typedef std::vector<TowerEnergyDeposit> TowerEnergyDepositList;

}

#endif // TOWERENERGYDEPOSIT_H
