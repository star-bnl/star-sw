// -*- mode: c++;-*-
// $Id: TowerEnergyDeposit.h,v 1.2 2008/07/08 10:35:31 tai Exp $
#ifndef TOWERENERGYDEPOSIT_H
#define TOWERENERGYDEPOSIT_H

#include <StDetectorId.h>

#include <TVector3.h>

namespace StSpinJet {

struct TowerEnergyDeposit {
  StDetectorId   detectorId;
  int            towerId;
  TVector3       towerLocation;
  TVector3       vertex;
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
