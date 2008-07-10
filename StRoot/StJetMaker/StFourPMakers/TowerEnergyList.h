// -*- mode: c++;-*-
// $Id: TowerEnergyList.h,v 1.1 2008/07/10 20:15:23 tai Exp $
#ifndef TOWERENERGYLIST_H
#define TOWERENERGYLIST_H

#include <StDetectorId.h>

#include <vector>

namespace StSpinJet {

struct TowerEnergy {
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

typedef std::vector<TowerEnergy> TowerEnergyList;

}

#endif // TOWERENERGYLIST_H
