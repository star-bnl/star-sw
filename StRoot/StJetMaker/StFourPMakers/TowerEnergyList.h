// -*- mode: c++;-*-
// $Id: TowerEnergyList.h,v 1.2 2008/07/10 20:49:00 tai Exp $
#ifndef TOWERENERGYLIST_H
#define TOWERENERGYLIST_H

#include <StDetectorId.h>

#include <vector>

namespace StSpinJet {

struct TowerEnergy {
  int            detectorId; // 9: BEMC, 13: EEMC
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
  int            status;     // 1 is good for BEMC. 0 is good for EEMC
};

typedef std::vector<TowerEnergy> TowerEnergyList;

}

#endif // TOWERENERGYLIST_H
