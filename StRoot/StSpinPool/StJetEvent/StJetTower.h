// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#ifndef ST_JET_TOWER_H
#define ST_JET_TOWER_H

class StMuTowerEmu;

#include "TVector3.h"

#include "StJetElement.h"

class StJetTower : public StJetElement {
public:
  StJetTower();
  StJetTower(const StMuTowerEmu* tower);

  short adc()      const { return mAdc; }
  float pedestal() const { return mPedestal; }
  float rms()      const { return mRms; }
  short status()   const { return mStatus; }

private:
  short mAdc;
  float mPedestal;
  float mRms;
  short mStatus;

  ClassDef(StJetTower, 1);
};

#endif // ST_JET_TOWER_H
