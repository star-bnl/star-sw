// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#ifndef ST_JET_TOWER_H
#define ST_JET_TOWER_H

#include "StJetElement.h"

class StJetTower : public StJetElement {
public:
  StJetTower()
    : StJetElement()
    , mAdc(0)
    , mPedestal(0)
    , mRms(0)
    , mStatus(0)
  {}

  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;

  float energy()   const { return momentum().Mag(); }
  short adc()      const { return mAdc; }
  float pedestal() const { return mPedestal; }
  float rms()      const { return mRms; }
  short status()   const { return mStatus; }

private:
  short mAdc;
  float mPedestal;
  float mRms;
  short mStatus;

  ClassDef(StJetTower,1);
};

#endif // ST_JET_TOWER_H
