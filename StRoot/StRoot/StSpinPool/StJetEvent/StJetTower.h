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
  friend class StUEMaker2009;

  float energy()   const { return momentum().Mag(); }
  short adc()      const { return mAdc; }
  float pedestal() const { return mPedestal; }
  float rms()      const { return mRms; }
  short status()   const { return mStatus; }

  // ID: 9=BEMC, 13=EEMC

  // EEMC sector: 1-12
  short sector() const { return (id()/60)+1; }

  // EEMC subsector: 1-5
  short subsector() const { return (id()%60)/12+1; }

  // EEMC etabin: 1-12
  short etabin() const { return (id()%60)%12+1; }

  // EEMC phibin: 1-60
  short phibin() const { return (sector()-1)*5+subsector(); }

private:
  short mAdc;
  float mPedestal;
  float mRms;
  short mStatus;

  ClassDef(StJetTower,1);
};

#endif // ST_JET_TOWER_H
