//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#include "StJetTower.h"

ClassImp(StJetTower);

StJetTower::StJetTower()
  : mAdc(0)
  , mPedestal(0)
  , mRms(0)
  , mStatus(0)
{
  mPt = mEta = mPhi = 0;
}
