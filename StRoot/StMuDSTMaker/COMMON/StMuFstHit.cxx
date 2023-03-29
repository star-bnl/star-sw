/***************************************************************************
 *
 * StMuFstHit.cxx
 *
 * Author: tchuang 2022
 ***************************************************************************
 *
 * Description: Implementation of StMuFstHit, the StEvent hit structure
 *
 ***************************************************************************/
#include "StMuFstHit.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StFstHit.h"
#include "StThreeVector.hh"

ClassImp(StMuFstHit)

StMuFstHit::StMuFstHit() :  TObject() { /* no op */ }

StMuFstHit::~StMuFstHit() { /* no op */ }

void StMuFstHit::print(int opt) {
}

void StMuFstHit::setLocalPosition(float vR, float vPhi, float vZ)
{
   mLocalPosition[0] = vR;
   mLocalPosition[1] = vPhi;
   mLocalPosition[2] = vZ;
}

float StMuFstHit::localPosition(unsigned int i) const
{
   if (i < 3)
      return mLocalPosition[i];
   else
      return 0;
}

void StMuFstHit::set( StFstHit *hit ){
    mId = hit->id();
    mIdTruth = hit->idTruth();
    mApv = hit->getApv();
    mMaxTimeBin = hit->getMaxTimeBin();
    mMeanRStrip = hit->getMeanRStrip();
    mMeanPhiStrip = hit->getMeanPhiStrip();
    mCharge = hit->charge();
    mChargeErr = hit->getChargeErr();
    mNRawHits = hit->getNRawHits();
    mNRawHitsR = hit->getNRawHitsR();
    mNRawHitsPhi = hit->getNRawHitsPhi();
    mLocalPosition[0] = hit->localPosition(0);
    mLocalPosition[1] = hit->localPosition(1);
    mLocalPosition[2] = hit->localPosition(2);

    const StThreeVectorF &pos = hit->position();
    mXYZ = TVector3( pos.x(), pos.y(), pos.z() );

    mHardwarePosition = hit->hardwarePosition();
} // set from StEvent
