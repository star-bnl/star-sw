/***************************************************************************
* $Id: StFstHit.cxx$
*
* Author: Shenghui Zhang, Oct. 2021
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StFstHit.h"


StMemoryPool StFstHit::mPool(sizeof(StFstHit));


StFstHit::StFstHit(unsigned char disk, unsigned char wedge, unsigned char sensor, unsigned char apv, float charge, float chargeErr, unsigned char maxTB, float meanRStrip, float meanPhiStrip,
   unsigned char nRawHits, unsigned char nRawHitsR, unsigned char nRawHitsPhi) : StHit(),
   mApv(apv),
   mMaxTimeBin(maxTB),
   mMeanRStrip(meanRStrip),
   mMeanPhiStrip(meanPhiStrip),
   mChargeErr(chargeErr),
   mNRawHits(nRawHits),
   mNRawHitsR(nRawHitsR),
   mNRawHitsPhi(nRawHitsPhi),
   mLocalPosition(),
   mDetectorId(kFstId)
{
   StHit::setHardwarePosition(1 + (wedge - 1)*kFstNumSensorsPerWedge + sensor);
   StHit::setCharge(charge);
}


StFstHit::StFstHit(const StThreeVectorF &p, const StThreeVectorF &e, unsigned int hw, float q, unsigned char c) :
   StHit(p, e, hw, q, c),
   mApv(-1),
   mMaxTimeBin(0),
   mMeanRStrip(-1),
   mMeanPhiStrip(-1),
   mChargeErr(0),
   mNRawHits(1),
   mNRawHitsR(0),
   mNRawHitsPhi(0),
   mLocalPosition{0},
   mDetectorId(kFstId)
{
}


StDetectorId StFstHit::detector() const {return mDetectorId;}

void StFstHit::setDetectorId(StDetectorId id) {mDetectorId = id;}

void StFstHit::setLocalPosition(float vR, float vPhi, float vZ)
{
   mLocalPosition[0] = vR;
   mLocalPosition[1] = vPhi;
   mLocalPosition[2] = vZ;
}

float StFstHit::localPosition(unsigned int i) const
{
   if (i < 3)
      return mLocalPosition[i];
   else
      return 0;
}

std::ostream &operator<<(std::ostream &os, const StFstHit &hit)
{
   os << "FST Hit -I- disk: " << static_cast<int>(hit.getDisk())
      << " wedge: " << static_cast<int>(hit.getWedge())
      << " sensor: " << static_cast<int>(hit.getSensor())
      << " localPosition[0]/localPosition[1]/localPosition[2] : " << hit.localPosition(0)
      << "/" << hit.localPosition(1)
	  << "/" << hit.localPosition(2)
      << " ADC : " << hit.charge() << "+-" << hit.getChargeErr()
      << " detector: " << hit.detector()
      << endl;
   return os;
}

ClassImp(StFstHit);
/****************************************************************************
* StFstHit.cxx,v 1.0
* Revision 1.0 2021/10/04 Shenghui Zhang
* Initial version
****************************************************************************/
