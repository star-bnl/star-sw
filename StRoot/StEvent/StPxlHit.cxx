/*!
 * \class StPxlHit 
 * \authors S. MArgetis, J. Bouchet, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHit.cxx,v 2.1 2013/03/05 14:40:40 ullrich Exp $
 *
 * Author: S. MArgetis, J. Bouchet, Jan 2013 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHit.cxx,v $
 * Revision 2.1  2013/03/05 14:40:40  ullrich
 * Initial Revision.
 * 
 **************************************************************************/ 

#include "StPxlHit.h"

ClassImp(StPxlHit)

StMemoryPool StPxlHit::mPool(sizeof(StPxlHit));

StPxlHit::~StPxlHit() { /* noop */ }

StPxlHit::StPxlHit()
{
    mSector = mLadder = mSensor = -1;
    mDetectorId = kUnknownId;
}

StPxlHit::StPxlHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c)
: StHit(p, e, hw, q, c)
{ 
    mSector = mLadder = mSensor = -1;
    mDetectorId = kUnknownId;
}

StDetectorId StPxlHit::detector() const {return mDetectorId;}

void StPxlHit::setDetectorId(StDetectorId id) {mDetectorId = id;}

float StPxlHit::localPosition(unsigned int i) const
{
	if (i<3)
		return mLocalPosition[i];
	else
        return 0;
}

void StPxlHit::setLocalPosition(float u, float v, float w)
{
	mLocalPosition[0] = u;	
	mLocalPosition[1] = v;
	mLocalPosition[2] = w;
}

ostream& operator<<(ostream& os, const StPxlHit& hit)
{
    os << "HFT Hit -I- \tSector:"<< static_cast<int>(hit.sector()) 
       << " ladder: "<< static_cast<int>(hit.ladder())
       << " sensor: "<< static_cast<int>(hit.sensor()) 
       << " localPosition[0]/localPosition[1]/localPosition[2] : " << hit.localPosition(0)
       << "/" << hit.localPosition(1) 
       << "/" << hit.localPosition(2)
       << " kDetectorId : " << hit.detector() 
       << endl;
    return os;
}
