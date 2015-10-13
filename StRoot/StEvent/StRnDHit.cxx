/*!
 * \class StRnDHit 
 * \author Mike Miller and Andrew Rose, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StRnDHit.cxx,v 2.1 2006/01/19 21:42:06 ullrich Exp $
 *
 * Author: Mike Miller and Andrew Rose, Jan 2006
 ***************************************************************************
 *
 * Description:  This is an experimental class and not final yet
 *
 ***************************************************************************
 *
 * $Log: StRnDHit.cxx,v $
 * Revision 2.1  2006/01/19 21:42:06  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StRnDHit.h"

ClassImp(StRnDHit)
StRnDHit::~StRnDHit() { /* noop */ }

StRnDHit::StRnDHit()
{
    mLayer = mLadder = mWafer = -1;
    mExtraByte1 = mExtraByte0 = 0;
    mKey = mVolumeId = -1;
    mDouble0 = mDouble1 = mDouble2 = mDouble3 = mDouble4 = 0.;
    mDetectorId = kUnknownId;
}

StRnDHit::StRnDHit(const StThreeVectorF& p,
                   const StThreeVectorF& e,
                   unsigned int hw, float q, unsigned char c,
	         Int_t idTruth,  unsigned short quality,
	         unsigned short id, StDetectorId dId)
    : StHit(p, e, hw, q, c, idTruth, quality, id)
{ 
    mLayer = mLadder = mWafer = -1;
    mExtraByte0 = mExtraByte1 = 0;
    
    mKey = mVolumeId = -1;
    
    mDouble0 = mDouble1 = mDouble2 = mDouble3 = mDouble4 = 0.;
    mDetectorId = dId;
}

StDetectorId StRnDHit::detector() const {return mDetectorId;}

void StRnDHit::setDetectorId(StDetectorId id) {mDetectorId = id;}

ostream& operator<<(ostream& os, const StRnDHit& hit)
{
    return os << "HFT Hit -I- \tLayer:"<<hit.mLayer<<" ladder: "<<hit.mLadder
	    << " wafer: "<< hit.mWafer<<"\n\t\t"<<((StHit)hit)
	    <<" \n\t\tExtraByte0: "<<hit.mExtraByte0<<" ExtraByte1: "<<hit.mExtraByte1
	    <<"\n\t\tDoubles: "<< hit.mDouble0<< " "<<hit.mDouble1<<" "<<hit.mDouble2
	    << hit.mDouble3 << " " << hit.mDouble4<<endl;
}
