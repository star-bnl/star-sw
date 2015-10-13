/*!
 * \class StPxlHit 
 * \authors S. MArgetis, J. Bouchet, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlHit.cxx,v 2.3 2015/05/13 18:05:25 ullrich Exp $
 *
 * Author: S. MArgetis, J. Bouchet, Jan 2013 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPxlHit.cxx,v $
 * Revision 2.3  2015/05/13 18:05:25  ullrich
 * New constructors for setting local hit position, proper initialization
 * of all data member, modified existing constructor, new getter and
 * setter for local hit coordinates.
 *
 * Revision 2.2  2013/03/13 08:01:45  ullrich
 * Set correct detector ID kPxlId in constructors,
 *
 * Revision 2.1  2013/03/05 14:40:40  ullrich
 * Initial Revision.
 * 
 **************************************************************************/ 
#include <algorithm>
#include "StPxlHit.h"

ClassImp(StPxlHit)
StPxlHit::~StPxlHit() { /* noop */ }

StPxlHit::StPxlHit() : StHit(),
   mSector(-1),
   mLadder(-1),
   mSensor(-1),
   mMeanRow(-1),
   mMeanColumn(-1),
   mNRawHits(0),
   mLocalPosition(),
   mDetectorId(kPxlId)
{ /* no op */ }


StPxlHit::StPxlHit(const Double_t localPos[3], UInt_t sector, UInt_t ladder, UInt_t sensor,
   const StThreeVectorF& position, const StThreeVectorF& error,
   UInt_t hwPosition, Float_t charge, UChar_t trackRefCount,
   Int_t idTruth, UShort_t quality, UShort_t id) :
   StHit(position, error, hwPosition, charge, trackRefCount, idTruth, quality, id),
   mSector(sector),
   mLadder(ladder),
   mSensor(sensor),
   mMeanRow(-1),
   mMeanColumn(-1),
   mNRawHits(0),
   mLocalPosition(),
   mDetectorId(kPxlId)
{ 
   std::copy(localPos, localPos+3, mLocalPosition);
}


/**
 * Suitable for constructing a PXL hit in a fast simulation. Note: Some data
 * members are not initialized with meaningful physical values. It is the user's
 * responsibility to set them as appropriate before the object is written in
 * StEvent. One can use the daughter class StPxlUtil/StPxlDigiHit with a more
 * automated initialization.
 */
StPxlHit::StPxlHit(const Double_t localPos[3], UInt_t sector, UInt_t ladder, UInt_t sensor,
   Int_t idTruth) :
   StHit(),
   mSector(sector),
   mLadder(ladder),
   mSensor(sensor),
   mMeanRow(-1),
   mMeanColumn(-1),
   mNRawHits(0),
   mLocalPosition(),
   mDetectorId(kPxlId)
{
   mIdTruth = idTruth;
   std::copy(localPos, localPos+3, mLocalPosition);
}


/**
 * Suitable for constructing a PXL hit from a cluster. Note: Some data members
 * are not initialized with meaningful physical values. It is the user's
 * responsibility to set them as appropriate before the object is written in
 * StEvent. One can use the daughter class StPxlUtil/StPxlDigiHit with a more
 * automated initialization.
 */
StPxlHit::StPxlHit(Float_t meanRow, Float_t meanColumn, UInt_t sector, UInt_t ladder,
   UInt_t sensor) :
   StHit(),
   mSector(sector),
   mLadder(ladder),
   mSensor(sensor),
   mMeanRow(meanRow),
   mMeanColumn(meanColumn),
   mNRawHits(0),
   mLocalPosition(),
   mDetectorId(kPxlId)
{
}


StDetectorId StPxlHit::detector() const {return mDetectorId;}

void StPxlHit::setDetectorId(StDetectorId id) {mDetectorId = id;}

Float_t StPxlHit::localPosition(UInt_t i) const
{
	if (i<3)
		return mLocalPosition[i];
	else
        return 0;
}

void StPxlHit::setLocalPosition(Float_t u, Float_t v, Float_t w)
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
