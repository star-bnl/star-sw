/***************************************************************************
*
* $Id: StIstRawHit.cxx,v 1.1 2014/01/23 20:11:31 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstRawHit.cxx,v $
* Revision 1.1  2014/01/23 20:11:31  ypwang
* adding scripts
*
*
****************************************************************************
* StIstRawHit.cxx,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

#include "StIstRawHit.h"

bool rawHitPtrLessThan::operator() (const StIstRawHit* rawHit1, const StIstRawHit* rawHit2) const
{
    //sort by geoId in ascending order
    if( rawHit1 && rawHit2 )
	return (rawHit1->getGeoId() < rawHit2->getGeoId());
    else
	return 0;
}

StIstRawHit::~StIstRawHit()
{
    /* no op */
}

StIstRawHit::StIstRawHit() : StObject(), mChannelId(-1), mGeoId(-1), mMaxTimeBin(3), mIdTruth(0)
{
    for( unsigned char i = 0; i < kIstNumTimeBins; ++i )	{
        mCharge[i] = -999.;
	mChargeErr[i] = 0.;
    }
}

StIstRawHit::StIstRawHit(const StIstRawHit& h) : StObject(), mChannelId(h.mChannelId), mGeoId(h.mGeoId), mMaxTimeBin(h.mMaxTimeBin), mIdTruth(h.mIdTruth)
{
    for( unsigned char i = 0; i < kIstNumTimeBins; ++i )	{
        mCharge[i] = h.mCharge[i]; 
        mChargeErr[i] = h.mChargeErr[i];
    }
}

StIstRawHit& StIstRawHit::operator=( const StIstRawHit& h)
{
    mChannelId  = h.mChannelId;
    mGeoId	= h.mGeoId;
    mMaxTimeBin = h.mMaxTimeBin;
    mIdTruth    = h.mIdTruth;
    
    for( unsigned char i = 0; i < kIstNumTimeBins; ++i )	{
        mCharge[i]     = h.mCharge[i];
	mChargeErr[i]  = h.mChargeErr[i];
    }
    return *this;
}

unsigned char StIstRawHit::mDefaultTimeBin = 3;

ClassImp(StIstRawHit);
