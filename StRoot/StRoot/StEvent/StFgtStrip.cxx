/***************************************************************************
 *
 * $Id: StFgtStrip.cxx,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 *
 * Author: S. Gliske, Aug. 2011
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtStrip.cxx,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#include "StFgtStrip.h"

bool stripPtrLessThan::operator() (const StFgtStrip* strip1, const StFgtStrip* strip2) const
{
    return ((strip1 && strip2) ? strip1->getGeoId() < strip2->getGeoId() : 0 );
}

StFgtStrip::~StFgtStrip() { /* no op */ }

StFgtStrip::StFgtStrip() : StObject(), mGeoId(-1), mMaxAdc(-9999), mClusterSeedType(kFgtSeedTypeNo),
mCharge(kInvalidChargeValue), mChargeUncert(kInvalidChargeValue),
mRdo(0), mArm(0), mApv(0), mChan(0), mPed(0), mPedErr(0) 
{
    for( int i = 0; i < kFgtNumTimeBins; ++i )
        mAdc[i] = -9999; //not set
}

StFgtStrip::StFgtStrip(const StFgtStrip& h) :
StObject(),                                  // copy the parent
mGeoId( h.mGeoId ),
mMaxAdc(h.mMaxAdc),
mClusterSeedType(h.mClusterSeedType),
mCharge( h.mCharge ),
mChargeUncert(h.mChargeUncert),
mRdo( h.mRdo ),
mArm( h.mArm ),
mApv( h.mApv ),
mChan( h.mChan ),
mPed(h.mPed),
mPedErr(h.mPedErr) 
{
    for( int i = 0; i < kFgtNumTimeBins; ++i )
        mAdc[i] = h.mAdc[i];
}

// note: there is no risk in assigning an StFgtStrip to equal itself.
StFgtStrip& StFgtStrip::operator=( const StFgtStrip& h) {
    mGeoId = h.mGeoId;
    mMaxAdc = h.mMaxAdc;
    mClusterSeedType = h.mClusterSeedType;
    mCharge = h.mCharge;
    mChargeUncert = h.mChargeUncert;
    mRdo = h.mRdo;
    mArm = h.mArm;
    mApv = h.mApv;
    mChan = h.mChan;
    mPed=h.mPed;
    mPedErr=h.mPedErr;
    
    for( int i = 0; i < kFgtNumTimeBins; ++i )
        mAdc[i] = h.mAdc[i];
    
    return *this;
}

int StFgtStrip::mDefaultTimeBin = 2;

ClassImp(StFgtStrip);
