/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtStrip
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************/

#include "StGmtStrip.h"

bool gmtStripPtrLessThan::operator() (const StGmtStrip* strip1, const StGmtStrip* strip2) const
{
    return ((strip1 && strip2) ? strip1->getGeoId() < strip2->getGeoId() : 0 );
}

StGmtStrip::~StGmtStrip() { /* no op */ }

StGmtStrip::StGmtStrip() : StObject(), mGeoId(-1),  
mModule(-1),  mCoordNum(-1), mIsY(0), mPosition(0.0), mMaxAdc(-9999),
mMaxPedSubtractedAdc(-9999), mMaxAdcTB(-1), mMaxPedSubtractedAdcTB(-1),
mCharge(kInvalidChargeValue), mChargeUncert(kInvalidChargeValue),
			   mRdo(0), mArm(0), mApv(0), mChan(0), mPed(0), mPedErr(0), mIsC(0) 
{
    for( int i = 0; i < kGmtNumTimeBins; ++i )
    {
        mAdc[i] = -9999; //not set
        mPedSubtractedAdc[i] = -9999; //not set
    }
}

StGmtStrip::StGmtStrip(const StGmtStrip& h) :
StObject(),                                  // copy the parent
mGeoId( h.mGeoId ),
mModule( h.mModule ),
mCoordNum( h.mCoordNum ),
mIsY( h.mIsY ),
mIsC( h.mIsC ),
mPosition( h.mPosition ),
mMaxAdc(h.mMaxAdc),
mMaxPedSubtractedAdc(h.mMaxPedSubtractedAdc),
mMaxAdcTB(h.mMaxAdcTB),
mMaxPedSubtractedAdcTB(h.mMaxPedSubtractedAdcTB),
// mClusterSeedType(h.mClusterSeedType),
mCharge( h.mCharge ),
mChargeUncert(h.mChargeUncert),
mRdo( h.mRdo ),
mArm( h.mArm ),
mApv( h.mApv ),
mChan( h.mChan ),
mPed(h.mPed),
mPedStdDev(h.mPedStdDev),
mPedErr(h.mPedErr) 
{
    for( int i = 0; i < kGmtNumTimeBins; ++i )
    {
        mAdc[i] = h.mAdc[i];
        mPedSubtractedAdc[i] = h.mPedSubtractedAdc[i];
    }
}

// note: there is no risk in assigning an StGmtStrip to equal itself.
StGmtStrip& StGmtStrip::operator=( const StGmtStrip& h) {
    mGeoId = h.mGeoId;
    mModule = h.mModule;
    mCoordNum = h.mCoordNum;
    mIsY = h.mIsY;
   mIsC = h.mIsC;
    mPosition = h.mPosition;
    mMaxAdc = h.mMaxAdc;
    mMaxPedSubtractedAdc = h.mMaxPedSubtractedAdc;
    mMaxAdcTB = h.mMaxAdcTB;
    mMaxPedSubtractedAdcTB = h.mMaxPedSubtractedAdcTB;
//     mClusterSeedType = h.mClusterSeedType;
    mCharge = h.mCharge;
    mChargeUncert = h.mChargeUncert;
    mRdo = h.mRdo;
    mArm = h.mArm;
    mApv = h.mApv;
    mChan = h.mChan;
    mPed=h.mPed;
    mPedStdDev=h.mPedStdDev;
    mPedErr=h.mPedErr;
    
    for( int i = 0; i < kGmtNumTimeBins; ++i )
    {
        mAdc[i] = h.mAdc[i];
        mPedSubtractedAdc[i] = h.mPedSubtractedAdc[i];
    }
    
    return *this;
}

int StGmtStrip::mDefaultTimeBin = 7;  // was 2 for the FGT, RW 03/15/13

ClassImp(StGmtStrip)
