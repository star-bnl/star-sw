/**
 * \class StGmtStrip
 * \brief Holds data for the strip in GMT
 * 
 * Data for an individual strip in GMT (based on StFgtStrip).
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

// StRoot headers
#include "StGmtStrip.h"
#include "St_base/StMessMgr.h"

// Number of defult time bins.
// Was 2 for the FGT, RW 03/15/13
int StGmtStrip::mDefaultTimeBin = 7;  

//________________
bool gmtStripPtrLessThan::operator() (const StGmtStrip* strip1, const StGmtStrip* strip2) const {
  return ((strip1 && strip2) ? strip1->getGeoId() < strip2->getGeoId() : 0 );
}

//________________
StGmtStrip::StGmtStrip() : StObject(), mGeoId(-1),  
    mModule(-1),  mCoordNum(-1), mIsY(0), mPosition(0.0), 
    mMaxAdc(-9999), mMaxPedSubtractedAdc(-9999), 
    mMaxAdcTB(-1), mMaxPedSubtractedAdcTB(-1),
    mCharge(kInvalidChargeValue), 
    mChargeUncert(kInvalidChargeValue),
    mRdo(0), mArm(0), mApv(0), mChan(0), 
    mPed(0), mPedStdDev(0), mPedErr(0), mIsC(0) {
  // Constuctor
  for( int i = 0; i < kGmtNumTimeBins; ++i ) {
    mAdc[i] = -9999;              //not set
    mPedSubtractedAdc[i] = -9999; //not set
  }
}

//________________
StGmtStrip::~StGmtStrip() { 
  /* empty */ 
}

//________________
StGmtStrip::StGmtStrip(const StGmtStrip& h) : StObject(),                                  // copy the parent
    mGeoId( h.mGeoId ), mModule( h.mModule ), mCoordNum( h.mCoordNum ),
    mIsY( h.mIsY ), mIsC( h.mIsC ), mPosition( h.mPosition ),
    mMaxAdc(h.mMaxAdc), mMaxPedSubtractedAdc(h.mMaxPedSubtractedAdc),
    mMaxAdcTB(h.mMaxAdcTB), mMaxPedSubtractedAdcTB(h.mMaxPedSubtractedAdcTB),
    mCharge( h.mCharge ), mChargeUncert(h.mChargeUncert),
    mRdo( h.mRdo ), mArm( h.mArm ), mApv( h.mApv ), mChan( h.mChan ),
    mPed(h.mPed), mPedStdDev(h.mPedStdDev), mPedErr(h.mPedErr) {
  // Copy constructor
  for( int i = 0; i < kGmtNumTimeBins; ++i ) {
    mAdc[i] = h.mAdc[i];
    mPedSubtractedAdc[i] = h.mPedSubtractedAdc[i];
  }
}

//________________
StGmtStrip& StGmtStrip::operator=( const StGmtStrip& h) {
  // Assignment operator
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
  mCharge = h.mCharge;
  mChargeUncert = h.mChargeUncert;
  mRdo = h.mRdo;
  mArm = h.mArm;
  mApv = h.mApv;
  mChan = h.mChan;
  mPed=h.mPed;
  mPedStdDev=h.mPedStdDev;
  mPedErr=h.mPedErr;
    
  for( int i = 0; i < kGmtNumTimeBins; ++i ) {
    mAdc[i] = h.mAdc[i];
    mPedSubtractedAdc[i] = h.mPedSubtractedAdc[i];
  }
    
  return *this;
}

//________________
ostream&  operator<<(ostream& os, const StGmtStrip& v) {
    return os << Form("GmtStrip gId %3i m %3i C %3i Y %1i C %1i p %8.3f",v.getGeoId(), v.getModule(), v.getCoordNum(), v.isY(), v.isC(), v.getPosition())
              << Form(" Rdo: %2i,Arm: %2i, Apv: %2i, cha: %3i",v.getRdo(), v.getArm(), v.getApv(), v.getChannel()); 
}

//________________
void StGmtStrip::Print(Option_t *option) const {
    LOG_INFO << *this << endm;
}


