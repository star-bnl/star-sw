#include <algorithm>

#include "StFstRawHit.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFstConsts.h"

bool rawHitPtrLessThan::operator() (const StFstRawHit *rawHit1, const StFstRawHit *rawHit2) const
{
   if ( rawHit1 && rawHit2 )
      return (rawHit1->getGeoId() < rawHit2->getGeoId());
   else
      return 0;
}


StFstRawHit::StFstRawHit() : StObject(), mChannelId(-1), mGeoId(-1), mCharge(), mChargeErr(), mMaxTimeBin(1),
   mIdTruth(0)
{
   std::fill_n(mCharge, kFstNumTimeBins, -999);
}


template<typename Container>
StFstRawHit::StFstRawHit(int channelId, int geoId,
   const Container &charges, const Container &chargeErrs,
   UChar_t maxTimeBin, UShort_t idTruth) :
   StObject(),
   mChannelId(channelId), mGeoId(geoId), mCharge(), mChargeErr(),
   mMaxTimeBin(maxTimeBin), mIdTruth(idTruth)
{
   std::copy(std::begin(charges), std::end(charges), mCharge);
   std::copy(std::begin(chargeErrs), std::end(chargeErrs), mChargeErr);
}

StFstRawHit::StFstRawHit(const StFstRawHit &rawHit)
{
    mChannelId = rawHit.mChannelId;
    mGeoId = rawHit.mGeoId;
    mSeedhitflag = rawHit.mSeedhitflag;
    for(int itb=0; itb<kFstNumTimeBins; itb++){
        mCharge[itb] = rawHit.mCharge[itb];
        mChargeErr[itb] = rawHit.mChargeErr[itb];
    }
    mMaxTimeBin = rawHit.mMaxTimeBin;
    mIdTruth = rawHit.mIdTruth;
    mDefaultTimeBin = rawHit.mDefaultTimeBin;
}

int StFstRawHit::getChannelId() const              { return mChannelId;      };
int StFstRawHit::getGeoId() const                  { return mGeoId;          };
int StFstRawHit::getSeedhitflag() const            { return mSeedhitflag;    };
unsigned char StFstRawHit::getMaxTimeBin() const   { return mMaxTimeBin;     };
unsigned char StFstRawHit::getDefaultTimeBin()     { return mDefaultTimeBin; };
unsigned short StFstRawHit::getIdTruth() const     { return mIdTruth;        };

unsigned char StFstRawHit::getDisk() const
{
   return 1 + mGeoId / ((kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor) * kFstNumWedgePerDisk);
};

unsigned char StFstRawHit::getWedge() const
{
   return 1 + mGeoId / (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
};

unsigned char StFstRawHit::getPhiStrip() const
{
   int strip = mGeoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
   return strip % kFstNumPhiSegPerWedge;
};

unsigned char StFstRawHit::getRStrip() const
{
   int strip = mGeoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
   return strip / kFstNumPhiSegPerWedge;
};

unsigned char StFstRawHit::getRdo() const
{
   return 1 + mChannelId / (kFstNumArmsPerRdo * kFstNumChanPerArm);
};

unsigned char StFstRawHit::getArm() const
{
   return (mChannelId % (kFstNumArmsPerRdo * kFstNumChanPerArm)) / kFstNumChanPerArm;
};

unsigned char StFstRawHit::getApv() const
{
   return ((mChannelId % (kFstNumArmsPerRdo * kFstNumChanPerArm)) % kFstNumChanPerArm) / kFstNumApvChannels;
};

unsigned char StFstRawHit::getSensor() const
{
   int strip = mChannelId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
   if(strip < kFstNumStripsPerInnerSensor) return strip/kFstNumStripsPerInnerSensor;
   else return strip / kFstNumStripsPerOuterSensor - 1;
};

unsigned char StFstRawHit::getChannel() const
{
   return ((mChannelId % (kFstNumArmsPerRdo * kFstNumChanPerArm)) % kFstNumChanPerArm) % kFstNumApvChannels;
};

float StFstRawHit::getCharge( int tb ) const
{
   return mCharge[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ];
};

float StFstRawHit::getChargeErr( int tb ) const
{
   return mChargeErr[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ];
};

//modifiers
void StFstRawHit::setChannelId(int rChannelId)
{
   mChannelId = rChannelId;
};

void StFstRawHit::setGeoId(int rGeoId)
{
   mGeoId = rGeoId;
};

void StFstRawHit::setSeedhitflag(int rSeedhitflag)
{
   mSeedhitflag = rSeedhitflag;
};

void StFstRawHit::setDefaultTimeBin( int tb )
{
   mDefaultTimeBin = tb;
};

void StFstRawHit::setIdTruth(unsigned short idTruth)
{
   mIdTruth = idTruth;
};

void StFstRawHit::setCharge( float charge, int tb )
{
   mCharge[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ] = charge;
};


void StFstRawHit::setChargeErr(float rChargeErr, int tb)
{
   mChargeErr[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ] = rChargeErr;
};

void StFstRawHit::setMaxTimeBin(int tb)
{
   mMaxTimeBin = ((tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb);
};


void StFstRawHit::Print(int nTimeBins) const
{
   LOG_DEBUG << " elecId=" << getChannelId() << " Charge=(" ;
   for(int i=0; i<nTimeBins; i++)
        LOG_DEBUG << getCharge(i) << " " ;
   LOG_DEBUG << ") ChargeErr=(" ;

   for(int i=0; i<nTimeBins; i++)
        LOG_DEBUG << getChargeErr(i) << " " ;
   LOG_DEBUG << ") decode0: at disk=" << (short) getDisk() << " wedge=" << (short) getWedge() << " sensor=" << (short) getSensor()
             << " rstrip=" << (short) getRStrip() << " phistrip=" << (short) getPhiStrip() << endm;
}

unsigned char StFstRawHit::mDefaultTimeBin = 2;

ClassImp(StFstRawHit);
