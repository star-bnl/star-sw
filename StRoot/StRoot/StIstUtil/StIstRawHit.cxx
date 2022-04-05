// $Id: StIstRawHit.cxx,v 1.20 2018/01/04 17:34:38 smirnovd Exp $

#include <algorithm>

#include "StIstRawHit.h"
#include "St_base/StMessMgr.h"

using namespace StIstConsts;


bool rawHitPtrLessThan::operator() (const StIstRawHit *rawHit1, const StIstRawHit *rawHit2) const
{
   if ( rawHit1 && rawHit2 )
      return (rawHit1->getGeoId() < rawHit2->getGeoId());
   else
      return 0;
}


StIstRawHit::StIstRawHit() : StObject(), mChannelId(-1), mGeoId(-1), mCharge(), mChargeErr(), mMaxTimeBin(3),
   mIdTruth(0)
{
   std::fill_n(mCharge, kIstNumTimeBins, -999);
}


template<typename Container>
StIstRawHit::StIstRawHit(int channelId, int geoId,
   const Container &charges, const Container &chargeErrs,
   UChar_t maxTimeBin, UShort_t idTruth) :
   StObject(),
   mChannelId(channelId), mGeoId(geoId), mCharge(), mChargeErr(),
   mMaxTimeBin(maxTimeBin), mIdTruth(idTruth)
{
   std::copy(std::begin(charges), std::end(charges), mCharge);
   std::copy(std::begin(chargeErrs), std::end(chargeErrs), mChargeErr);
}


int StIstRawHit::getChannelId() const              { return mChannelId;     };
int StIstRawHit::getGeoId() const                  { return mGeoId;      };
unsigned char StIstRawHit::getMaxTimeBin() const   { return mMaxTimeBin;    };
unsigned char StIstRawHit::getDefaultTimeBin()     { return mDefaultTimeBin;};
unsigned short StIstRawHit::getIdTruth() const     { return mIdTruth;    };

unsigned char StIstRawHit::getLadder() const
{
   return 1 + (mGeoId - 1) / (kIstNumSensorsPerLadder * kIstNumPadsPerSensor);
};

unsigned char StIstRawHit::getSensor() const
{
   return 1 + ((mGeoId - 1) % (kIstNumSensorsPerLadder * kIstNumPadsPerSensor)) / kIstNumPadsPerSensor;
};

unsigned char StIstRawHit::getRow() const
{
   int pad = ((mGeoId - 1) % (kIstNumSensorsPerLadder * kIstNumPadsPerSensor)) % kIstNumPadsPerSensor;
   return 1 + pad % kIstNumRowsPerSensor;
};

unsigned char StIstRawHit::getColumn() const
{
   int pad = ((mGeoId - 1) % (kIstNumSensorsPerLadder * kIstNumPadsPerSensor)) % kIstNumPadsPerSensor;
   return 1 + pad / kIstNumRowsPerSensor;
};

unsigned char StIstRawHit::getRdo() const
{
   return 1 + mChannelId / (kIstNumArmsPerRdo * kIstNumChanPerArm);
};

unsigned char StIstRawHit::getArm() const
{
   return (mChannelId % (kIstNumArmsPerRdo * kIstNumChanPerArm)) / kIstNumChanPerArm;
};

unsigned char StIstRawHit::getApv() const
{
   return ((mChannelId % (kIstNumArmsPerRdo * kIstNumChanPerArm)) % kIstNumChanPerArm) / kIstNumApvChannels;
};

unsigned char StIstRawHit::getChannel() const
{
   return ((mChannelId % (kIstNumArmsPerRdo * kIstNumChanPerArm)) % kIstNumChanPerArm) % kIstNumApvChannels;
};

float StIstRawHit::getCharge( int tb ) const
{
   return mCharge[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ];
};

float StIstRawHit::getChargeErr( int tb ) const
{
   return mChargeErr[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ];
};

//modifiers
void StIstRawHit::setChannelId(int rChannelId)
{
   mChannelId = rChannelId;
};

void StIstRawHit::setGeoId(int rGeoId)
{
   mGeoId = rGeoId;
};

void StIstRawHit::setDefaultTimeBin( int tb )
{
   mDefaultTimeBin = tb;
};

void StIstRawHit::setIdTruth(unsigned short idTruth)
{
   mIdTruth = idTruth;
};

void StIstRawHit::setCharge( float charge, int tb )
{
   mCharge[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ] = charge;
};


void StIstRawHit::setChargeErr(float rChargeErr, int tb)
{
   mChargeErr[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ] = rChargeErr;
};

void StIstRawHit::setMaxTimeBin(int tb)
{
   mMaxTimeBin = ((tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb);
};


void StIstRawHit::Print(int nTimeBins) const
{
   LOG_DEBUG << " elecId=" << getChannelId() << " Charge=(" ;
   for(int i=0; i<nTimeBins; i++)
        LOG_DEBUG << getCharge(i) << " " ;
   LOG_DEBUG << ") ChargeErr=(" ;

   for(int i=0; i<nTimeBins; i++)
        LOG_DEBUG << getChargeErr(i) << " " ;
   LOG_DEBUG << ") decode0: at ladder=" << (short) getLadder() << " sensor=" << (short) getSensor()
             << " column=" << (short) getColumn() << " row=" << (short) getRow() << endm;
}

unsigned char StIstRawHit::mDefaultTimeBin = 2;

ClassImp(StIstRawHit);
