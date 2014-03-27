/***************************************************************************
*
* $Id: StIstRawHit.cxx,v 1.8 2014/03/27 22:46:47 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************
*
* $Log: StIstRawHit.cxx,v $
* Revision 1.8  2014/03/27 22:46:47  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.7  2014/03/13 22:10:12  smirnovd
* Move some constants from StIstUtil/StIstConsts.h to StEvent/StEnumerations.h to avoid external dependance of StEvent on StIstUtil
*
* Revision 1.6  2014/02/20 02:30:59  smirnovd
* Use constructor list to initialize vectors of pointers and arrays
*
* Revision 1.5  2014/02/20 02:29:27  smirnovd
* Remove destructor that does nothing
*
* Revision 1.4  2014/02/18 07:52:40  ypwang
* updating mDefaultTimeBin initial value to 2
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHit.cxx,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

#include "StIstRawHit.h"

using namespace StIstConsts;


bool rawHitPtrLessThan::operator() (const StIstRawHit *rawHit1, const StIstRawHit *rawHit2) const
{
   //sort by geoId in ascending order
   if ( rawHit1 && rawHit2 )
      return (rawHit1->getGeoId() < rawHit2->getGeoId());
   else
      return 0;
}


StIstRawHit::StIstRawHit() : StObject(), mChannelId(-1), mGeoId(-1), mCharge(), mChargeErr(), mMaxTimeBin(3),
   mIdTruth(0)
{
   for ( unsigned char i = 0; i < kIstNumTimeBins; ++i )	{
      mCharge[i] = -999.;
      mChargeErr[i] = 0.;
   }
}

StIstRawHit::StIstRawHit(const StIstRawHit &h) : StObject(), mChannelId(h.mChannelId), mGeoId(h.mGeoId), mCharge(),
   mChargeErr(), mMaxTimeBin(h.mMaxTimeBin), mIdTruth(h.mIdTruth)
{
   for ( unsigned char i = 0; i < kIstNumTimeBins; ++i )	{
      mCharge[i] = h.mCharge[i];
      mChargeErr[i] = h.mChargeErr[i];
   }
}

StIstRawHit &StIstRawHit::operator=( const StIstRawHit &h)
{
   mChannelId  = h.mChannelId;
   mGeoId	= h.mGeoId;
   mMaxTimeBin = h.mMaxTimeBin;
   mIdTruth    = h.mIdTruth;

   for ( unsigned char i = 0; i < kIstNumTimeBins; ++i )	{
      mCharge[i]     = h.mCharge[i];
      mChargeErr[i]  = h.mChargeErr[i];
   }

   return *this;
}

//accessories
int StIstRawHit::getChannelId() const  			{ return mChannelId;   	 };
int StIstRawHit::getGeoId() const  			{ return mGeoId;   	 };
unsigned char StIstRawHit::getMaxTimeBin() const  	{ return mMaxTimeBin;    };
unsigned char StIstRawHit::getDefaultTimeBin() const  	{ return mDefaultTimeBin;};
unsigned short StIstRawHit::getIdTruth() const  	{ return mIdTruth; 	 };

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
   short pad = ((mGeoId - 1) % (kIstNumSensorsPerLadder * kIstNumPadsPerSensor)) % kIstNumPadsPerSensor;
   return 1 + pad % kIstNumRowsPerSensor;
};

unsigned char StIstRawHit::getColumn() const
{
   short pad = ((mGeoId - 1) % (kIstNumSensorsPerLadder * kIstNumPadsPerSensor)) % kIstNumPadsPerSensor;
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

float StIstRawHit::getCharge( unsigned char tb ) const
{
   return mCharge[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ];
};

float StIstRawHit::getChargeErr( unsigned char tb ) const
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

void StIstRawHit::setDefaultTimeBin( unsigned char tb )
{
   mDefaultTimeBin = tb;
};

void StIstRawHit::setIdTruth(unsigned short idTruth)
{
   mIdTruth = idTruth;
};

void StIstRawHit::setCharge( float charge, unsigned char tb )
{
   mCharge[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ] = charge;
};

void StIstRawHit::setChargeErr(float rChargeErr, unsigned char tb)
{
   mChargeErr[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ] = rChargeErr;
};

void StIstRawHit::setMaxTimeBin(unsigned char tb)
{
   mMaxTimeBin = ((tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb);
};

unsigned char StIstRawHit::mDefaultTimeBin = 2;

ClassImp(StIstRawHit);
