/***************************************************************************
 *
 * StMuFstRawHit.cxx
 *
 * Author: tchuang 2022
 ***************************************************************************
 *
 * Description: Implementation of StMuFstRawHit, the StEvent hit structure
 *
 ***************************************************************************/
#include "StMuFstRawHit.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEnumerations.h"
#include "StRoot/StFstUtil/StFstRawHit.h"
#include "StThreeVector.hh"

ClassImp(StMuFstRawHit)

StMuFstRawHit::StMuFstRawHit() :  TObject() { /* no op */ }

StMuFstRawHit::~StMuFstRawHit() { /* no op */ }

int StMuFstRawHit::getChannelId() const              { return mChannelId;      };
int StMuFstRawHit::getGeoId() const                  { return mGeoId;          };
int StMuFstRawHit::getSeedhitflag() const            { return mSeedhitflag;    };
unsigned char StMuFstRawHit::getMaxTimeBin() const   { return mMaxTimeBin;     };
unsigned char StMuFstRawHit::getDefaultTimeBin()     { return mDefaultTimeBin; };
unsigned short StMuFstRawHit::getIdTruth() const     { return mIdTruth;        };

unsigned char StMuFstRawHit::getDisk() const
{
    return 1 + mGeoId / ((kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor) * kFstNumWedgePerDisk);
};

unsigned char StMuFstRawHit::getWedge() const
{
    return 1 + mGeoId / (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
};

unsigned char StMuFstRawHit::getPhiStrip() const
{
    int strip = mGeoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
    return strip % kFstNumPhiSegPerWedge;
};

unsigned char StMuFstRawHit::getRStrip() const
{
    int strip = mGeoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
    return strip / kFstNumPhiSegPerWedge;
};

unsigned char StMuFstRawHit::getRdo() const
{
    return 1 + mChannelId / (kFstNumArmsPerRdo * kFstNumChanPerArm);
};

unsigned char StMuFstRawHit::getArm() const
{
    return (mChannelId % (kFstNumArmsPerRdo * kFstNumChanPerArm)) / kFstNumChanPerArm;
};

unsigned char StMuFstRawHit::getApv() const
{
    return ((mChannelId % (kFstNumArmsPerRdo * kFstNumChanPerArm)) % kFstNumChanPerArm) / kFstNumApvChannels;
};

unsigned char StMuFstRawHit::getSensor() const
{
    int strip = mChannelId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);
    if(strip < kFstNumStripsPerInnerSensor) return strip/kFstNumStripsPerInnerSensor;
    else return strip / kFstNumStripsPerOuterSensor - 1;
};

unsigned char StMuFstRawHit::getChannel() const
{
    return ((mChannelId % (kFstNumArmsPerRdo * kFstNumChanPerArm)) % kFstNumChanPerArm) % kFstNumApvChannels;
};

float StMuFstRawHit::getCharge( int tb ) const
{
    return mCharge[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ];
};

float StMuFstRawHit::getChargeErr( int tb ) const
{
    return mChargeErr[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ];
};

//modifiers
void StMuFstRawHit::setChannelId(int rChannelId)
{
    mChannelId = rChannelId;
};

void StMuFstRawHit::setGeoId(int rGeoId)
{
    mGeoId = rGeoId;
};

void StMuFstRawHit::setSeedhitflag(int rSeedhitflag)
{
    mSeedhitflag = rSeedhitflag;
};

void StMuFstRawHit::setDefaultTimeBin( int tb )
{
    mDefaultTimeBin = tb;
};

void StMuFstRawHit::setIdTruth(unsigned short idTruth)
{
    mIdTruth = idTruth;
};

void StMuFstRawHit::setCharge( float charge, int tb )
{
    mCharge[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ] = charge;
};


void StMuFstRawHit::setChargeErr(float rChargeErr, int tb)
{
    mChargeErr[ (tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb ] = rChargeErr;
};

void StMuFstRawHit::setMaxTimeBin(int tb)
{
    mMaxTimeBin = ((tb < 0 || tb >= kFstNumTimeBins) ? mDefaultTimeBin : tb);
};


void StMuFstRawHit::print(int nTimeBins)
{
    cout << " elecId=" << getChannelId() << " Charge=(" ;
    for(int i=0; i<nTimeBins; i++)
        cout << getCharge(i) << " " ;
    cout << ") ChargeErr=(" ;

    for(int i=0; i<nTimeBins; i++)
        cout << getChargeErr(i) << " " ;
    cout << ") decode0: at disk=" << (short) getDisk() << " wedge=" << (short) getWedge() << " sensor=" << (short) getSensor()
        << " rstrip=" << (short) getRStrip() << " phistrip=" << (short) getPhiStrip() << endl;
}

unsigned char StMuFstRawHit::mDefaultTimeBin = 2;

void StMuFstRawHit::set( StFstRawHit *hit ){

    mChannelId = hit->getChannelId();
    mGeoId = hit->getGeoId();
    mSeedhitflag = hit->getSeedhitflag();
    for(int ibin=0; ibin<kFstNumTimeBins; ibin++){
        mCharge[ibin] = hit->getCharge(ibin);
        mChargeErr[ibin] = hit->getChargeErr(ibin);
    }
    mMaxTimeBin = hit->getMaxTimeBin();
    mIdTruth = hit->getIdTruth();

    mDefaultTimeBin = hit->getDefaultTimeBin();
} // set from StEvent
