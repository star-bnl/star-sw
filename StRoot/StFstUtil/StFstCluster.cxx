/**
 * Author: Shenghui Zhang, Sep 2021
 *
 * Description:
 * See header file.
 */

#include "StFstConsts.h"
#include "StFstCluster.h"
#include "St_base/StMessMgr.h"


StFstCluster::StFstCluster(int key, int disk, int wedge, int sensor, int apv,
                           float meanRStrip, float meanPhiStrip, float totCharge, float totChargeErr,
                           int clusteringType):
   mKey(key),
   mMeanRStrip(meanRStrip),
   mMeanPhiStrip(meanPhiStrip),
   mTotCharge(totCharge),
   mTotChargeErr(totChargeErr),
   mIdTruth(0),
   mDiskId(disk),
   mWedgeId(wedge),
   mSensorId(sensor),
   mApv(apv),
   mClusteringType(clusteringType),
   mMaxTimeBin(1), mNRawHits(1), mNRawHitsR(1),
   mNRawHitsPhi(1), mRawHitVec()
{
}

StFstCluster::~StFstCluster()
{
   while (!mRawHitVec.empty()) delete mRawHitVec.back(), mRawHitVec.pop_back();
}

//accessors
vector<StFstRawHit *> &StFstCluster::getRawHitVec()     	{  return mRawHitVec;       };
const vector<StFstRawHit *> &StFstCluster::getRawHitVec() const {  return mRawHitVec;       };
int              StFstCluster::getKey() const                   {  return mKey;             };
unsigned char    StFstCluster::getDisk() const                  {  return mDiskId;          };
unsigned char    StFstCluster::getWedge() const        	        {  return mWedgeId;         };
unsigned char    StFstCluster::getSensor() const        	{  return mSensorId;        };
unsigned char    StFstCluster::getApv() const                   {  return mApv;             };
float            StFstCluster::getMeanRStrip() const       	{  return mMeanRStrip;      };
float            StFstCluster::getMeanPhiStrip() const    	{  return mMeanPhiStrip;    };
float            StFstCluster::getTotCharge() const     	{  return mTotCharge;       };
float            StFstCluster::getTotChargeErr() const  	{  return mTotChargeErr;    };
unsigned char    StFstCluster::getMaxTimeBin() const    	{  return mMaxTimeBin;      };
unsigned char    StFstCluster::getClusteringType() const 	{  return mClusteringType;  };
unsigned char    StFstCluster::getNRawHits() const      	{  return mNRawHits;        };
unsigned char    StFstCluster::getNRawHitsR() const      	{  return mNRawHitsR;       };
unsigned char    StFstCluster::getNRawHitsPhi() const     	{  return mNRawHitsPhi;     };
unsigned short   StFstCluster::getIdTruth() const       	{  return mIdTruth;         };

//modifiers
void StFstCluster::setDisk(int disk)
{
   mDiskId = disk;
};

void StFstCluster::setWedge(int wedge)
{
   mWedgeId = wedge;
};

void StFstCluster::setSensor(int sensor)
{
   mSensorId = sensor;
};

void StFstCluster::setApv(int apv)
{
   mApv = apv;
};

void StFstCluster::setMeanRStrip(float meanRStrip)
{
   mMeanRStrip = meanRStrip;
};

void StFstCluster::setMeanPhiStrip(float meanPhiStrip)
{
   mMeanPhiStrip = meanPhiStrip;
};

void StFstCluster::setTotCharge(float totCharge)
{
   mTotCharge = totCharge;
};

void StFstCluster::setTotChargeErr(float totChargeErr)
{
   mTotChargeErr = totChargeErr;
};

void StFstCluster::setMaxTimeBin(int tb)
{
   mMaxTimeBin = tb;
};

void StFstCluster::setClusteringType(int clusteringType)
{
   mClusteringType = clusteringType;
};

void StFstCluster::setNRawHits(int nRawHits)
{
   mNRawHits = nRawHits;
};

void StFstCluster::setNRawHitsR(int nRawHitsR)
{
   mNRawHitsR = nRawHitsR;
};

void StFstCluster::setNRawHitsPhi(int nRawHitsPhi)
{
   mNRawHitsPhi = nRawHitsPhi;
};

void StFstCluster::setIdTruth(unsigned short idTruth)
{
   mIdTruth = idTruth;
};


void StFstCluster::Print(Option_t *opt) const
{
   LOG_DEBUG << " totCharge=" << getTotCharge() << " totChargeErr=" << getTotChargeErr()
             << " meanPhiStrip=" << getMeanPhiStrip() << " meanRStrip= " << getMeanRStrip()
             << " at disk=" << (short) getDisk() << " wedge=" << (short) getWedge() << " sensor=" << (short) getSensor()
             << " clusterSize=" << (short) getNRawHits()
             << " clusterSizeR=" << (short) getNRawHitsR()
             << " clusterSizePhi=" << (short) getNRawHitsPhi() << endm;
}


ClassImp(StFstCluster);
