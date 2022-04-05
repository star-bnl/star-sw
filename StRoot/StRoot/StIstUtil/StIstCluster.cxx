// $Id: StIstCluster.cxx,v 1.16 2018/01/04 17:34:38 smirnovd Exp $

/**
 * Author: Yaping Wang, March 2013
 *
 * Description:
 * See header file.
 */

#include "StIstConsts.h"
#include "StIstCluster.h"
#include "St_base/StMessMgr.h"


StIstCluster::StIstCluster(int key, int ladder, int sensor,
                           float meanRow, float meanColumn, float totCharge, float totChargeErr,
                           int clusteringType):
   mKey(key),
   mMeanRow(meanRow),
   mMeanColumn(meanColumn),
   mTotCharge(totCharge),
   mTotChargeErr(totChargeErr),
   mIdTruth(0),
   mLadderId(ladder),
   mSensorId(sensor),
   mClusteringType(clusteringType),
   mMaxTimeBin(3), mNRawHits(1), mNRawHitsRPhi(1),
   mNRawHitsZ(1), mRawHitVec()
{
}

StIstCluster::~StIstCluster()
{
   while (!mRawHitVec.empty()) delete mRawHitVec.back(), mRawHitVec.pop_back();
}

//accessors
vector<StIstRawHit *> &StIstCluster::getRawHitVec()     	{    return mRawHitVec;     };
const vector<StIstRawHit *> &StIstCluster::getRawHitVec() const {    return mRawHitVec;     };
int              StIstCluster::getKey() const           	{    return mKey;           };
unsigned char    StIstCluster::getLadder() const        	{    return mLadderId;      };
unsigned char    StIstCluster::getSensor() const        	{    return mSensorId;      };
float            StIstCluster::getMeanRow() const       	{    return mMeanRow;       };
float            StIstCluster::getMeanColumn() const    	{    return mMeanColumn;    };
float            StIstCluster::getTotCharge() const     	{    return mTotCharge;     };
float            StIstCluster::getTotChargeErr() const  	{    return mTotChargeErr;  };
unsigned char    StIstCluster::getMaxTimeBin() const    	{    return mMaxTimeBin;    };
unsigned char    StIstCluster::getClusteringType() const 	{    return mClusteringType;};
unsigned char    StIstCluster::getNRawHits() const      	{    return mNRawHits;      };
unsigned char    StIstCluster::getNRawHitsRPhi() const  	{    return mNRawHitsRPhi;  };
unsigned char    StIstCluster::getNRawHitsZ() const     	{    return mNRawHitsZ;     };
unsigned short   StIstCluster::getIdTruth() const       	{    return mIdTruth;       };

//modifiers
void StIstCluster::setLadder(int ladder)
{
   mLadderId = ladder;
};

void StIstCluster::setSensor(int sensor)
{
   mSensorId = sensor;
};

void StIstCluster::setMeanRow(float meanRow)
{
   mMeanRow = meanRow;
};

void StIstCluster::setMeanColumn(float meanColumn)
{
   mMeanColumn = meanColumn;
};

void StIstCluster::setTotCharge(float totCharge)
{
   mTotCharge = totCharge;
};

void StIstCluster::setTotChargeErr(float totChargeErr)
{
   mTotChargeErr = totChargeErr;
};

void StIstCluster::setMaxTimeBin(int tb)
{
   mMaxTimeBin = tb;
};

void StIstCluster::setClusteringType(int clusteringType)
{
   mClusteringType = clusteringType;
};

void StIstCluster::setNRawHits(int nRawHits)
{
   mNRawHits = nRawHits;
};

void StIstCluster::setNRawHitsRPhi(int nRawHitsRPhi)
{
   mNRawHitsRPhi = nRawHitsRPhi;
};

void StIstCluster::setNRawHitsZ(int nRawHitsZ)
{
   mNRawHitsZ = nRawHitsZ;
};

void StIstCluster::setIdTruth(unsigned short idTruth)
{
   mIdTruth = idTruth;
};


void StIstCluster::Print(Option_t *opt) const
{
   LOG_DEBUG << " totCharge=" << getTotCharge() << " totChargeErr=" << getTotChargeErr()
             << " meanColumn=" << getMeanColumn() << " meanRow= " << getMeanRow()
             << " at ladder=" << (short) getLadder() << " sensor=" << (short) getSensor()
             << " clusterSize=" << (short) getNRawHits()
             << " clusterSize(Z)=" << (short) getNRawHitsZ()
             << " clusterSize(R-Phi)=" << (short) getNRawHitsRPhi() << endm;
}


ClassImp(StIstCluster);
