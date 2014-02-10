/***************************************************************************
*
* $Id: StIstCluster.cxx,v 1.5 2014/02/10 16:34:21 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************
*
* $Log: StIstCluster.cxx,v $
* Revision 1.5  2014/02/10 16:34:21  smirnovd
* Use constructor initialization list, other nit-picks
*
* Revision 1.4  2014/02/10 16:33:46  smirnovd
* Trimmed trailing spaces, expanded tabs to eight spaces
*
* Revision 1.3  2014/02/03 16:12:19  ypwang
* updating scripts
*
*
****************************************************************************
* StIstCluster.cxx,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

#include "StIstConsts.h"
#include "StIstCluster.h"


StIstCluster::StIstCluster(int key, unsigned char ladder, unsigned char sensor,
   float meanRow, float meanColumn, float totCharge, float totChargeErr, unsigned
   char clusteringType):
   mKey(key),
   mLadderId(ladder),
   mSensorId(sensor),
   mMeanRow(meanRow),
   mMeanColumn(meanColumn),
   mTotCharge(totCharge),
   mTotChargeErr(totChargeErr),
   mClusteringType(clusteringType),
   mMaxTimeBin(3), mNRawHits(1), mNRawHitsRPhi(1),
   mNRawHitsZ(1), mIdTruth(0)
{
}


StIstCluster::~StIstCluster() {}


//accessors
rawHitMap_t &StIstCluster::getRawHitMap()               {    return mRawHitMap;     };
const rawHitMap_t &StIstCluster::getRawHitMap() const   {    return mRawHitMap;     };
int              StIstCluster::getKey() const           {    return mKey;           };
unsigned char    StIstCluster::getLadder() const        {    return mLadderId;      };
unsigned char    StIstCluster::getSensor() const        {    return mSensorId;      };
float            StIstCluster::getMeanRow() const       {    return mMeanRow;       };
float            StIstCluster::getMeanColumn() const    {    return mMeanColumn;    };
float            StIstCluster::getTotCharge() const     {    return mTotCharge;     };
float            StIstCluster::getTotChargeErr() const  {    return mTotChargeErr;  };
unsigned char    StIstCluster::getMaxTimeBin() const    {    return mMaxTimeBin;    };
unsigned char    StIstCluster::getClusteringType() const {    return mClusteringType;};
unsigned char    StIstCluster::getNRawHits() const      {    return mNRawHits;      };
unsigned char    StIstCluster::getNRawHitsRPhi() const  {    return mNRawHitsRPhi;  };
unsigned char    StIstCluster::getNRawHitsZ() const     {    return mNRawHitsZ;     };
unsigned short   StIstCluster::getIdTruth() const       {    return mIdTruth;       };

//modifiers
void StIstCluster::setLadder(unsigned char ladder)
{
   mLadderId = ladder;
};

void StIstCluster::setSensor(unsigned char sensor)
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

void StIstCluster::setMaxTimeBin(unsigned char tb)
{
   mMaxTimeBin = tb;
};

void StIstCluster::setClusteringType(unsigned char clusteringType)
{
   mClusteringType = clusteringType;
};

void StIstCluster::setNRawHits(unsigned char nRawHits)
{
   mNRawHits = nRawHits;
};

void StIstCluster::setNRawHitsRPhi(unsigned char nRawHitsRPhi)
{
   mNRawHitsRPhi = nRawHitsRPhi;
};

void StIstCluster::setNRawHitsZ(unsigned char nRawHitsZ)
{
   mNRawHitsZ = nRawHitsZ;
};

void StIstCluster::setIdTruth(unsigned short idTruth)
{
   mIdTruth = idTruth;
};

ClassImp(StIstCluster);
