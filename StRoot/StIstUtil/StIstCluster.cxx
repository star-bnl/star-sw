/***************************************************************************
*
* $Id: StIstCluster.cxx,v 1.14 2015/08/03 14:26:03 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************/

#include "StIstConsts.h"
#include "StIstCluster.h"
#include "StRoot/St_base/StMessMgr.h"


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


/***************************************************************************
*
*
* $Log: StIstCluster.cxx,v $
* Revision 1.14  2015/08/03 14:26:03  smirnovd
* Corrected style with 'astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f'
*
* Revision 1.13  2014/09/09 08:33:28  ypwang
* minor update the data member initialization order
*
* Revision 1.12  2014/09/09 08:23:46  ypwang
* all unsgined char was updated to int type as Victor P. suggested
*
* Revision 1.11  2014/09/08 19:06:57  smirnovd
* Added Print() methods to print out properties of StIstCluster and StIstRawHit objects and their respective collections
*
* Revision 1.10  2014/03/27 22:46:47  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.9  2014/02/20 02:30:58  smirnovd
* Use constructor list to initialize vectors of pointers and arrays
*
* Revision 1.8  2014/02/20 02:30:44  smirnovd
* Simplified the destructor
*
* Revision 1.7  2014/02/15 01:16:18  ypwang
* replace the std::map() with std::vector() for StIstCluster
*
* Revision 1.6  2014/02/13 02:35:48  smirnovd
* Moved CVS log to the bottom of the file
*
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
