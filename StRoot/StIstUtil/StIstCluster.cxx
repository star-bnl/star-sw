/***************************************************************************
*
* $Id: StIstCluster.cxx,v 1.2 2014/01/29 18:25:03 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* See header file.
****************************************************************************
*
* $Log: StIstCluster.cxx,v $
* Revision 1.2  2014/01/29 18:25:03  ypwang
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

StIstCluster::StIstCluster(int key, unsigned char ladder, unsigned char sensor, float meanRow, float meanColumn, float totCharge, float totChargeErr, unsigned char clusteringType): mMaxTimeBin(3), mNRawHits(1), mNRawHitsRPhi(1), mNRawHitsZ(1), mIdTruth(0)
{
    mKey = key;
    mLadderId = ladder;
    mSensorId = sensor;
    mMeanRow = meanRow;
    mMeanColumn = meanColumn;
    mTotCharge = totCharge;
    mTotChargeErr = totChargeErr;
    mClusteringType = clusteringType;
}

StIstCluster::~StIstCluster()
{ /*  no op  */ }

ClassImp(StIstCluster);
