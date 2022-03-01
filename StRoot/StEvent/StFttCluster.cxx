/***************************************************************************
 *
 * $Id: StFttCluster.cxx,v 1.0 2021/11/18 14:53:48 jdb Exp $
 *
 * Author: jdb, Nov 2021
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************/ 
#include "StEvent/StFttCluster.h"


StFttCluster::StFttCluster() :
mId(-1),
mOrientation(kFttUnknownOrientation),
mNStrips(0),
mSumAdc(0.0),
mX(0.0),
mSigma(0.0),
mRawHits(0),
mNeighbors(0)
{

}


StFttCluster::~StFttCluster(){}

void StFttCluster::addPoint(StFttPoint* p) {
    mPoints.push_back(p);
}

void StFttCluster::addRawHit(StFttRawHit* p) {
    mRawHits.push_back(p);
}

void StFttCluster::addNeighbor(StFttCluster* neighbor) {
    int n=nNeighbors();
    for(int i=0; i<n; i++) if(mNeighbors[i]==neighbor) return; //already there, do nothing
        mNeighbors.push_back(neighbor);
}