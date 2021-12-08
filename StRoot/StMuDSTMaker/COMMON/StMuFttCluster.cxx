/***************************************************************************
 *
 * $Id: StMuFttCluster.cxx
 *
 * Author: jdb, Nov 2021
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************/ 
#include "StMuFttCluster.h"
#include "StMuFttPoint.h"
#include "StMuFttRawHit.h"
#include "StEvent/StFttCluster.h"


StMuFttCluster::StMuFttCluster() : TObject(),
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


StMuFttCluster::~StMuFttCluster(){}

void StMuFttCluster::addPoint(StMuFttPoint* p) {
    mPoints.Add(p);
}

void StMuFttCluster::addRawHit(StMuFttRawHit* p) {
    mRawHits.Add(p);
}

void StMuFttCluster::addNeighbor(StMuFttCluster* neighbor) {
    int n=nNeighbors();
    for(int i=0; i<n; i++) if(mNeighbors[i]==neighbor) return; //already there, do nothing
        mNeighbors.Add(neighbor);
}

void StMuFttCluster::set( StFttCluster * clu ){
    mId          = clu->id();
    mPlane       = clu->plane();
    mQuadrant    = clu->quadrant();
    mOrientation = clu->orientation();
    mNStrips     = clu->nStrips();
    mSumAdc      = clu->sumAdc();
    mX           = clu->x();
    mSigma       = clu->sigma();
}