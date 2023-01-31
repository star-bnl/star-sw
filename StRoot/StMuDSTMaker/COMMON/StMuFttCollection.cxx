/***************************************************************************
 *
 * $Id: StMuFttCollection.cxx
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: Fcs data interface to StMuFttRawHit, StMuFttCluster, StMuFttPoint, and StMuFcsInfo
 *
 ***************************************************************************/

#include "StMuDSTMaker/COMMON/StMuFttCollection.h"
#include "StMuDSTMaker/COMMON/StMuFttCluster.h"
#include "StMuDSTMaker/COMMON/StMuFttRawHit.h"
#include "StMuDSTMaker/COMMON/StMuFttPoint.h"

#include "St_base/StMessMgr.h"

ClassImp(StMuFttCollection)

StMuFttCollection::StMuFttCollection() { mHits = 0; mClusters = 0; mPoints = 0;  }

StMuFttCollection::~StMuFttCollection() {
    delete mHits;
    delete mClusters;
    delete mPoints;
    mHits = mClusters = mPoints = nullptr;
}

void StMuFttCollection::init() {
    mHits     = new TClonesArray("StMuFttRawHit", 0);
    mClusters = new TClonesArray("StMuFttCluster", 0);
    mPoints   = new TClonesArray("StMuFttPoint", 0);
}

StMuFttRawHit* StMuFttCollection::addRawHit(){
    if(!mHits) init();
    int counter = mHits->GetEntriesFast();
    StMuFttRawHit* newFcsHit = new ((*mHits)[counter]) StMuFttRawHit();
    return newFcsHit;
}

StMuFttCluster* StMuFttCollection::addCluster() {
    if (!mClusters) init();
    int counter = mClusters->GetEntriesFast();
    return new ((*mClusters)[counter]) StMuFttCluster;
}

StMuFttPoint* StMuFttCollection::addPoint() {
    if (!mPoints) init();
    int counter = mPoints->GetEntriesFast();
    return new ((*mPoints)[counter]) StMuFttPoint;
}

unsigned int StMuFttCollection::numberOfRawHits() const{
    if(!mHits) return 0;
    return mHits->GetEntriesFast();
}

unsigned int StMuFttCollection::numberOfClusters() const {
    if (!mClusters) return 0;
    return mClusters->GetEntriesFast();
}

unsigned int StMuFttCollection::numberOfPoints() const {
    if (!mPoints) return 0;
    return mPoints->GetEntriesFast();
}

StMuFttRawHit*  StMuFttCollection::getRawHit(int index){
    if(!mHits) return NULL;
    return (StMuFttRawHit*) mHits->At(index);
}

StMuFttCluster* StMuFttCollection::getCluster(int index) {
    if (!mClusters) return NULL;
    return static_cast<StMuFttCluster*>(mClusters->At(index));
}

StMuFttPoint* StMuFttCollection::getPoint(int index) {
    if (!mPoints) return NULL;
    return static_cast<StMuFttPoint*>(mPoints->At(index));
}