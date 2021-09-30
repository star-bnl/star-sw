/***************************************************************************
 *
 * $Id: StMuFcsCollection.cxx,v 1.6 2017/08/14 16:22:36 smirnovd Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description: Fcs data interface to StMuFcsHit, StMuFcsCluster and StMuFcsPoint
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"

#include "St_base/StMessMgr.h"

static const char rcsid[] = "$Id: StMuFcsCollection.cxx,v 1.6 2021/09/26 16:22:36 jdb Exp $";

ClassImp(StMuFcsCollection)

StMuFcsCollection::StMuFcsCollection() { mHits = 0; mClusters = 0; mPoints = 0;}

StMuFcsCollection::~StMuFcsCollection() {
  if (mHits) {
    delete mHits;
  }  // if
  if (mClusters) {
    delete mClusters;
  }  // if
  if (mPoints) {
    delete mPoints;
  }  // if
  mHits = mClusters = mPoints = NULL;
}

void StMuFcsCollection::init() {
  mHits = new TClonesArray("StMuFcsHit", 0);
  mClusters = new TClonesArray("StMuFcsCluster", 0);
  mPoints = new TClonesArray("StMuFcsPoint", 0);
}

StMuFcsHit* StMuFcsCollection::addHit(){
  if(!mHits) init();
  int counter = mHits->GetEntriesFast();
  StMuFcsHit* newFcsHit = new ((*mHits)[counter]) StMuFcsHit();
  return newFcsHit;
}

StMuFcsCluster* StMuFcsCollection::addCluster() {
  if (!mClusters) init();
  int counter = mClusters->GetEntriesFast();
  return new ((*mClusters)[counter]) StMuFcsCluster;
}

StMuFcsPoint* StMuFcsCollection::addPoint() {
  if (!mPoints) init();
  int counter = mPoints->GetEntriesFast();
  return new ((*mPoints)[counter]) StMuFcsPoint;
}

unsigned int StMuFcsCollection::numberOfHits() const{
  if(!mHits) return 0;
  return mHits->GetEntriesFast();
}

unsigned int StMuFcsCollection::numberOfClusters() const {
  if (!mClusters) return 0;
  return mClusters->GetEntriesFast();
}

unsigned int StMuFcsCollection::numberOfPoints() const {
  if (!mPoints) return 0;
  return mPoints->GetEntriesFast();
}

StMuFcsHit*  StMuFcsCollection::getHit(int index){
  if(!mHits) return NULL;
  return (StMuFcsHit*) mHits->At(index);
}

StMuFcsCluster* StMuFcsCollection::getCluster(int index) {
  if (!mClusters) return NULL;
  return static_cast<StMuFcsCluster*>(mClusters->At(index));
}

StMuFcsPoint* StMuFcsCollection::getPoint(int index) {
  if (!mPoints) return NULL;
  return static_cast<StMuFcsPoint*>(mPoints->At(index));
}