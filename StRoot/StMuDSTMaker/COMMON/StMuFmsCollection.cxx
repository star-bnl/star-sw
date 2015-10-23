/***************************************************************************
 *
 * $Id: StMuFmsCollection.cxx,v 1.4 2015/10/23 19:22:49 jdb Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description: FMS data interface to StMuFmsHit, StMuFmsCluster and StMuFmsPoint
 *
 ***************************************************************************
 *
 * $Log: StMuFmsCollection.cxx,v $
 * Revision 1.4  2015/10/23 19:22:49  jdb
 * akio added mFmsReconstructionFlag and related getters and setters. pushed version number of StMuFmsCollection. Corresponding changes for reconstruction flag in StMuFmsUtil.cxx
 *
 * Revision 1.3  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 * Revision 1.2  2012/11/26 23:14:33  fisyak
 * Replace GetEntries() by GetEntriesFast(), fix print outs
 *
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#include "StMuDSTMaker/COMMON/StMuFmsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFmsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFmsHit.h"
#include "StMuDSTMaker/COMMON/StMuFmsPoint.h"

static const char rcsid[] = "$Id: StMuFmsCollection.cxx,v 1.4 2015/10/23 19:22:49 jdb Exp $";

ClassImp(StMuFmsCollection)

StMuFmsCollection::StMuFmsCollection() { mHits = 0; mClusters = 0; mPoints = 0; mFmsReconstructionFlag=0;}

StMuFmsCollection::~StMuFmsCollection() {
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

void StMuFmsCollection::init() {
  mHits = new TClonesArray("StMuFmsHit", 0);
  mClusters = new TClonesArray("StMuFmsCluster", 0);
  mPoints = new TClonesArray("StMuFmsPoint", 0);
}

void StMuFmsCollection::addHit(){
  if(!mHits) init();
  int counter = mHits->GetEntriesFast();
  new ((*mHits)[counter]) StMuFmsHit();
  return;
}

void StMuFmsCollection::addCluster() {
  if (!mClusters) init();
  int counter = mClusters->GetEntriesFast();
  new ((*mClusters)[counter]) StMuFmsCluster;
}

StMuFmsPoint* StMuFmsCollection::addPoint() {
  if (!mPoints) init();
  int counter = mPoints->GetEntriesFast();
  return new ((*mPoints)[counter]) StMuFmsPoint;
}

unsigned int StMuFmsCollection::numberOfHits() const{
  if(!mHits) return 0;
  return mHits->GetEntriesFast();
}

unsigned int StMuFmsCollection::numberOfClusters() const {
  if (!mClusters) return 0;
  return mClusters->GetEntriesFast();
}

unsigned int StMuFmsCollection::numberOfPoints() const {
  if (!mPoints) return 0;
  return mPoints->GetEntriesFast();
}

StMuFmsHit*  StMuFmsCollection::getHit(int hitId){
  if(!mHits) return NULL;
  return (StMuFmsHit*) mHits->At(hitId);
}

StMuFmsCluster* StMuFmsCollection::getCluster(int index) {
  if (!mClusters) return NULL;
  return static_cast<StMuFmsCluster*>(mClusters->At(index));
}

StMuFmsPoint* StMuFmsCollection::getPoint(int index) {
  if (!mPoints) return NULL;
  return static_cast<StMuFmsPoint*>(mPoints->At(index));
}
