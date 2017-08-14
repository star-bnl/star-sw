/***************************************************************************
 *
 * $Id: StMuFmsCollection.cxx,v 1.6 2017/08/14 16:22:36 smirnovd Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description: FMS data interface to StMuFmsHit, StMuFmsCluster and StMuFmsPoint
 *
 ***************************************************************************
 *
 * $Log: StMuFmsCollection.cxx,v $
 * Revision 1.6  2017/08/14 16:22:36  smirnovd
 * Recover FMS hits using StTriggerData
 *
 * commit 6d7358f4c86a15edd0671326580d291a9843aec9
 * Date:   Tue Aug 8 23:42:41 2017 -0400
 *
 *     StMuFmsUtil: Recover FMS hits using StTriggerData
 *
 * commit 556d07cb8fd87cb62e4ac674226423671c94917d
 * Date:   Tue Aug 8 23:42:34 2017 -0400
 *
 *     StMuFmsUtil: Added func to fill StMuFmsCollection with FMS hits from StTriggerData in StMuEvent
 *
 * commit c355529c1ee401849b2b81d74df8d452886593d1
 * Date:   Tue Aug 8 23:42:19 2017 -0400
 *
 *     [Cosmetic] Changes in whitespace
 *
 * commit 67fdc1b348bebbfbfb137b726ee9c455a7d8be37
 * Date:   Mon Jun 5 12:00:24 2017 -0400
 *
 *     StMuFmsCollection::addHit() Return pointer to just added default FMS hit object
 *
 * Revision 1.5  2015/11/06 17:47:16  jdb
 * Added StMuFmsInfo.{h,cxx} as a new branch for storing event-by-event FMS paramters
 *
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
 #include "StMuDSTMaker/COMMON/StMuFmsInfo.h"

static const char rcsid[] = "$Id: StMuFmsCollection.cxx,v 1.6 2017/08/14 16:22:36 smirnovd Exp $";

ClassImp(StMuFmsCollection)

StMuFmsCollection::StMuFmsCollection() { mHits = 0; mClusters = 0; mPoints = 0; mInfo=0;}

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
  if (mInfo) {
    delete mInfo;
  }  // if
  mHits = mClusters = mPoints = mInfo = NULL;
}

void StMuFmsCollection::init() {
  mHits = new TClonesArray("StMuFmsHit", 0);
  mClusters = new TClonesArray("StMuFmsCluster", 0);
  mPoints = new TClonesArray("StMuFmsPoint", 0);
  mInfo = new TClonesArray("StMuFmsInfo", 0);
}

StMuFmsHit* StMuFmsCollection::addHit(){
  if(!mHits) init();
  int counter = mHits->GetEntriesFast();
  StMuFmsHit* newFmsHit = new ((*mHits)[counter]) StMuFmsHit();
  return newFmsHit;
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

void StMuFmsCollection::addInfo() {
  if (!mInfo) init();
  int counter = mInfo->GetEntriesFast();
  new ((*mInfo)[counter]) StMuFmsInfo;
  return;
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

StMuFmsInfo* StMuFmsCollection::getInfo() {
  if (!mInfo) return NULL;
  return static_cast<StMuFmsInfo*>(mInfo->At(0));
}


Int_t StMuFmsCollection::fmsReconstructionFlag() {
  return getInfo()->fmsReconstructionFlag();
}
void StMuFmsCollection::setFmsReconstructionFlag(Int_t v){ 
  getInfo()->setFmsReconstructionFlag(v);
}