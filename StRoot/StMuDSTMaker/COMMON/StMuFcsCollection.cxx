/***************************************************************************
 *
 * $Id: StMuFcsCollection.cxx,v 1.0 2021/11/17 16:22:36 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 ***************************************************************************
 *
 * Description: Fcs data interface to StMuFcsHit, StMuFcsCluster, StMuFcsPoint, and StMuFcsInfo
 *
 ***************************************************************************/

#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"
#include "StMuDSTMaker/COMMON/StMuFcsInfo.h"

#include "St_base/StMessMgr.h"

static const char rcsid[] = "$Id: StMuFcsCollection.cxx,v 1.6 2021/09/26 16:22:36 jdb Exp $";

ClassImp(StMuFcsCollection)

StMuFcsCollection::StMuFcsCollection() { mHits = 0; mClusters = 0; mPoints = 0; mInfo = 0; }

StMuFcsCollection::~StMuFcsCollection() {
    delete mHits;
    delete mClusters;
    delete mPoints;
    delete mInfo;
    mHits = mClusters = mPoints = mInfo = nullptr;
}

void StMuFcsCollection::init() {
    mHits     = new TClonesArray("StMuFcsHit", 0);
    mClusters = new TClonesArray("StMuFcsCluster", 0);
    mPoints   = new TClonesArray("StMuFcsPoint", 0);
    mInfo     = new TClonesArray("StMuFcsInfo", 0);
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

StMuFcsInfo* StMuFcsCollection::addInfo() {
    if (!mInfo) init();
    int counter = mInfo->GetEntriesFast();
    return new ((*mInfo)[counter]) StMuFcsInfo;
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

StMuFcsInfo* StMuFcsCollection::getInfo() {
    if (!mInfo) return NULL;
    return static_cast<StMuFcsInfo*>(mInfo->At(0));
}

Int_t StMuFcsCollection::fcsReconstructionFlag() {
  return getInfo()->fcsReconstructionFlag();
}
void StMuFcsCollection::setFcsReconstructionFlag(Int_t v){ 
  getInfo()->setFcsReconstructionFlag(v);
}

unsigned int StMuFcsCollection::indexOfFirstHit( unsigned int idet ){
    return getInfo()->hitIndex(idet);
}

unsigned int StMuFcsCollection::indexOfFirstCluster( unsigned int idet ){
    return getInfo()->clusterIndex(idet);
}

unsigned int StMuFcsCollection::indexOfFirstPoint( unsigned int idet ){
    return getInfo()->pointIndex(idet);
}

unsigned int StMuFcsCollection::numberOfHits( unsigned int idet ){
    return (getInfo()->hitIndex(idet+1) - getInfo()->hitIndex(idet));
}

unsigned int StMuFcsCollection::numberOfClusters( unsigned int idet ){
    return (getInfo()->clusterIndex(idet+1) - getInfo()->clusterIndex(idet));
}

unsigned int StMuFcsCollection::numberOfPoints( unsigned int idet ){
    return (getInfo()->pointIndex(idet+1) - getInfo()->pointIndex(idet));
}