/***************************************************************************
 *
 * $Id: StFttCollection.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: Akio Ogawa, 2018 Aug
 ***************************************************************************
 *
 * Description: Collection of all hits (towers), clusters and 
 *              points (photons) in the Ftt
 *
 ***************************************************************************
 *
 * $Log: StFttCollection.cxx,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEvent/StFttCollection.h"

#include "StEvent/StFttRawHit.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttPoint.h"

ClassImp(StFttCollection)

StFttCollection::StFttCollection() {/* no operation*/}

StFttCollection::~StFttCollection() {/* no operation */}

void StFttCollection::addRawHit(StFttRawHit* hit){mRawHits.push_back(hit);}
StSPtrVecFttRawHit& StFttCollection::rawHits() {return mRawHits;}
const StSPtrVecFttRawHit& StFttCollection::rawHits() const {return mRawHits;}
unsigned int StFttCollection::numberOfRawHits() const { return mRawHits.size(); }

void StFttCollection::addCluster(StFttCluster* cluster){mClusters.push_back(cluster);}
StSPtrVecFttCluster& StFttCollection::clusters() {return mClusters;}
const StSPtrVecFttCluster& StFttCollection::clusters() const {return mClusters;}
unsigned int StFttCollection::numberOfClusters() const { return mClusters.size(); }

void StFttCollection::addPoint(StFttPoint* point){mPoints.push_back(point);}
StSPtrVecFttPoint& StFttCollection::points() {return mPoints;}
const StSPtrVecFttPoint& StFttCollection::points() const {return mPoints;}
unsigned int StFttCollection::numberOfPoints() const { return mPoints.size(); }

void StFttCollection::print(int option) {
    cout << "  *** Print Ftt collection *** " << endl;
}

