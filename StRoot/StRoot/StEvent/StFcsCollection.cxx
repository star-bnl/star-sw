/***************************************************************************
 *
 * $Id: StFcsCollection.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: Akio Ogawa, 2018 Aug
 ***************************************************************************
 *
 * Description: Collection of all hits (towers), clusters and 
 *              points (photons) in the FCS
 *
 ***************************************************************************
 *
 * $Log: StFcsCollection.cxx,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEvent/StFcsCollection.h"

#include "StEvent/StFcsHit.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsPoint.h"

static const char rcsid[] = "$Id: StFcsCollection.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $";

ClassImp(StFcsCollection)

StFcsCollection::StFcsCollection() {/* no operation*/}

StFcsCollection::~StFcsCollection() {/* no operation */}

void StFcsCollection::addHit(unsigned int det, StFcsHit* hit){mHits[det%(kFcsNDet+1)].push_back(hit);}
StSPtrVecFcsHit& StFcsCollection::hits(unsigned int det) {return mHits[det%(kFcsNDet+1)];}
const StSPtrVecFcsHit& StFcsCollection::hits(unsigned int det) const {return mHits[det%(kFcsNDet+1)];}
unsigned int StFcsCollection::numberOfHits(unsigned int det) const { return mHits[det%(kFcsNDet+1)].size(); }

void StFcsCollection::addCluster(unsigned int det, StFcsCluster* cluster){mClusters[det%kFcsNDet].push_back(cluster);}
StSPtrVecFcsCluster& StFcsCollection::clusters(unsigned int det) {return mClusters[det%kFcsNDet];}
const StSPtrVecFcsCluster& StFcsCollection::clusters(unsigned int det) const {return mClusters[det%kFcsNDet];}
unsigned int StFcsCollection::numberOfClusters(unsigned int det) const { return mClusters[det%kFcsNDet].size(); }

void StFcsCollection::addPoint(unsigned int det, StFcsPoint* point){mPoints[det%kFcsNDet].push_back(point);}
StSPtrVecFcsPoint& StFcsCollection::points(unsigned int det) {return mPoints[det%kFcsNDet];}
const StSPtrVecFcsPoint& StFcsCollection::points(unsigned int det) const {return mPoints[det%kFcsNDet];}
unsigned int StFcsCollection::numberOfPoints(unsigned int det) const { return mPoints[det%kFcsNDet].size(); }

/*
void StFcsCollection::addCluster(int det, StFcsCluster* cluster){
    if(det==0){mClustersEcal.push_back(cluster);}
    else     {mClustersHcal.push_back(cluster);}
}
StSPtrVecFcsCluster& StFcsCollection::clusters(int det) {return (det==0 ? mClustersEcal : mClustersHcal);}
const StSPtrVecFcsCluster& StFcsCollection::clusters(int det) const {return (det==0 ? mClustersEcal : mClustersHcal);}
unsigned int StFcsCollection::numberOfClusters(int det) const {return (det==0 ? mClustersEcal.size() : mClustersHcal.size());}

void StFcsCollection::addPoint(int det, StFcsPoint* point){
    if(det==0){mPointsEcal.push_back(point);}
    else     {mPointsHcal.push_back(point);}
}
StSPtrVecFcsPoint& StFcsCollection::points(int det) {return (det==0 ? mPointsEcal : mPointsHcal);}
const StSPtrVecFcsPoint& StFcsCollection::points(int det) const {return (det==0 ? mPointsEcal : mPointsHcal);}
unsigned int StFcsCollection::numberOfPoints(int det) const {return (det==0 ? mPointsEcal.size() : mPointsHcal.size());}
*/

void StFcsCollection::print(int option) {
    cout << Form("  *** Print FCS collection ***  RecFlag=%d",mFcsReconstructionFlag) << endl;
    for(unsigned int det=0; det<kFcsNDet+1; det++){ 
	cout << Form("  *** FCS Det=%1d *** NHit=%3d NCluster=%3d NPoint=%3d",
			 det,numberOfHits(det),numberOfClusters(det),numberOfPoints(det)) << endl;
	if(option>=3) for(unsigned int i=0; i<numberOfHits(det); i++)     {hits(det)[i]->print();}
	if(det==kFcsNDet) continue;
	if(option>=2) for(unsigned int i=0; i<numberOfClusters(det); i++) {clusters(det)[i]->print();}
	if(option>=1) for(unsigned int i=0; i<numberOfPoints(det); i++)   {points(det)[i]->print();}
    }
}

