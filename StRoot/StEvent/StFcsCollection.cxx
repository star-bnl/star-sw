/***************************************************************************
 *
 * $Id: StFcsCollection.cxx,v 1.1.2.1 2018/08/24 15:14:36 jwebb Exp $
 *
 * Author: Akio Ogawa, 2018 Aug
 ***************************************************************************
 *
 * Description: Collection of all hits (towers), clusters and points (photons) in the FCS
 *
 ***************************************************************************
 *
 * $Log: StFcsCollection.cxx,v $
 * Revision 1.1.2.1  2018/08/24 15:14:36  jwebb
 * Forward Calorimeter System (HCAL and WCAL) added to event model.
 *
 *
 **************************************************************************/
#include "StEvent/StFcsCollection.h"

#include "StEvent/StFcsHit.h"
//#include "StEvent/StFcsCluster.h"
//#include "StEvent/StFcsPoint.h"

static const char rcsid[] = "$Id: StFcsCollection.cxx,v 1.1.2.1 2018/08/24 15:14:36 jwebb Exp $";

StFcsCollection::StFcsCollection() {}

StFcsCollection::~StFcsCollection() {}

void StFcsCollection::addHit(int wh, StFcsHit* hit){
    if(wh==0){mHitsWcal.push_back(hit);}
    else     {mHitsHcal.push_back(hit);}
}
StSPtrVecFcsHit& StFcsCollection::hits(int wh) {return (wh==0 ? mHitsWcal : mHitsHcal);}
const StSPtrVecFcsHit& StFcsCollection::hits(int wh) const {return (wh==0 ? mHitsWcal : mHitsHcal);}
unsigned int StFcsCollection::numberOfHits(int wh) const {return (wh==0 ? mHitsWcal.size() : mHitsHcal.size());}

void StFcsCollection::addHitWcal(StFcsHit* hit) {mHitsWcal.push_back(hit);}
StSPtrVecFcsHit& StFcsCollection::hitsWcal() {return mHitsWcal;}
const StSPtrVecFcsHit& StFcsCollection::hitsWcal() const {return mHitsWcal;}
unsigned int StFcsCollection::numberOfHitsWcal() const {return mHitsWcal.size();}

void StFcsCollection::addHitHcal(StFcsHit* hit) {mHitsHcal.push_back(hit);}
StSPtrVecFcsHit& StFcsCollection::hitsHcal() {return mHitsHcal;}
const StSPtrVecFcsHit& StFcsCollection::hitsHcal() const {return mHitsHcal;}
unsigned int StFcsCollection::numberOfHitsHcal() const {return mHitsHcal.size();}

//void StFcsCollection::addCluster(StFcsCluster* cluster) {mClusters.push_back(cluster);}
//StSPtrVecFcsCluster& StFcsCollection::clusters() {return mClusters;}
//const StSPtrVecFcsCluster& StFcsCollection::clusters() const {return mClusters;}
//unsigned int StFcsCollection::numberOfClusters() const {return mClusters.size();}

//void StFcsCollection::addPoint(StFcsPoint* point) {mPoints.push_back(point);}
//StSPtrVecFcsPoint& StFcsCollection::points() {return mPoints;}
//const StSPtrVecFcsPoint& StFcsCollection::points() const {return mPoints;}
//unsigned int StFcsCollection::numberOfPoints()   const {return mPoints.size();}

void StFcsCollection::print(int option) {
    cout << Form("Flag=%x\n",mFcsReconstructionFlag);
    cout << Form("NHit Wcal=%3d\n",numberOfHitsWcal());
    if(option>=4) for(unsigned int i=0; i<numberOfHitsWcal(); i++)     {hitsWcal()[i]->print();}
    cout << Form("NHit Hcal=%3d\n",numberOfHitsHcal());
    if(option>=4) for(unsigned int i=0; i<numberOfHitsHcal(); i++)     {hitsHcal()[i]->print();}
    //cout << Form("NHit=%3d NCluster=%3d NPoint=%3d\n",numberOfHits(),numberOfClusters(),numberOfPoints());
    //if(option>=3) for(unsigned int i=0; i<numberOfClusters(); i++)   {clusters()[i]->print();}
    //if(option>=2) for(unsigned int i=0; i<numberOfPoints(); i++)     {points()[i]->print();}
}

