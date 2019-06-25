/***************************************************************************
 *
 * $Id: StFmsCollection.cxx,v 2.9 2019/06/25 15:56:33 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsCollection.cxx,v $
 * Revision 2.9  2019/06/25 15:56:33  ullrich
 * FMS shower shape scaling in StFmsCollection (Akio)
 *
 * Revision 2.8  2015/11/05 19:01:15  ullrich
 * Improve print-out.
 *
 * Revision 2.7  2015/10/21 14:53:59  ullrich
 * Added new member and methods.
 *
 * Revision 2.6  2015/09/14 16:59:53  ullrich
 * Added StFmsPointPair collection.
 *
 * Revision 2.5  2015/09/01 21:01:47  ullrich
 * Minor changes to format of print statments and \nchange to naming of data member.
 *
 * Revision 2.4  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
 * Revision 2.3  2015/08/26 16:51:59  ullrich
 * Added print out fct and operator.
 *
 * Revision 2.2  2015/02/14 18:57:24  ullrich
 * Big upgrade after adding StFmPoint and StFmsCluster.
 *
 * Revision 2.1  2010/01/08 22:42:30  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StEvent/StFmsCollection.h"

#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsCluster.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFpsSlat.h"
#include "StEvent/StFmsPointPair.h"
#include "StarClassLibrary/StParticleTypes.hh"

static const char rcsid[] = "$Id: StFmsCollection.cxx,v 2.9 2019/06/25 15:56:33 ullrich Exp $";

StFmsCollection::StFmsCollection() :
    mFmsReconstructionFlag(0), mFpsSlatFilled(false), mFpsAssociationFilled(false), mFmsPointPairFilled(false) {
}

StFmsCollection::~StFmsCollection() {
    for(unsigned int i=0; i<mPointPairs.size(); i++) {delete (mPointPairs[i]);}
    mPointPairs.clear();
}

unsigned int StFmsCollection::numberOfHits() const {
    return mHits.size();
}

unsigned int StFmsCollection::numberOfClusters() const {
    return mClusters.size();
}

unsigned int StFmsCollection::numberOfPoints() const {
    return mPoints.size();
}

void StFmsCollection::addHit(StFmsHit* hit) {
    mHits.push_back(hit);
}

void StFmsCollection::addCluster(StFmsCluster* cluster) {
    mClusters.push_back(cluster);
}

void StFmsCollection::addPoint(StFmsPoint* point) {
    mPoints.push_back(point);
}

StSPtrVecFmsHit& StFmsCollection::hits() {
    return mHits;
}

const StSPtrVecFmsHit& StFmsCollection::hits() const {
    return mHits;
}

StSPtrVecFmsCluster& StFmsCollection::clusters() {
    return mClusters;
}

const StSPtrVecFmsCluster& StFmsCollection::clusters() const {
    return mClusters;
}

StSPtrVecFmsPoint& StFmsCollection::points() {
    return mPoints;
}

const StSPtrVecFmsPoint& StFmsCollection::points() const {
    return mPoints;
}

StSPtrVecFpsSlat& StFmsCollection::fpsSlats() {
    if(!mFpsAssociationFilled) fillFpsAssociation();
    return mFpsSlats;
}

StFpsSlat*  StFmsCollection::fps(int slatid){
    if(!mFpsAssociationFilled) fillFpsAssociation();
    return mFpsSlats[slatid];
}

void StFmsCollection::fillFpsSlat(){
    mFpsSlats.Clear();
    for(unsigned int i=0; i<kFpsMaxSlat; i++) {
        mFpsSlats.push_back(new StFpsSlat(i,0.0));
    }
    for(unsigned int i=0; i<numberOfHits(); i++) {
        if(mHits[i]->detectorId()==kFpsDetId){
            int slatid=int(mHits[i]->channel());
            if (slatid>=0 && slatid<kFpsMaxSlat){
                mFpsSlats[slatid]->setMip(mHits[i]->energy());
            }
        }
    }
    mFpsSlatFilled=true;
}

void StFmsCollection::fillFpsAssociation(){
    if (!mFpsSlatFilled) fillFpsSlat();
    for(unsigned int i=0; i<numberOfPoints(); i++) {
        for(int l=1; l<=kFpsNLayer; l++) {
            for(int c=0; c<kFpsNCandidate; c++) {
                int slatid=mPoints[i]->fpsSlatId(l,c);
                if (slatid>=0){
                    mFpsSlats[slatid]->addPoint(mPoints[i],c);
                }
            }
        }
    }
    mFpsAssociationFilled=true;
}

void StFmsCollection::fillFmsPointPair(){
    int np=numberOfPoints();
    if(np<=1) return;
    sortPointsByEnergy(); //first sort points by energy
    for(int i=0; i<np-1; i++){
	if(mPoints[i]->energy()<1.0) break; //don't make pair with E<1GeV points
        for(int j=i+1; j<np; j++){ 
	    if(mPoints[j]->energy()<1.0) break; //don't make pair with E<1GeV points
            mPointPairs.push_back(new StFmsPointPair(mPoints[i],mPoints[j]));
        }
    }
    
    mPointPairsEnergySorted = mPointPairs;
    std::sort(mPointPairsEnergySorted.begin(), mPointPairsEnergySorted.end(), [](StFmsPointPair* a, StFmsPointPair* b) {
        return b->energy() < a->energy();
    });
    
    mPointPairsETSorted = mPointPairs;
    std::sort(mPointPairsETSorted.begin(), mPointPairsETSorted.end(), [](StFmsPointPair* a, StFmsPointPair* b) {
        return b->pT() < a->pT();
    });
    
    mPointPairsPi0MassSorted = mPointPairs;
    std::sort(mPointPairsPi0MassSorted.begin(), mPointPairsPi0MassSorted.end(), [](StFmsPointPair* a, StFmsPointPair* b) {
        return fabs(b->mass() - StPionZero::instance()->mass()) > fabs(a->mass() -  StPionZero::instance()->mass());
    });
    
    mFmsPointPairFilled=true;
}

unsigned int StFmsCollection::numberOfPointPairs() {
    if(!mFmsPointPairFilled) fillFmsPointPair();
    return mPointPairs.size();
}

vector<StFmsPointPair*>& StFmsCollection::pointPairs() {
    if(!mFmsPointPairFilled) fillFmsPointPair();
    return mPointPairs;
}

vector<StFmsPointPair*>& StFmsCollection::pointPairsEnergySorted() {
    if(!mFmsPointPairFilled) fillFmsPointPair();
    return mPointPairsEnergySorted;
}

vector<StFmsPointPair*>& StFmsCollection::pointPairsETSorted() {
    if(!mFmsPointPairFilled) fillFmsPointPair();
    return mPointPairsETSorted;
}

vector<StFmsPointPair*>& StFmsCollection::pointPairsPi0MassSorted() {
    if(!mFmsPointPairFilled) fillFmsPointPair();
    return mPointPairsPi0MassSorted;
}

void StFmsCollection::sortPointsByEnergy() {
    std::sort(mPoints.begin(), mPoints.end(), [](StFmsPoint* a, StFmsPoint* b) {
        return b->energy() < a->energy();
    });
}

void StFmsCollection::sortPointsByET() {
    std::sort(mPoints.begin(), mPoints.end(), [](StFmsPoint* a, StFmsPoint* b) {
        return b->fourMomentum().perp() < a->fourMomentum().perp();
    });
}

void StFmsCollection::print(int option) {
    cout << Form("Flag=%x MergeSmallToLarge=%1d GlobalRefit=%1d Try1PhotonFit=%1d NewClusterCateg=%1d ScaleShowerShape=%1d Scale L/S=%f/%f\n",
		 mFmsReconstructionFlag,isMergeSmallToLarge(),isGlobalRefit(),isTry1PhotonFit(),
		 isNewClusterCategorization(),isScaleShowerShape(),scaleShowerShapeLarge(),scaleShowerShapeSmall());
    cout << Form("NHit=%3d NCluster=%3d NPoint=%3d\n",numberOfHits(),numberOfClusters(),numberOfPoints());
    if(option>=5) for(unsigned int i=0; i<numberOfHits(); i++)       {hits()[i]->print();}
    if(option>=4) for(unsigned int i=0; i<numberOfClusters(); i++)   {clusters()[i]->print();}
    if(option>=3) for(unsigned int i=0; i<mFpsSlats.size(); i++)     {fpsSlats()[i]->print(1);}
    if(option>=2) for(unsigned int i=0; i<numberOfPoints(); i++)     {points()[i]->print();}
    if(option>=1) for(unsigned int i=0; i<numberOfPointPairs(); i++) {pointPairs()[i]->print();}
}

