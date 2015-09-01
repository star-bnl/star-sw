/***************************************************************************
 *
 * $Id: StFmsCollection.cxx,v 2.5 2015/09/01 21:01:47 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsCollection.cxx,v $
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

static const char rcsid[] = "$Id: StFmsCollection.cxx,v 2.5 2015/09/01 21:01:47 ullrich Exp $";

StFmsCollection::StFmsCollection() :
mFpsSlatFilled(false), mFpsAssociationFilled(false) { /* no op */ }

StFmsCollection::~StFmsCollection() { /* no op */ }

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
        if(mHits[i]->detectorId()==kFpsDetId
           || mHits[i]->detectorId()==15  //hack for dealing with bugged old detid=15
           ) {
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

void StFmsCollection::sortPointsByEnergy(){
    std::sort(mPoints.begin(), mPoints.end(), [](StFmsPoint* a, StFmsPoint* b) {
        return b->energy() < a->energy();
    });
}

void StFmsCollection::sortPointsByET(){
    std::sort(mPoints.begin(), mPoints.end(), [](StFmsPoint* a, StFmsPoint* b) {
        return b->fourMomentum().perp() < a->fourMomentum().perp();
    });
}

void StFmsCollection::print(int option) {
    cout << Form("NHit=%3d NCluster=%3d NPoint=%3d\n",numberOfHits(),numberOfClusters(),numberOfPoints());
    if(option>3) for(unsigned int i=0; i<numberOfHits(); i++)     {hits()[i]->print();}
    if(option>2) for(unsigned int i=0; i<numberOfClusters(); i++) {clusters()[i]->print();}
    if(option>1) for(unsigned int i=0; i<mFpsSlats.size(); i++)   {fpsSlats()[i]->print();}
    if(option>0) for(unsigned int i=0; i<numberOfPoints(); i++)   {points()[i]->print();}
}

