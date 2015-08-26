/***************************************************************************
 *
 * $Id: StFmsCollection.cxx,v 2.3 2015/08/26 16:51:59 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsCollection.cxx,v $
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

static const char rcsid[] = "$Id: StFmsCollection.cxx,v 2.3 2015/08/26 16:51:59 ullrich Exp $";

StFmsCollection::StFmsCollection() { /* no op */ }

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

void StFmsCollection::print(Option_t *option) const {
    cout << Form("NHit=%3d NCluster=%3d NPoint=%3d\n",numberOfHits(),numberOfClusters(),numberOfPoints());
    for(unsigned int i=0; i<numberOfHits(); i++)    {hits()[i]->print();}
    for(unsigned int i=0; i<numberOfClusters(); i++){clusters()[i]->print();}
    for(unsigned int i=0; i<numberOfPoints(); i++)  {points()[i]->print();}
}

