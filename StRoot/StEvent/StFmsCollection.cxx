/***************************************************************************
 *
 * $Id: StFmsCollection.cxx,v 2.1 2010/01/08 22:42:30 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsCollection.cxx,v $
 * Revision 2.1  2010/01/08 22:42:30  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFmsCollection.h"

static const char rcsid[] = "$Id: StFmsCollection.cxx,v 2.1 2010/01/08 22:42:30 ullrich Exp $";

ClassImp(StFmsCollection)

StFmsCollection::StFmsCollection() { /* no op */ }

StFmsCollection::~StFmsCollection() { /* no op */ }
    
unsigned int
StFmsCollection::numberOfHits() const { return mHits.size(); }

void
StFmsCollection::addHit(StFmsHit* hit){mHits.push_back(hit);}

StSPtrVecFmsHit&
StFmsCollection::hits() {return mHits;}

const StSPtrVecFmsHit&
StFmsCollection::hits() const {return mHits;}

//unsigned int StFmsCollection::nClusters() const { return mClusters.size(); }
//unsigned int StFmsCollection::nPoints() const { return mPoints.size(); }
//void StFmsCollection::addCluster(StFmsCluster* cluster){mClusters.push_back(cluster);}
//void StFmsCollection::addPoint(StFmsPoint* point){mPoints.push_back(point);}
//StSPtrVecFmsCluster& StFmsCollection::clusters() {return mClusters;}
//const StSPtrVecFmsCluster& StFmsCollection::clusters() const {return mClusters;}
//StSPtrVecFmsPoint& StFmsCollection::points() {return mPoints;}
//const StSPtrVecFmsPoint& StFmsCollection::points() const {return mPoints;}
