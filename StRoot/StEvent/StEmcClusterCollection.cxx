/***************************************************************************
 *
 * $Id: StEmcClusterCollection.cxx,v 2.1 2000/02/23 17:34:02 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StEmcClusterCollection.cxx,v $
 * Revision 2.1  2000/02/23 17:34:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcClusterCollection.h"

ClassImp(StEmcClusterCollection);

static const char rcsid[] = "$Id: StEmcClusterCollection.cxx,v 2.1 2000/02/23 17:34:02 ullrich Exp $";

StEmcClusterCollection::StEmcClusterCollection() {/* noop*/};

StEmcClusterCollection::~StEmcClusterCollection() {/* noop*/};

StDetectorId
StEmcClusterCollection::detector() const  {return mDetector;}

void
StEmcClusterCollection::setDetector(StDetectorId var)  {mDetector = var;}

Int_t
StEmcClusterCollection::numberOfClusters() const{return mClusters.size();}

void
StEmcClusterCollection::addCluster(StEmcCluster* cluster)
{
  mClusters.push_back(cluster);
}

StSPtrVecEmcCluster&
StEmcClusterCollection::clusters() {return mClusters;}

const StSPtrVecEmcCluster&
StEmcClusterCollection::clusters() const {return mClusters;}

Int_t
StEmcClusterCollection::clusterFinderId() const  {return mClusterFinderId;}

Int_t
StEmcClusterCollection::clusterFinderParamVersion() const  {return mClusterFinderParamVersion;}

void
StEmcClusterCollection::setClusterFinderId(Int_t var) {mClusterFinderId = var;}

void
StEmcClusterCollection::setClusterFinderParamVersion(Int_t var) {mClusterFinderParamVersion = var;}
