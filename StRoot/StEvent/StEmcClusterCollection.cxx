/***************************************************************************
 *
 * $Id: StEmcClusterCollection.cxx,v 2.3 2001/04/05 04:00:47 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcClusterCollection.cxx,v $
 * Revision 2.3  2001/04/05 04:00:47  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/07/28 19:49:27  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.1  2000/02/23 17:34:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcClusterCollection.h"

ClassImp(StEmcClusterCollection);

static const char rcsid[] = "$Id: StEmcClusterCollection.cxx,v 2.3 2001/04/05 04:00:47 ullrich Exp $";

StEmcClusterCollection::StEmcClusterCollection() {/*noop*/};

StEmcClusterCollection::~StEmcClusterCollection() {/*noop*/};

StDetectorId
StEmcClusterCollection::detector() const  {return mDetector;}

void
StEmcClusterCollection::setDetector(StDetectorId var)  {mDetector = var;}

int
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

int
StEmcClusterCollection::clusterFinderId() const  {return mClusterFinderId;}

int
StEmcClusterCollection::clusterFinderParamVersion() const  {return mClusterFinderParamVersion;}

void
StEmcClusterCollection::setClusterFinderId(int var) {mClusterFinderId = var;}

void
StEmcClusterCollection::setClusterFinderParamVersion(int var) {mClusterFinderParamVersion = var;}
