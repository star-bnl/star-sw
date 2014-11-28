/********************************************************************
 *
 * $Id: StPhmdClusterCollection.cxx,v 2.1 2002/12/20 22:33:00 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: StPhmdClusterCollection is base class for
 *              PMD cluster collection. 
 *
 ********************************************************************
 *
 * $Log: StPhmdClusterCollection.cxx,v $
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#include "StPhmdClusterCollection.h"
#include "StPhmdHit.h"

ClassImp(StPhmdClusterCollection)

StPhmdClusterCollection::StPhmdClusterCollection()
{ /* noop */ }

StPhmdClusterCollection::~StPhmdClusterCollection()
{ /* noop */ }

void 
StPhmdClusterCollection::deleteClusters()
{
    StSPtrVecPhmdClusterIterator iter;
    for (iter=mClusters.begin(); iter != mClusters.end(); iter++)
	mClusters.erase(iter);
}

void 
StPhmdClusterCollection::deleteCluster(StPhmdCluster* cluster)
{
    StSPtrVecPhmdClusterIterator iter;
    for (iter=mClusters.begin(); iter != mClusters.end(); iter++)
	if (*iter == cluster)  mClusters.erase(iter);
}

void StPhmdClusterCollection::addCluster(StPhmdCluster* cluster)
{
    mClusters.push_back(cluster);
}

int
StPhmdClusterCollection::numberOfclusters() const
{return mClusters.size();}

StSPtrVecPhmdCluster&
StPhmdClusterCollection::clusters()
{return mClusters;}

const StSPtrVecPhmdCluster&
StPhmdClusterCollection::clusters() const
{return mClusters;}
    
int
StPhmdClusterCollection::clusterFinderId() const
{return mClusterFinderId;}

int
StPhmdClusterCollection::clusterFinderParamVersion() const
{return mClusterFinderParamVersion;}

void
StPhmdClusterCollection::setClusterFinderId(int val)
{mClusterFinderId = val;}

void
StPhmdClusterCollection::setClusterFinderParamVersion(int val)
{mClusterFinderParamVersion = val;}
  

